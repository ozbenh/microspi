library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.wishbone_types.all;

entity spi_simple_ctrl is
    generic (
        CLK_DIV       : positive := 2;      -- Clock divider (SCK = CLK/(CLK_DIV+1))
        CS_LINES      : positive := 1
        );
    port (
        clk : in std_ulogic;
        rst : in std_ulogic;

        -- Wishbone ports:
        wb_in : in wb_io_master_out;
        wb_out : out wb_io_slave_out;
        
        -- SPI port
        sck   : out std_ulogic;
        cs_n  : out std_ulogic_vector(CS_LINES-1 downto 0);
        mosi  : out std_ulogic;
        miso  : in  std_ulogic
        );
end entity spi_simple_ctrl;

architecture rtl of spi_simple_ctrl is

    -- Register indices
    constant SPI_REG_BITS       : positive := 4;

    -- Register addresses (matches wishbone addr downto 2, ie, 4 bytes per reg)
    constant SPI_REG_RX_SHIFT  : std_ulogic_vector(SPI_REG_BITS-1 downto 0) := "0000";
    constant SPI_REG_RXTX_BUF  : std_ulogic_vector(SPI_REG_BITS-1 downto 0) := "0001";
    constant SPI_REG_STATUS    : std_ulogic_vector(SPI_REG_BITS-1 downto 0) := "0010";
    constant SPI_REG_CTRL      : std_ulogic_vector(SPI_REG_BITS-1 downto 0) := "0011";
    constant SPI_REG_BAUD      : std_ulogic_vector(SPI_REG_BITS-1 downto 0) := "0100";
    constant SPI_REG_CS        : std_ulogic_vector(SPI_REG_BITS-1 downto 0) := "1000";

    -- Bits in status register
    constant SPI_REG_STAT_TX_END_BIT    : integer := 0;
    constant SPI_REG_STAT_TX_RDY_BIT    : integer := 1;

    -- Bits in CS register
    constant SPI_REG_CS0_BIT    : integer := 0; -- CSn value

    -- SPI signals
    signal sdat_o  : std_ulogic_vector(0 downto 0);
    signal sdat_oe : std_ulogic_vector(0 downto 0);
    signal sdat_i  : std_ulogic_vector(0 downto 0);
    
     -- Control register
    signal cs_reg      : std_ulogic_vector(CS_LINES-1 downto 0);

    -- Internals
    signal cmd_valid   : std_ulogic;
    signal cmd_ack     : std_ulogic;
    signal d_clks      : std_ulogic_vector(2 downto 0);
    signal d_tx        : std_ulogic_vector(7 downto 0);
    signal d_rx        : std_ulogic_vector(7 downto 0);
    signal d_ack       : std_ulogic;

    -- OpenCore Tiny-SPI style interface 
    signal d_buf       : std_ulogic_vector(7 downto 0);
    signal tx_end      : std_ulogic;
    signal tx_rdy      : std_ulogic;

    -- Wishbone decode
    signal wb_valid    : std_ulogic;
    signal wb_write    : std_ulogic;
    signal wb_reg      : std_ulogic_vector(SPI_REG_BITS-1 downto 0);

begin

    assert CS_LINES <= 8  report "CS_LINES out of bounds (1..8)" severity failure;
    assert CLK_DIV <= 255 report "CLK_DIV out of bounds (1..255)" severity failure;

    spi_rxtx: entity work.spi_rxtx
        generic map (
            DATA_LINES => 1
            )
        port map(
            rst => rst,
            clk => clk,
            cmd_valid => cmd_valid,
            cmd_mode => "000",
            cmd_ack => cmd_ack,
            d_clks => d_clks,
            d_tx => d_tx,
            d_rx => d_rx,
            d_ack => d_ack,
            sck => sck,
            sdat_o => sdat_o,
            sdat_oe => sdat_oe,
            sdat_i => sdat_i
            );

    -- SPI signals
    cs_n <= not cs_reg;
    mosi <= sdat_o(0);
    sdat_i(0) <= miso;

    -- Wishbone decode
    wb_valid     <= wb_in.stb and wb_in.cyc;
    wb_write     <= wb_valid and wb_in.we;
    wb_reg       <= wb_in.adr(SPI_REG_BITS+1 downto 2);

    -- Command is valid when there's something in the buffer register
    cmd_valid    <= not tx_rdy;
    d_tx         <= d_buf;
    d_clks       <= "111";

    -- Generate ack and stall
    --
    -- XXX Our rxtx will only latch cmd when it's in the right phase, which means
    -- it can be delayed. The Linux driver however doesn't check TXR when sending
    -- the second byte of a command as it assumes the HW always latches 2 bytes
    -- in a row at the beginning of a sequence.
    --
    -- We need to fix this, but in the meantime, hold the bus if a write to the
    -- buffer register is attempted while TRX is 0.
    --
    ack_stall: process(all)
    begin
        if wb_write = '1' and wb_reg = SPI_REG_RXTX_BUF then
            wb_out.ack <= tx_rdy;
            wb_out.stall <= not tx_rdy;
        else
            wb_out.ack <= wb_valid ;
            wb_out.stall <= '0';
        end if;
    end process;

    -- Geneate Wishbonne data out
    wb_dout: process(all)
    begin
        wb_out.dat <= (others => '0');
        case wb_reg is
        when SPI_REG_RX_SHIFT =>
            wb_out.dat(7 downto 0) <= d_rx;
        when SPI_REG_RXTX_BUF =>
            wb_out.dat(7 downto 0) <= d_buf;
        when SPI_REG_STATUS =>
            wb_out.dat(0) <= tx_end;
            wb_out.dat(1) <= tx_rdy;
        when SPI_REG_CS =>
            wb_out.dat(cs_reg'left downto 0) <= cs_reg;
        when SPI_REG_CTRL =>            -- XXX TODO
        when SPI_REG_BAUD =>            -- XXX TODO
        when others =>
        end case;
    end process;

    -- Data buffer writes. This manages d_buf, tx_end and tx_rdy in order
    -- to match the operations of OpenCores Tiny SPI.
    dbuf_write: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                -- At reset, TX is ended, and buffer is ready to receive
                -- a new byte to send
                tx_end <= '1';
                tx_rdy <= '1';            
            elsif wb_write = '1' and wb_reg = SPI_REG_RXTX_BUF and tx_rdy = '1' then
                -- We fill the buffer and clear TXR to indicate that we
                -- have a byte latched
                tx_rdy <= '0';
                tx_end <= '0';
                d_buf  <= wb_in.dat(7 downto 0);
            elsif cmd_ack = '1' then
                -- The byte was consumed by the shifter, populate the data
                -- buffer with the previous input byte and mark us ready
                -- to receive a new byte to send.
                d_buf  <= d_rx;
                tx_rdy <= '1';
                tx_end <= '0';
            elsif d_ack = '1' then
                -- The data cycle is complete. If we don't have next data
                -- latched already, then mark this as end of transfer.
                tx_end <= tx_rdy;
            end if;
        end if;
    end process;

    -- Misc Registers write sync machine
    reg_write: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                cs_reg <= (others => '0');
            else
                if wb_write = '1' then
                    if wb_reg = SPI_REG_CS then
                        cs_reg <= wb_in.dat(cs_reg'left downto 0);
                    end if;
                end if;
            end if;
        end if;
    end process;
 
end architecture;
