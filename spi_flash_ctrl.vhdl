library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.wishbone_types.all;

entity spi_flash_ctrl is
    generic (
        -- Default config for auto-mode
        DEF_CLK_DIV     : natural  := 2;      -- Clock divider SCK = CLK/((CLK_DIV+1)*2)
        DEF_QUAD_READ   : boolean  := false;  -- Use quad read with 8 clk dummy

        -- Number of data lines (1=MISO/MOSI, otherwise 2 or 4)
        DATA_LINES      : positive := 1
        );
    port (
        clk : in std_ulogic;
        rst : in std_ulogic;

        -- Wishbone ports:
        wb_in  : in  wb_io_master_out;
        wb_out : out wb_io_slave_out;

        -- Wishbone extra selects
        wb_sel_reg : in std_ulogic; 
        wb_sel_map : in std_ulogic;

        -- SPI port
        sck     : out std_ulogic;
        cs_n    : out std_ulogic;
        sdat_o  : out std_ulogic_vector(DATA_LINES-1 downto 0);
        sdat_oe : out std_ulogic_vector(DATA_LINES-1 downto 0);
        sdat_i  : in  std_ulogic_vector(DATA_LINES-1 downto 0)
        );
end entity spi_flash_ctrl;

architecture rtl of spi_flash_ctrl is

    -- Register indices
    constant SPI_REG_BITS       : positive := 3;

    -- Register addresses (matches wishbone addr downto 2, ie, 4 bytes per reg)
    constant SPI_REG_DATA         : std_ulogic_vector(SPI_REG_BITS-1 downto 0) := "000";
    constant SPI_REG_CTRL         : std_ulogic_vector(SPI_REG_BITS-1 downto 0) := "001";
    constant SPI_REG_AUTO_CFG     : std_ulogic_vector(SPI_REG_BITS-1 downto 0) := "010";

    -- Bits in CTRL register
    constant SPI_CTRL_RESET_BIT  : integer := 0; -- Reset & drop CS
    constant SPI_CTRL_CS_BIT     : integer := 1; -- CSn value. Also acts as
                                                     -- manual mode enable
    -- Auto mode config register
    signal auto_cfg_reg     : std_ulogic_vector(23 downto 0) := (others => '0');
    alias  auto_cfg_cmd     : std_ulogic_vector(7 downto 0) is auto_cfg_reg(7 downto 0);
    alias  auto_cfg_dummies : std_ulogic_vector(2 downto 0) is auto_cfg_reg(10 downto 8);
    alias  auto_cfg_mode    : std_ulogic_vector(1 downto 0) is auto_cfg_reg(12 downto 11);
    alias  auto_cfg_addr4   : std_ulogic                    is auto_cfg_reg(13);
    alias  auto_cfg_rsrv1   : std_ulogic                    is auto_cfg_reg(14);
    alias  auto_cfg_rsrv2   : std_ulogic                    is auto_cfg_reg(15);
    alias  auto_cfg_div     : std_ulogic_vector(7 downto 0) is auto_cfg_reg(23 downto 16);

    -- Constants below match top 2 bits of rxtx "mode"
    constant SPI_AUTO_CFG_MODE_SINGLE : std_ulogic_vector(1 downto 0) := "00";
    constant SPI_AUTO_CFG_MODE_DUAL   : std_ulogic_vector(1 downto 0) := "10";
    constant SPI_AUTO_CFG_MODE_QUAD   : std_ulogic_vector(1 downto 0) := "11";

    -- Control register
    signal ctrl_reg    : std_ulogic_vector(15 downto 0) := (others => '0');
    alias  ctrl_reset  : std_ulogic is ctrl_reg(0);
    alias  ctrl_cs     : std_ulogic is ctrl_reg(1);
    alias  ctrl_rsrv1  : std_ulogic is ctrl_reg(2);
    alias  ctrl_rsrv2  : std_ulogic is ctrl_reg(3);
    alias  ctrl_div    : std_ulogic_vector(7 downto 0) is ctrl_reg(15 downto 8);

    -- Internals
    signal cmd_valid   : std_ulogic;
    signal cmd_clk_div : natural range 0 to 255;
    signal cmd_mode    : std_ulogic_vector(2 downto 0);
    signal cmd_ack     : std_ulogic;
    signal d_clks      : std_ulogic_vector(2 downto 0);
    signal d_rx        : std_ulogic_vector(7 downto 0);
    signal d_tx        : std_ulogic_vector(7 downto 0);
    signal d_ack       : std_ulogic;

    signal wb_valid     : std_ulogic;
    signal wb_reg_valid : std_ulogic;
    signal wb_map_valid : std_ulogic;
    signal wb_reg       : std_ulogic_vector(SPI_REG_BITS-1 downto 0);

    -- Auto mode clock counts XXX FIXME
    constant CS_DELAY_ASSERT   : integer := 1;  -- CS low to cmd
    constant CS_DELAY_DEASSERT : integer := 1;  -- last byte to CS high
    constant CS_DELAY_RECOVERY : integer := 10; -- CS high to CS low

    -- Automatic mode state
    type auto_state_t is (AUTO_IDLE, AUTO_CS_ON, AUTO_CMD,
                          AUTO_ADR0, AUTO_ADR1, AUTO_ADR2, AUTO_ADR3,
                          AUTO_DUMMY,
                          AUTO_DAT0, AUTO_DAT1, AUTO_DAT2, AUTO_DAT3,
                          AUTO_CS_OFF, AUTO_RECOVERY);
    -- Automatic mode signals
    signal auto_cs        : std_ulogic;
    signal auto_cmd_valid : std_ulogic;
    signal auto_cmd_mode  : std_ulogic_vector(2 downto 0);
    signal auto_d_txd     : std_ulogic_vector(7 downto 0);
    signal auto_d_clks    : std_ulogic_vector(2 downto 0);
    signal auto_data_next : std_ulogic_vector(wb_out.dat'left downto 0);
    signal auto_cnt_next  : integer range 0 to 63;
    signal auto_ack       : std_ulogic;
    signal auto_next      : auto_state_t;

    -- Automatic mode latches
    signal auto_data      : std_ulogic_vector(wb_out.dat'left downto 0);
    signal auto_cnt       : integer range 0 to 63;
    signal auto_state     : auto_state_t;
begin

    spi_rxtx: entity work.spi_rxtx
        generic map (
            DATA_LINES => DATA_LINES
            )
        port map(
            rst => rst,
            clk => clk,
            clk_div => cmd_clk_div,
            cmd_valid => cmd_valid,
            cmd_mode => cmd_mode,
            cmd_ack => cmd_ack,
            d_clks => d_clks,
            d_rx => d_rx,
            d_tx => d_tx,
            d_ack => d_ack,
            sck => sck,
            sdat_o => sdat_o,
            sdat_oe => sdat_oe,
            sdat_i => sdat_i
            );
    
    -- Valid wb command
    wb_valid     <= wb_in.stb and wb_in.cyc;
    wb_reg_valid <= wb_valid and wb_sel_reg;
    wb_map_valid <= wb_valid and wb_sel_map;

    -- Register decode. For map accesses, make it look like "data"
    wb_reg       <= wb_in.adr(SPI_REG_BITS+1 downto 2) when wb_reg_valid else SPI_REG_DATA;

    -- Big mode mux for SPI control
    mode_mux: process(all)
    begin
        if ctrl_cs = '1' then
            -- Note: The "and not d_ack" below makes cmd_valid go down a cycle
            -- earlier. This is necessary for CLK_DIV=0 (SCLK=CLK/2) otherwise
            -- we will latch a spurrious new command on the next cycle.
            --
            cmd_valid  <= wb_reg_valid and not d_ack when wb_reg = SPI_REG_DATA else '0';

            -- Clock divider from control reg
            cmd_clk_div <= to_integer(unsigned(ctrl_div));

            -- Mode based on sel: LSB is single mode, next is dual, next is quad
            if wb_in.sel = "0010" then
                cmd_mode <= "10" & wb_in.we;
                d_clks   <= "011";
            elsif wb_in.sel = "0100" then
                cmd_mode <= "11" & wb_in.we;
                d_clks   <= "001";
            else
                cmd_mode <= "00" & wb_in.we;
                d_clks   <= "111";
            end if;
            d_tx       <= wb_in.dat(7 downto 0);
            cs_n       <= not ctrl_cs;
        else
            -- See above note about "and not d_ack"
            cmd_valid   <= auto_cmd_valid and not d_ack;
            cmd_mode    <= auto_cmd_mode;
            cmd_clk_div <= to_integer(unsigned(auto_cfg_div));
            d_tx        <= auto_d_txd;
            d_clks      <= auto_d_clks;
            cs_n        <= not auto_cs;
        end if;
    end process;
    
    -- Generate wishbone responses
    wb_response: process(all)
    begin
        wb_out.dat <= (others => '0');
        wb_out.ack <= '0';
        wb_out.stall <= '0';

        -- Data vs register
        if wb_map_valid = '1' or (ctrl_cs = '1' and wb_reg = SPI_REG_DATA) then
            -- Data accesses, either manual or auto mode
            if ctrl_cs = '1' then
                wb_out.ack <= d_ack when cmd_valid = '1' else wb_valid;
                wb_out.dat <= x"00" & d_rx & d_rx & d_rx;
            else
                wb_out.ack <= auto_ack;
                wb_out.dat <= auto_data;
            end if;
        else
            -- Normal register access
            --
            -- Normally single cycle but ensure any auto-mode operation is
            -- complete first
            if auto_state = AUTO_IDLE then
                wb_out.ack <= wb_valid;
                case wb_reg is
                when SPI_REG_CTRL =>
                    wb_out.dat <= (SPI_CTRL_RESET_BIT  => ctrl_reset,
                                   SPI_CTRL_CS_BIT     => ctrl_cs,
                                   others => '0');
                when others => null;
                end case;
            end if;
        end if;

        -- No pipelining
        wb_out.stall <= '0' when wb_in.cyc = '0' else not wb_out.ack;
    end process;

    -- Automatic mode state machine
    auto_sync: process(clk)
    begin
        if rising_edge(clk) then
            auto_state <= auto_next;
            auto_cnt   <= auto_cnt_next;
            auto_data  <= auto_data_next;
        end if;
    end process;

    auto_comb: process(all)
        variable auto_addr : std_ulogic_vector(31 downto 0);

        function mode_to_clks(mode: std_ulogic_vector(1 downto 0)) return std_ulogic_vector is
        begin
            if mode = SPI_AUTO_CFG_MODE_QUAD then
                return "001";
            elsif mode = SPI_AUTO_CFG_MODE_DUAL then
                return "011";
            else
                return "111";
            end if;
        end function;
   begin
        -- Default outputs
        auto_ack <= '0';
        auto_cs <= '0';
        auto_cmd_valid <= '0';
        auto_d_txd <= x"00";
        auto_cmd_mode <= "000";
        auto_d_clks <= "111";

        -- Default next state
        auto_next <= auto_state;
        auto_cnt_next <= auto_cnt;
        auto_data_next <= auto_data;

        -- Convert wishbone address into a flash address
        -- For now assume 3-bytes addresses. We can add
        -- larger flash support later.
        auto_addr := "0000" & wb_in.adr(27 downto 2) & "00";

        -- XXX TODO:
        --
        --  - Support < 32-bit accesses
        --  - Capture previous address and delay releasing CS to be able
        --    to "chain" accesses to consecutive addresses without a new
        --    address cycle
                     
        -- Reset        
        if rst = '1' or ctrl_reset = '1' then
            auto_cs <= '0';
            auto_cnt_next <= 0;
            auto_next <= AUTO_IDLE;
        else
            -- Run counter
            if auto_cnt /= 0 then
                auto_cnt_next <= auto_cnt - 1;
            end if;

            -- Automatic CS is set whenever state isn't IDLE or RECOVERY
            if auto_state /= AUTO_IDLE and
               auto_state /= AUTO_RECOVERY then
                auto_cs <= '1';
            end if;

            -- State machine
            case auto_state is
            when AUTO_IDLE =>
                -- Access to the memory map only when manual CS isn't set
                if wb_map_valid = '1' and ctrl_cs = '0' then
                    -- Ignore writes, we don't support them yet
                    if wb_in.we = '1' then
                        auto_ack <= '1';
                    else
                        -- Start machine with CS assertion delay
                        auto_next <= AUTO_CS_ON;
                        auto_cnt_next <= CS_DELAY_ASSERT;
                    end if;
                end if;
            when AUTO_CS_ON =>
                if auto_cnt = 0 then
                    -- CS asserted long enough, send command
                    auto_next <= AUTO_CMD;
                end if;
            when AUTO_CMD =>
                auto_d_txd <= auto_cfg_cmd;
                auto_cmd_valid <= '1';
                if d_ack = '1' then
                    if auto_cfg_addr4 = '1' then
                        auto_next <= AUTO_ADR3;
                    else
                        auto_next <= AUTO_ADR2;
                    end if;
                end if;
            when AUTO_ADR3 =>
                auto_d_txd <= "00" & auto_addr(29 downto 24);
                auto_cmd_valid <= '1';
                if d_ack = '1' then
                    auto_next <= AUTO_ADR2;
                end if;
            when AUTO_ADR2 =>
                auto_d_txd <= auto_addr(23 downto 16);
                auto_cmd_valid <= '1';
                if d_ack = '1' then
                    auto_next <= AUTO_ADR1;
                end if;
            when AUTO_ADR1 =>
                auto_d_txd <= auto_addr(15 downto 8);
                auto_cmd_valid <= '1';
                if d_ack = '1' then
                    auto_next <= AUTO_ADR0;
                end if;
            when AUTO_ADR0 =>
                auto_d_txd <= auto_addr(7 downto 0);
                auto_cmd_valid <= '1';
                if d_ack = '1' then
                    if auto_cfg_dummies = "000" then
                        auto_next <= AUTO_DAT0;
                    else
                        auto_next <= AUTO_DUMMY;
                    end if;
                end if;
            when AUTO_DUMMY =>
                auto_cmd_valid <= '1';
                auto_d_clks <= auto_cfg_dummies;
                if d_ack = '1' then
                    auto_next <= AUTO_DAT0;
                end if;                
            when AUTO_DAT0 =>
                auto_cmd_valid <= '1';
                auto_cmd_mode <= auto_cfg_mode & "0";
                auto_d_clks <= mode_to_clks(auto_cfg_mode);
                if d_ack = '1' then 
                    auto_data_next(7 downto 0) <= d_rx;
                    auto_next <= AUTO_DAT1;
                end if;
            when AUTO_DAT1 =>
                auto_cmd_valid <= '1';
                auto_cmd_mode <= auto_cfg_mode & "0";
                auto_d_clks <= mode_to_clks(auto_cfg_mode);
                if d_ack = '1' then
                    auto_data_next(15 downto 8) <= d_rx;
                    auto_next <= AUTO_DAT2;
                end if;
            when AUTO_DAT2 =>
                auto_cmd_valid <= '1';
                auto_cmd_mode <= auto_cfg_mode & "0";
                auto_d_clks <= mode_to_clks(auto_cfg_mode);
                if d_ack = '1' then
                    auto_data_next(23 downto 16) <= d_rx;
                    auto_next <= AUTO_DAT3;
                end if;
            when AUTO_DAT3 =>
                auto_cmd_valid <= '1';
                auto_cmd_mode <= auto_cfg_mode & "0";
                auto_d_clks <= mode_to_clks(auto_cfg_mode);
                if d_ack = '1' then
                    auto_data_next(31 downto 24) <= d_rx;
                    auto_next <= AUTO_CS_OFF;
                    auto_cnt_next <= CS_DELAY_ASSERT;
                end if;
            when AUTO_CS_OFF =>
                if auto_cnt = 0 then
                    auto_ack <= '1';
                    auto_cnt_next <= CS_DELAY_RECOVERY;
                    auto_next <= AUTO_RECOVERY;
                end if;
            when AUTO_RECOVERY =>
                if auto_cnt = 0 then
                    auto_next <= AUTO_IDLE;
                end if;
            end case;
        end if;
    end process;

    -- Register write sync machine
    reg_write: process(clk)
    begin
        if rising_edge(clk) then
            -- Reset auto-clear
            if rst = '1' or ctrl_reset = '1' then
                ctrl_reset       <= '0';
                ctrl_cs          <= '0';
                ctrl_rsrv1       <= '0';
                ctrl_rsrv2       <= '0';
                ctrl_div         <= std_ulogic_vector(to_unsigned(DEF_CLK_DIV, 8));
                if DEF_QUAD_READ then
                    auto_cfg_cmd     <= x"6b";
                    auto_cfg_dummies <= "111";
                    auto_cfg_mode    <= SPI_AUTO_CFG_MODE_QUAD;
                else
                    auto_cfg_cmd     <= x"03";
                    auto_cfg_dummies <= "000";
                    auto_cfg_mode    <= SPI_AUTO_CFG_MODE_SINGLE;
                end if;
                auto_cfg_addr4   <= '0';
                auto_cfg_rsrv1   <= '0';
                auto_cfg_rsrv2   <= '0';
                auto_cfg_div     <= std_ulogic_vector(to_unsigned(DEF_CLK_DIV, 8));
            end if;

            if wb_reg_valid = '1' and wb_in.we = '1' and auto_state = AUTO_IDLE then
                if wb_reg = SPI_REG_CTRL then
                    ctrl_reg     <= wb_in.dat(ctrl_reg'range);
                end if;
                if wb_reg = SPI_REG_AUTO_CFG then
                    auto_cfg_reg <= wb_in.dat(auto_cfg_reg'range);
                end if;
            end if;
        end if;
    end process;
 
end architecture;
