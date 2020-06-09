library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.wishbone_types.all;

entity spi_rxtx is
    generic (
        CLK_DIV       : positive := 2;      -- Clock divider (0..255) XXX Make
                                            -- runtime config ?
        DATA_LINES    : positive := 1       -- Number of data lines
                                            -- (1=MISO/MOSI, otherwise 2 or 4)
        );
    port (
        clk : in std_ulogic;
        rst : in std_ulogic;

        -- Command port
        --
        -- Command modes:
        --  0xx : Normal MISO/MOSI command
        --  100 : Dual read
        --  101 : Dual write
        --  110 : Quad read
        --  111 : Quad write
        cmd_valid   : in std_ulogic;
        cmd_mode    : in std_ulogic_vector(2 downto 0);
        cmd_ack     : out std_ulogic;

        -- # clocks for command -1 (0=>1 clk, 7=>8 clks)
        d_clks      : in std_ulogic_vector(2 downto 0);

        -- Data
        d_tx        : in std_ulogic_vector(7 downto 0);
        d_rx        : out std_ulogic_vector(7 downto 0);
        d_ack       : out std_ulogic;

        -- SPI port. These might need to go into special IOBUFs or STARTUPE2 on
        -- Xilinx.
        --
        -- Data lines are organized as follow:
        --
        -- DATA_LINES = 1
        --
        --   sdat_o(0) is MOSI (master output slave input)
        --   sdat_i(0) is MISO (master input slave output)
        --
        -- DATA_LINES > 1
        --
        --   sdat_o(0..n) are DQ(0..n)
        --   sdat_i(0..n) are DQ(0..n)
        --
        --   as such, beware that:
        --
        --   sdat_o(0) is MOSI (master output slave input)
        --   sdat_i(1) is MISO (master input slave output)
        --
        -- In order to leave dealing with the details of how to wire the tristate
        -- and bidirectional pins to the system specific toplevel, we separate
        -- the input and output signals, and provide a "sdat_oe" signal which
        -- is the "output enable" of each line.
        --
        sck     : out std_ulogic;
        sdat_o  : out std_ulogic_vector(DATA_LINES-1 downto 0);
        sdat_oe : out std_ulogic_vector(DATA_LINES-1 downto 0);
        sdat_i  : in  std_ulogic_vector(DATA_LINES-1 downto 0)
        );
end entity spi_rxtx;

architecture rtl of spi_rxtx is

    -- Internal clock signal. Output is gated by sck_en_int
    signal sck_0    : std_ulogic;
    signal sck_1    : std_ulogic;

    -- 1 clk pulses indicating when sck goes up or down
    signal sck_up     : std_ulogic;
    signal sck_down   : std_ulogic;

    -- Output shift register (use fifo ?)
    signal oreg       : std_ulogic_vector(7 downto 0);

    -- Input shift register (use fifo ?)
    signal ireg       : std_ulogic_vector(7 downto 0);

    -- Bit counter
    signal bit_count  : std_ulogic_vector(2 downto 0);

    -- Start command signal. Set when counter goes negative
    signal start_cmd : std_ulogic;

    -- Data mode latch from cmd_mode
    signal data_mode : std_ulogic_vector(2 downto 0);

    function data_single(mode : std_ulogic_vector(2 downto 0)) return boolean is
    begin
        return mode(2) = '0';
    end;
    function data_dual(mode : std_ulogic_vector(2 downto 0)) return boolean is
    begin
        return mode(2 downto 1) = "10";
    end;
    function data_quad(mode : std_ulogic_vector(2 downto 0)) return boolean is
    begin
        return mode(2 downto 1) = "11";
    end;
    function data_write(mode : std_ulogic_vector(2 downto 0)) return boolean is
    begin
        return mode(0) = '1';
    end;

    type state_t is (STANDBY, DATA);
    signal state : state_t := STANDBY;
begin

    -- We don't support multiple data lines at this point
    assert DATA_LINES = 1 or DATA_LINES = 2 or DATA_LINES = 4
        report "Unsupported DATA_LINES configuration !" severity failure;

    -- Clock generation
    sck_gen: process(clk)
        variable counter : integer range 0 to CLK_DIV;
    begin
        if rising_edge(clk) then
            if rst = '1' then
                sck_0 <= '1';
                sck_1 <= '1';
                sck_up <= '0';
                sck_down <= '0';
            elsif counter = CLK_DIV then
                counter := 0;

                -- Internal version of the clock
                sck_0 <= not sck_0;

                -- Generate up/down pulses to run out state machine
                -- Note: This limits sck to half of clk.
                sck_up <= not sck_0;
                sck_down <= sck_0;
            else
                counter := counter + 1;
                sck_up <= '0';
                sck_down <= '0';
            end if;

            -- Delayed version of the clock to line up with
            -- the up/down signals
            sck_1 <= sck_0;
        end if;
    end process;

    -- Gated clock
    sck <= sck_1 when state = DATA else '1';

    -- We can start a new command. This is set on the clock down
    -- after the counter goes negative.
    -- Note: in addition to latching a new command, this will cause
    -- the counter to be reloaded.
    start_cmd <= '1' when sck_down  = '1' and bit_count = "111" else '0';

    -- Main state machine. Also generates cmd and data ACKs
    machine: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                state <= STANDBY;
                cmd_ack <= '0';
                d_ack <= '0';
            else
                -- Ack is single cycle pulse
                d_ack <= '0';
                cmd_ack <= '0';

                -- Requests complete on the rising edge of sck when the
                -- bit counter has reched 0 and is about to go negative.
                -- The read shift register will latch the last input bit
                -- at the same time.
                -- XXX Dbl check if we need to check state = DATA here
                if sck_up = '1' and bit_count = "000" then
                    d_ack  <= '1';
                end if;

                -- First clk down of a new cycle. Latch a request if any
                -- or get out.
                if start_cmd = '1' then
                    if cmd_valid = '1' then
                        -- XXX validate cmd_mode vs. DATA_LINES
                        data_mode <= cmd_mode;
                        cmd_ack <= '1';
                        state <= DATA;
                    else
                        data_mode <= "000";
                        state <= STANDBY;
                    end if;
                end if;
            end if;
        end if;
    end process; 

    -- Run the bit counter in DATA state. It will update on rising
    -- SCK edges. It starts at d_clks on command latch
    count_bit: process(clk)
    begin
        if rising_edge(clk) then
            if state /= DATA then
                bit_count <= (others => '1');
            elsif start_cmd = '1' then                
                bit_count <= d_clks;
            elsif sck_up = '1' then
                bit_count <= std_ulogic_vector(unsigned(bit_count) - 1);
            end if;
        end if;
    end process;

    -- Shift output data
    shift_out: process(clk)
    begin
        if rising_edge(clk) then
            -- Starting a command
            if start_cmd = '1' then
                -- Do we have data to send ? Otherwise shift out zeros
                if cmd_valid = '1' then
                    oreg <= d_tx(7 downto 0);
                else
                    oreg <= (others => '0');
                end if;
            elsif sck_down = '1' then
                -- Get shift amount
                if data_single(data_mode) then
                    oreg <= oreg(6 downto 0) & '0';
                elsif data_dual(data_mode) then
                    oreg <= oreg(5 downto 0) & "00";
                else
                    oreg <= oreg(3 downto 0) & "0000";
                end if;
            end if;                
        end if;
    end process;

    -- Data out
    sdat_o(0) <= oreg(7);
    dl2: if DATA_LINES > 1 generate
        sdat_o(1) <= oreg(6);
    end generate;
    dl4: if DATA_LINES > 2 generate
        sdat_o(2) <= oreg(5);
        sdat_o(3) <= oreg(4);
    end generate;

    -- Data lines direction
    dlines: process(all)
    begin
        for i in DATA_LINES-1 downto 0 loop
            sdat_oe(i) <= '0';
            if state = DATA then
                -- In single mode, we always enable MOSI, otherwise
                -- we control the output enable based on the direction
                -- of transfer.
                --
                if i = 0 and (data_single(data_mode) or data_write(data_mode)) then
                    sdat_oe(i) <= '1';
                end if;
                if i = 1 and data_dual(data_mode) and data_write(data_mode) then
                    sdat_oe(i) <= '1';
                end if;
                if i > 0 and data_quad(data_mode) and data_write(data_mode) then
                    sdat_oe(i) <= '1';
                end if;
            end if;
        end loop;
    end process;

    -- Shift input data
    shift_in: process(clk)
    begin
        if rising_edge(clk) then
            if state = DATA and sck_up = '1' then
                if DATA_LINES = 1 then
                    ireg <= ireg(6 downto 0) & sdat_i(0);
                else
                    if data_dual(data_mode) then
                        ireg <= ireg(5 downto 0) & sdat_i(1) & sdat_i(0);
                    elsif data_quad(data_mode) then
                        ireg <= ireg(3 downto 0) & sdat_i(3) & sdat_i(2) & sdat_i(1) & sdat_i(0);
                    else
                        assert(data_single(data_mode));
                        ireg <= ireg(6 downto 0) & sdat_i(1);
                    end if;
                end if;
            end if;
        end if;
    end process;

    -- Data recieve register
    d_rx <= ireg;

end architecture;
