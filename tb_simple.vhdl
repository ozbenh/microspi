library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.wishbone_types.all;

library model;
use model.all;

entity tb_simple is
end tb_simple;

architecture behave of tb_simple is
    signal clk : std_ulogic;
    signal rst : std_ulogic := '1';

    -- testbench signals
    constant clk_period : time := 10 ns;

    subtype byte_t is std_ulogic_vector(7 downto 0);
    type buf_t is array (natural range <>) of byte_t;
    constant null_buf : buf_t(-1 downto 0) := (others => (others => '0'));

    signal wb_out : wb_io_master_out;
    signal wb_in  : wb_io_slave_out;

    signal sck     : std_ulogic;
    signal cs_n    : std_ulogic_vector(0 downto 0);    
    signal rdat    : std_ulogic_vector(7 downto 0);
    signal hold_n  : std_logic := 'Z';
    signal wp_n    : std_ulogic := '1';
    signal mosi    : std_logic;
    signal miso    : std_logic;

begin

    spi: entity work.spi_simple_ctrl
        port map(
            rst => rst,
            clk => clk,
            wb_in => wb_out,
            wb_out => wb_in,
            sck => sck,
            cs_n => cs_n,
            mosi => mosi,
            miso => miso
            );


    flash: entity model.s25fl128s
        generic map (
            TimingModel => "S25FL128SAGNFI000_R_30pF",
            LongTimming => false
            )
        port map(
            SCK => sck,
            SI => mosi,
            CSNeg => cs_n(0),
            HOLDNeg => hold_n,
            WPNeg => wp_n,
            RSTNeg => '1',
            SO => miso
            );

    clk_process: process
    begin
        clk <= '0';
        wait for clk_period/2;
        clk <= '1';
        wait for clk_period/2;
    end process;

    test : process
        procedure wb_cycle is
        begin
            wb_out.cyc <= '1';
            wb_out.stb <= '1';
            loop
            wait until rising_edge(clk);
                if wb_in.stall = '0' then
                    wb_out.stb <= '0';
                end if;
                if wb_in.ack = '1' then
                    wb_out.cyc <= '0';
                    rdat <= wb_in.dat(7 downto 0);
                    -- Need a wait for rdat to be visible
                    wait until falling_edge(clk);
                    exit;
                end if;
            end loop;
        end procedure;

        procedure wb_write(adr : in std_ulogic_vector(7 downto 0);
                           dat : in byte_t) is
        begin
            wb_out.adr(7 downto 0) <= adr;
            wb_out.dat <= x"000000" & dat;
            wb_out.we  <= '1';
            wb_cycle;
        end procedure;

        procedure wb_read(adr : in std_ulogic_vector(7 downto 0);
                          dat : out byte_t) is 
        begin
            wb_out.adr(7 downto 0) <= adr;
            wb_out.we  <= '0';
            wb_cycle;
            dat := rdat;
        end procedure;

        procedure cs_set is
        begin
            wb_write(x"20", x"01");
        end procedure;

        procedure cs_clr is
        begin
            wb_write(x"20", x"00");
        end procedure;

        procedure tx_write(cmd : in byte_t) is
        begin
            wb_write(x"04", cmd);
        end procedure;

        procedure tx_read(dat : out byte_t) is
        begin
            wb_read(x"04", dat);
        end procedure;

        procedure rx_read(dat : out byte_t) is
        begin
            wb_read(x"00", dat);
        end procedure;

        procedure st_read(dat : out byte_t) is
        begin
            wb_read(x"08", dat);
        end procedure;

        procedure wait_txr is
            variable dat : std_ulogic_vector(7 downto 0);
        begin
            loop
                st_read(dat);
                if dat(1) = '1' then
                   exit;
                end if;
            end loop;
        end procedure;

        procedure wait_txe is
            variable dat : std_ulogic_vector(7 downto 0);
        begin
            loop
                st_read(dat);
                if dat(0) = '1' then
                   exit;
                end if;
            end loop;
        end procedure;

        procedure tx_next(tx_buf : buf_t; idx : inout natural; dat : out byte_t) is
        begin
            if idx < tx_buf'length then
                dat := tx_buf(idx);
            else
                dat := x"00";
            end if;
            idx := idx + 1;
        end procedure;

        procedure txrx (tx_buf : in buf_t; rx_buf : inout buf_t; len : positive) is
            variable ti  : natural := 0;
            variable ri  : natural := 0;
            variable dat : byte_t;
        begin
            assert(rx_buf'length = 0 or rx_buf'length >= len) severity failure;

            tx_next(tx_buf, ti, dat);
            tx_write(dat);
            for i in 1 to len-1 loop
                tx_next(tx_buf, ti, dat);
                tx_write(dat);
                if rx_buf'length /= 0 or i /= (len - 1) then
                    wait_txr;
                end if;
                if rx_buf'length /= 0 then
                    tx_read(rx_buf(ri));
                    ri := ri + 1;
                end if;
            end loop;
            wait_txe;
            if rx_buf'length /= 0 then
                rx_read(rx_buf(ri));
            end if;
        end procedure;

        procedure clr_bufs(tx_buf : inout buf_t; rx_buf : inout buf_t) is
        begin
            for i in tx_buf'range loop
                tx_buf(i) := x"00";
            end loop;
            for i in rx_buf'range loop
                rx_buf(i) := x"00";
            end loop;
        end procedure;

        procedure spi_flash_cmd(cmd: byte_t;
                                args: in buf_t; nargs: natural;
                                rets: out buf_t; nrets: natural) is
        begin
            tx_write(cmd);
            for i in 0 to nargs-1 loop
                tx_write(args(i));
                wait_txr;
            end loop;
            wait_txe;
            if nrets > 0 then
                tx_write(x"00");
                for i in 0 to nrets-2 loop
                    tx_write(x"00");
                    wait_txr;
                    tx_read(rets(i));
                end loop;
            end if;
            wait_txe;
            if nrets > 0 then
                rx_read(rets(nrets-1));
            end if;
        end procedure;

        
        variable dat : std_ulogic_vector(7 downto 0);
        variable tx_buf : buf_t(0 to 16);
        variable rx_buf : buf_t(0 to 16);
        
    begin
        rst <= '1';
        wb_out.stb <= '0';
        wb_out.cyc <= '0';
        wb_out.adr <= (others => '0');        
        -- Leave flash a chance...
        wait for 1 ms;
        rst <= '0';
        wait for 1 ms;
        wait until rising_edge(clk);

        -- Sending read ID command
        report "Sending RDID...";
        cs_set;
        spi_flash_cmd(x"9f", null_buf, 0, rx_buf, 5);
        report "ID0=" & to_hstring(rx_buf(0));
        assert rx_buf(0) = x"01" report "Unexpected result" severity failure;
        report "ID1=" & to_hstring(rx_buf(1));
        assert rx_buf(1) = x"20" report "Unexpected result" severity failure;
        report "ID2=" & to_hstring(rx_buf(2));
        assert rx_buf(2) = x"18" report "Unexpected result" severity failure;
        report "ID3=" & to_hstring(rx_buf(3));
        assert rx_buf(3) = x"00" report "Unexpected result" severity failure;
        report "ID4=" & to_hstring(rx_buf(4));
        assert rx_buf(4) = x"01" report "Unexpected result" severity failure;
        cs_clr;

        -- Sending read ID command
        report "Sending RDID again...";
        cs_set;
        clr_bufs(tx_buf, rx_buf);
        tx_buf(0) := x"9f";
        txrx(tx_buf, rx_buf, 6);
        report "ID0=" & to_hstring(rx_buf(1));
        assert rx_buf(1) = x"01" report "Unexpected result" severity failure;
        report "ID1=" & to_hstring(rx_buf(2));
        assert rx_buf(2) = x"20" report "Unexpected result" severity failure;
        report "ID2=" & to_hstring(rx_buf(3));
        assert rx_buf(3) = x"18" report "Unexpected result" severity failure;
        report "ID3=" & to_hstring(rx_buf(4));
        assert rx_buf(4) = x"00" report "Unexpected result" severity failure;
        report "ID4=" & to_hstring(rx_buf(5));
        assert rx_buf(5) = x"01" report "Unexpected result" severity failure;
        cs_clr;

        -- Send WREN
        report "Sending WREN...";
        cs_set;
        clr_bufs(tx_buf, rx_buf);
        tx_buf(0) := x"06";
        txrx(tx_buf, rx_buf, 1);
        cs_clr;

	-- Send PP to write
        report "Sending PP...";
        cs_set;
        clr_bufs(tx_buf, rx_buf);
        tx_buf(0) := x"02";
        -- Address
        tx_buf(1) := x"00";
        tx_buf(2) := x"00";
        tx_buf(3) := x"00";
        -- Data
        tx_buf(4) := x"aa";
        tx_buf(5) := x"55";
        tx_buf(6) := x"01";
        tx_buf(7) := x"02";
        txrx(tx_buf, rx_buf, 8);
        cs_clr;

	-- Wait for status
        report "Waiting for WIP=0...";
        loop
            cs_set;
            clr_bufs(tx_buf, rx_buf);
            tx_buf(0) := x"05";
            txrx(tx_buf, rx_buf, 2);
            cs_clr;
            dat := rx_buf(1);
            --report "RDSR=" & to_hstring(dat);
            if dat(0) = '0' then
                exit;
            end if;
        end loop;

        -- Send READ to read
        report "Sending READ...";
        cs_set;
        clr_bufs(tx_buf, rx_buf);
        tx_buf(0) := x"03";
        -- Address
        tx_buf(1) := x"00";
        tx_buf(2) := x"00";
        tx_buf(3) := x"00";
        txrx(tx_buf, rx_buf, 8);
        cs_clr;
	-- Data
        report "DATA0=" & to_hstring(rx_buf(4));
        -- assert dat = x"aa" report "Unexpected result" severity failure;
        report "DATA1=" & to_hstring(rx_buf(5));
        -- assert dat = x"55" report "Unexpected result" severity failure;
        report "DATA2=" & to_hstring(rx_buf(6));
        -- assert dat = x"01" report "Unexpected result" severity failure;
        report "DATA3=" & to_hstring(rx_buf(7));
        -- assert dat = x"02" report "Unexpected result" severity failure;
        cs_clr;
        
        wait for 1000 ns;
        std.env.finish;
    end process;
end;
