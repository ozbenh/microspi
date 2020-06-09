library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.wishbone_types.all;

library model;
use model.all;

entity tb_flash is
end tb_flash;

architecture behave of tb_flash is
    signal clk : std_ulogic;
    signal rst : std_ulogic := '1';

    -- testbench signals
    constant clk_period : time := 10 ns;

    signal wb_out : wb_io_master_out;
    signal wb_in  : wb_io_slave_out;
    signal wb_sel_reg : std_ulogic;
    signal wb_sel_map : std_ulogic;
    signal sck     : std_ulogic;
    signal cs_n    : std_ulogic;
    signal sdat_o  : std_ulogic_vector(3 downto 0);
    signal sdat_oe : std_ulogic_vector(3 downto 0);
    signal sdat_i  : std_ulogic_vector(3 downto 0);
    
    signal rdat    : std_ulogic_vector(31 downto 0);
    signal hold_n  : std_logic := 'Z';
    signal wp_n    : std_logic := 'Z';
    signal mosi    : std_logic;
    signal miso    : std_logic;

begin

    spi: entity work.spi_flash_ctrl
        generic map (
            DATA_LINES => 4
            )
        port map(
            rst => rst,
            clk => clk,
            wb_in => wb_out,
            wb_out => wb_in,
            wb_sel_reg => wb_sel_reg,
            wb_sel_map => wb_sel_map,
            sck => sck,
            cs_n => cs_n,
            sdat_o => sdat_o,
            sdat_oe => sdat_oe,
            sdat_i => sdat_i
            );

        flash: entity model.s25fl128s
        generic map (
            TimingModel => "S25FL128SAGNFI000_R_30pF",
            LongTimming => false
            )
        port map(
            SCK => sck,
            SI => mosi,
            CSNeg => cs_n,
            HOLDNeg => hold_n,
            WPNeg => wp_n,
            RSTNeg => '1',
            SO => miso -- sdat_i(0)
            );

    mosi   <= sdat_o(0) when sdat_oe(0) = '1' else 'Z';
    miso   <= sdat_o(1) when sdat_oe(1) = '1' else 'Z';
    wp_n   <= sdat_o(2) when sdat_oe(2) = '1' else 'Z';
    hold_n <= sdat_o(3) when sdat_oe(3) = '1' else '1' when sdat_oe(0) = '1' else 'Z';

    sdat_i(0) <= mosi;
    sdat_i(1) <= miso;
    sdat_i(2) <= wp_n;
    sdat_i(3) <= hold_n;
    
    clk_process: process
    begin
        clk <= '0';
        wait for clk_period/2;
        clk <= '1';
        wait for clk_period/2;
    end process;

    test : process
        procedure wb_reg_cycle is
        begin
            wb_out.cyc <= '1';
            wb_out.stb <= '1';
            wb_sel_reg <= '1';
            loop
            wait until rising_edge(clk);
                if wb_in.stall = '0' then
                    wb_out.stb <= '0';
                end if;
                if wb_in.ack = '1' then
                    wb_out.cyc <= '0';
                    rdat <= wb_in.dat;
                    -- Need a wait for rdat to be visible
                    wait until falling_edge(clk);
                    exit;
                end if;
            end loop;
            wb_sel_reg <= '0';
        end procedure;

        procedure wb_map_read(adr: in  std_ulogic_vector(23 downto 0);
                              dat: out std_ulogic_vector(31 downto 0)) is
        begin
            wb_out.cyc <= '1';
            wb_out.stb <= '1';
            wb_out.we  <= '0';
            wb_out.adr <= x"00" & adr(23 downto 2);
            wb_sel_map <= '1';
            loop
            wait until rising_edge(clk);
                if wb_in.stall = '0' then
                    wb_out.stb <= '0';
                end if;
                if wb_in.ack = '1' then
                    wb_out.cyc <= '0';
                    dat := wb_in.dat;
                    -- Need a wait for rdat to be visible
                    wait until falling_edge(clk);
                    exit;
                end if;
            end loop;
            wb_sel_map <= '0';
        end procedure;

        function encode_sel (adr : std_ulogic_vector(1 downto 0)) return std_ulogic_vector is
        begin
            case adr is
            when "00" =>
                return "0001";
            when "01" =>
                return "0010";
            when "10" =>
                return "0100";
            when "11" =>
                return "1000";
            when others =>
                return "UUUU";
            end case;
        end function;
            
        procedure wb_reg_write(adr: in std_ulogic_vector(7 downto 0);
                               dat: in std_ulogic_vector(7 downto 0)) is
        begin
            wb_out.adr(5 downto 0) <= adr(7 downto 2);
            wb_out.sel <= encode_sel(adr(1 downto 0));
            wb_out.dat <= x"000000" & dat;
            wb_out.we  <= '1';
            wb_reg_cycle;
        end procedure;

        procedure wb_reg_read(adr: in  std_ulogic_vector(7 downto 0);
                              dat: out std_ulogic_vector(7 downto 0)) is
        begin
            wb_out.adr(5 downto 0) <= adr(7 downto 2);
            wb_out.sel <= encode_sel(adr(1 downto 0));
            wb_out.we  <= '0';
            wb_reg_cycle;
            dat := rdat(7 downto 0);
        end procedure;

        procedure cs_set is
        begin
            wb_reg_write(x"04", x"02");
        end procedure;

        procedure cs_clr is
        begin
            wb_reg_write(x"04", x"00");
        end procedure;

        procedure cmd_write(cmd : in std_ulogic_vector(7 downto 0)) is
        begin
            wb_reg_write(x"00", cmd);
        end procedure;

        procedure dat_write(dat : in std_ulogic_vector(7 downto 0)) is
        begin
            wb_reg_write(x"00", dat);
        end procedure;

        procedure dat_read(dat : out std_ulogic_vector(7 downto 0)) is
        begin
            wb_reg_read(x"00", dat);
        end procedure;

        procedure dat2_read(dat : out std_ulogic_vector(7 downto 0)) is
        begin
            wb_reg_read(x"01", dat);
        end procedure;

        procedure dat4_read(dat : out std_ulogic_vector(7 downto 0)) is
        begin
            wb_reg_read(x"02", dat);
        end procedure;

        variable dat : std_ulogic_vector(7 downto 0);
        variable d32 : std_ulogic_vector(31 downto 0);

    begin
        rst <= '1';
        wb_out.stb <= '0';
        wb_out.cyc <= '0';
        wb_out.adr <= (others => '0');
        wb_sel_reg <= '0';
        wb_sel_map <= '0';

        -- Leave flash a chance...
        wait for 1 ms;
        rst <= '0';
        wait for 1 ms;
        wait until rising_edge(clk);

        -- Sending read ID command
        report "Sending RDID...";
        cs_set;
        cmd_write(x"9f");
        dat_read(dat);
        report "ID0=" & to_hstring(dat);
        assert dat = x"01" report "Unexpected result" severity failure;
        dat_read(dat);
        report "ID1=" & to_hstring(dat);
        assert dat = x"20" report "Unexpected result" severity failure;
        dat_read(dat);
        report "ID2=" & to_hstring(dat);
        assert dat = x"18" report "Unexpected result" severity failure;
        dat_read(dat);
        report "ID3=" & to_hstring(dat);
        assert dat = x"00" report "Unexpected result" severity failure;
        dat_read(dat);
        report "ID4=" & to_hstring(dat);
        assert dat = x"01" report "Unexpected result" severity failure;
        cs_clr;

        -- Sending read ID command again
        report "Sending RDID again...";
        cs_set;
        cmd_write(x"9f");
        -- Random delay
        for i in 0 to 15 loop
           wait until rising_edge(clk);
        end loop;
        dat_read(dat);
        report "ID0=" & to_hstring(dat);
        assert dat = x"01" report "Unexpected result" severity failure;
        dat_read(dat);
        report "ID1=" & to_hstring(dat);
        assert dat = x"20" report "Unexpected result" severity failure;
        -- Random delay
        for i in 0 to 15 loop
           wait until rising_edge(clk);
        end loop;
        dat_read(dat);
        report "ID2=" & to_hstring(dat);
        assert dat = x"18" report "Unexpected result" severity failure;
        dat_read(dat);
        report "ID3=" & to_hstring(dat);
        assert dat = x"00" report "Unexpected result" severity failure;
        dat_read(dat);
        report "ID4=" & to_hstring(dat);
        assert dat = x"01" report "Unexpected result" severity failure;
        cs_clr;

        -- Send WREN
        report "Sending WREN...";
        cs_set;
        cmd_write(x"06");
        cs_clr;

        -- Send PP to write
        report "Sending PP...";
        cs_set;
        cmd_write(x"02");
        -- Address
        dat_write(x"00");
        dat_write(x"00");
        dat_write(x"00");
        -- Data
        dat_write(x"aa");
        dat_write(x"55");
        dat_write(x"01");
        dat_write(x"02");
        dat_write(x"f0");
        dat_write(x"0d");
        dat_write(x"03");
        dat_write(x"04");
        dat_write(x"ba");
        dat_write(x"af");
        dat_write(x"05");
        dat_write(x"06");
        cs_clr;

        -- Wait for status 
        report "Waiting on RDSR...";
       loop
            cs_set;
            cmd_write(x"05");
            dat_read(dat);
            cs_clr;
            --report "RDSR=" & to_hstring(dat);
            if dat(0) = '0' then
                report "Done";
                exit;
            end if;
        end loop;

        -- Send READ to read
        report "Sending READ...";
        cs_set;
        cmd_write(x"03");
        -- Address
        dat_write(x"00");
        dat_write(x"00");
        dat_write(x"00");
        -- Data
        dat_read(dat);
        report "DATaA0=" & to_hstring(dat);
        assert dat = x"aa" report "Unexpected result" severity failure;
        dat_read(dat);
        report "DATA1=" & to_hstring(dat);
        assert dat = x"55" report "Unexpected result" severity failure;
        dat_read(dat);
        report "DATA2=" & to_hstring(dat);
        assert dat = x"01" report "Unexpected result" severity failure;
        dat_read(dat);
        report "DATA3=" & to_hstring(dat);
        assert dat = x"02" report "Unexpected result" severity failure;
        dat_read(dat);
        report "DATA4=" & to_hstring(dat);
        dat_read(dat);
        report "DATA5=" & to_hstring(dat);
        dat_read(dat);
        report "DATA6=" & to_hstring(dat);
        dat_read(dat);
        report "DATA7=" & to_hstring(dat);
        dat_read(dat);
        report "DATA8=" & to_hstring(dat);
        dat_read(dat);
        report "DATA9=" & to_hstring(dat);
        dat_read(dat);
        report "DATAa=" & to_hstring(dat);
        dat_read(dat);
        report "DATAb=" & to_hstring(dat);
        cs_clr;

        -- Try reading via memory map
        report "Reading map...";
        wb_map_read(x"000000", d32);
        report "DATA0=" & to_hstring(d32);
        wb_map_read(x"000000", d32);
        report "DATA0=" & to_hstring(d32);
        wb_map_read(x"000004", d32);
        report "DATA4=" & to_hstring(d32);
        wb_map_read(x"000004", d32);
        report "DATA4=" & to_hstring(d32);
        wb_map_read(x"000008", d32);
        report "DATA8=" & to_hstring(d32);

        -- Try reading via dual mode
        report "Sending DUAL FAST READ...";
        cs_set;
        cmd_write(x"3b");
        -- Address
        dat_write(x"00");
        dat_write(x"00");
        dat_write(x"00");
        -- Dummy
        dat_write(x"00");
        -- Data
        dat2_read(dat);
        report "DATA0=" & to_hstring(dat);
        assert dat = x"aa" report "Unexpected result" severity failure;
        dat2_read(dat);
        report "DATA1=" & to_hstring(dat);
        assert dat = x"55" report "Unexpected result" severity failure;
        dat2_read(dat);
        report "DATA2=" & to_hstring(dat);
        assert dat = x"01" report "Unexpected result" severity failure;
        dat2_read(dat);
        report "DATA3=" & to_hstring(dat);
        assert dat = x"02" report "Unexpected result" severity failure;
        dat2_read(dat);
        cs_clr;

        -- Enable QUAD mode
        report "Enabling QUAD mode";
        cs_set;
        cmd_write(x"35");
        dat_read(dat);
        cs_clr;
        report "  CF1=" & to_hstring(dat);
        cs_set;
        cmd_write(x"05");
        dat_read(dat);
        cs_clr;
        report "  ST1=" & to_hstring(dat);
        cs_set;
        report "  Sending WREN...";
        cs_set;
        cmd_write(x"06");
        cs_clr;
        report "  Writing register...";
        cs_set;
        cmd_write(x"01");
        cmd_write(x"00");
        cmd_write(x"02");
        cs_clr;
        report "  Waiting on RDSR...";
        loop
            cs_set;
            cmd_write(x"05");
            dat_read(dat);
            cs_clr;
            --report "RDSR=" & to_hstring(dat);
            if dat(0) = '0' then
                report "  Done";
                exit;
            end if;
        end loop;
        cs_set;
        cmd_write(x"35");
        dat_read(dat);
        cs_clr;
        report "  CF1=" & to_hstring(dat);
      
 
        -- Try reading via quad mode
        report "Sending QUAD FAST READ...";
        cs_set;
        cmd_write(x"6b");
        -- Address
        dat_write(x"00");
        dat_write(x"00");
        dat_write(x"00");
        -- Dummy
        dat_write(x"00");
        -- Data
        dat4_read(dat);
        report "DATA0=" & to_hstring(dat);
        assert dat = x"aa" report "Unexpected result" severity failure;
        dat4_read(dat);
        report "DATA1=" & to_hstring(dat);
        assert dat = x"55" report "Unexpected result" severity failure;
        dat4_read(dat);
        report "DATA2=" & to_hstring(dat);
        assert dat = x"01" report "Unexpected result" severity failure;
        dat4_read(dat);
        report "DATA3=" & to_hstring(dat);
        assert dat = x"02" report "Unexpected result" severity failure;
        dat4_read(dat);
        cs_clr;
      
        wait for 1000 ns;
        std.env.finish;
    end process;
end;
