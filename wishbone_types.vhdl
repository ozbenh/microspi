library ieee;
use ieee.std_logic_1164.all;

package wishbone_types is
    --
    -- IO Bus to a device, 30-bit address, 32-bits data
    --
    type wb_io_master_out is record
        adr : std_ulogic_vector(29 downto 0);
        dat : std_ulogic_vector(31 downto 0);
        sel : std_ulogic_vector(3 downto 0);
        cyc : std_ulogic;
        stb : std_ulogic;
        we  : std_ulogic;
    end record;

    type wb_io_slave_out is record
        dat   : std_ulogic_vector(31 downto 0);
        ack   : std_ulogic;
        stall : std_ulogic;
    end record;
    
end package wishbone_types;
