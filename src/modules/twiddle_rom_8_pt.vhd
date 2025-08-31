library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- N=8 twiddles W8^k = exp(-j*2*pi*k/8) = cos - j sin
-- k = 0..3 used
entity twiddle_rom_8pt is
    port (
        clk     : in  std_logic;
        en      : in  std_logic;
        addr    : in  std_logic_vector(1 downto 0); -- 0..3
        data_re : out std_logic_vector(15 downto 0);
        data_im : out std_logic_vector(15 downto 0)
    );
end entity;

architecture rtl of twiddle_rom_8pt is
    type vec16 is array(0 to 3) of std_logic_vector(15 downto 0);
    constant C_RE : vec16 := (
        -- cos(0), cos(pi/4), cos(pi/2), cos(3pi/4)
        std_logic_vector(to_signed( 32767,16)),  -- 1.0000
        std_logic_vector(to_signed( 23170,16)),  -- 0.7071
        std_logic_vector(to_signed(     0,16)),  -- 0
        std_logic_vector(to_signed(-23170,16))   -- -0.7071
    );
    constant C_IM : vec16 := (
        -- -sin(0), -sin(pi/4), -sin(pi/2), -sin(3pi/4)
        std_logic_vector(to_signed(     0,16)),  -- 0
        std_logic_vector(to_signed(-23170,16)),  -- -0.7071
        std_logic_vector(to_signed(-32767,16)),  -- -1.0000
        std_logic_vector(to_signed(-23170,16))   -- -0.7071
    );

    signal re_q, im_q : std_logic_vector(15 downto 0) := (others=>'0');
begin
    process(clk)
        variable ai : integer;
    begin
        if rising_edge(clk) then
            if en = '1' then
                ai := to_integer(unsigned(addr));
                re_q <= C_RE(ai);
                im_q <= C_IM(ai);
            end if;
        end if;
    end process;

    data_re <= re_q;
    data_im <= im_q;
end architecture;
