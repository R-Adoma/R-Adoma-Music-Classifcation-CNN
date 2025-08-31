library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity bitrev_pipe is
    generic (
        M : integer := 11  -- Address width (4 bits = 16-point FFT, 10 bits = 1024-point FFT)
    );
    port (
        clk       : in  std_logic;
        rst       : in  std_logic;
        in_valid  : in  std_logic;                      -- Input address is valid
        addr_in   : in  std_logic_vector(M-1 downto 0); -- Sequential address input
        out_valid : out std_logic;                      -- Output address is valid (2 clocks ltr)
        addr_out  : out std_logic_vector(M-1 downto 0)  -- Bit-reversed address output
    );
end entity;

architecture behavioral of bitrev_pipe is
    
    -- Pipeline registers
    signal v1       : std_logic := '0';                      -- Valid signal for stage 1
    signal a1       : std_logic_vector(M-1 downto 0) := (others => '0'); -- Address register for stage 1
    signal rev      : std_logic_vector(M-1 downto 0);       -- Combinational bit-reversed result
    
    -- Pipeline stage 2 registers
    signal out_valid_reg : std_logic := '0';
    signal addr_out_reg  : std_logic_vector(M-1 downto 0) := (others => '0');

begin

    -- Combinational bit-reversal logic
    bit_reverse_gen: for i in 0 to M-1 generate
        rev(M-1-i) <= a1(i);
    end generate;

    -- Stage 1: Register inputs
    stage1_proc: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                v1 <= '0';
                a1 <= (others => '0');
            else
                v1 <= in_valid;        
                a1 <= addr_in;         
            end if;
        end if;
    end process;

    -- Stage 2: Register bit-reversed output  
    stage2_proc: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                out_valid_reg <= '0';
                addr_out_reg  <= (others => '0');
            else
                out_valid_reg <= v1;       
                addr_out_reg  <= rev;    
            end if;
        end if;
    end process;

    -- Output assignments
    out_valid <= out_valid_reg;
    addr_out  <= addr_out_reg;

end architecture;