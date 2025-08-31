library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_fft_butterfly is
end entity;

architecture sim of tb_fft_butterfly is

    constant DATA_WIDTH : integer := 16;
    constant CLK_PERIOD : time    := 10 ns;

    -- DUT ports
    signal clk        : std_logic := '0';
    signal rst        : std_logic := '1';
    signal valid_in   : std_logic := '0';
    signal xr, xi     : signed(DATA_WIDTH-1 downto 0) := (others => '0');
    signal yr, yi     : signed(DATA_WIDTH-1 downto 0) := (others => '0');
    signal wr, wi     : signed(DATA_WIDTH-1 downto 0) := (others => '0');

    signal valid_out  : std_logic;
    signal out1r, out1i, out2r, out2i : signed(DATA_WIDTH-1 downto 0);

    -- Helper procedure for running tests
    procedure run_test(
        constant test_name : string;
        constant xr_val, xi_val : integer;
        constant yr_val, yi_val : integer;
        constant wr_val, wi_val : integer;
        constant exp_out1r, exp_out1i : integer;
        constant exp_out2r, exp_out2i : integer;
        signal clk : in std_logic;
        signal valid_in : out std_logic;
        signal xr, xi, yr, yi, wr, wi : out signed(DATA_WIDTH-1 downto 0);
        signal valid_out : in std_logic;
        signal out1r, out1i, out2r, out2i : in signed(DATA_WIDTH-1 downto 0)
    ) is
    begin
        -- Wait a bit between tests
        wait for CLK_PERIOD*3;
        
        -- Set inputs
        xr <= to_signed(xr_val, DATA_WIDTH);
        xi <= to_signed(xi_val, DATA_WIDTH);
        yr <= to_signed(yr_val, DATA_WIDTH);
        yi <= to_signed(yi_val, DATA_WIDTH);
        wr <= to_signed(wr_val, DATA_WIDTH);
        wi <= to_signed(wi_val, DATA_WIDTH);

        wait for CLK_PERIOD;
        valid_in <= '1';
        wait for CLK_PERIOD;
        valid_in <= '0';

        wait until valid_out = '1';

        report test_name & 
               " | out1r=" & integer'image(to_integer(out1r)) &
               " (exp:" & integer'image(exp_out1r) & ")" &
               " | out1i=" & integer'image(to_integer(out1i)) &
               " (exp:" & integer'image(exp_out1i) & ")" &
               " | out2r=" & integer'image(to_integer(out2r)) &
               " (exp:" & integer'image(exp_out2r) & ")" &
               " | out2i=" & integer'image(to_integer(out2i)) &
               " (exp:" & integer'image(exp_out2i) & ")"
               severity note;
               
        -- Check if results are close to expected (allow ±2 LSB tolerance)
        assert abs(to_integer(out1r) - exp_out1r) <= 2
            report test_name & " FAIL: out1r mismatch" severity error;
        assert abs(to_integer(out1i) - exp_out1i) <= 2
            report test_name & " FAIL: out1i mismatch" severity error;
        assert abs(to_integer(out2r) - exp_out2r) <= 2
            report test_name & " FAIL: out2r mismatch" severity error;
        assert abs(to_integer(out2i) - exp_out2i) <= 2
            report test_name & " FAIL: out2i mismatch" severity error;
    end procedure;

begin
    -- Clock generator: 100 MHz
    clk_proc: process
    begin
        clk <= '0'; wait for CLK_PERIOD/2;
        clk <= '1'; wait for CLK_PERIOD/2;
    end process;

    -- Instantiate the 4-stage pipelined butterfly
    uut: entity work.fft_butterfly
        generic map(DATA_WIDTH => DATA_WIDTH)
        port map(
            clk       => clk,
            rst       => rst,
            valid_in  => valid_in,
            xr        => xr,
            xi        => xi,
            yr        => yr,
            yi        => yi,
            wr        => wr,
            wi        => wi,
            valid_out => valid_out,
            out1r     => out1r,
            out1i     => out1i,
            out2r     => out2r,
            out2i     => out2i
        );

    -- Stimulus
    stim: process
    begin
        --------------------------------------------------------------------
        -- RESET
        --------------------------------------------------------------------
        rst <= '1';
        wait for 2*CLK_PERIOD;
        rst <= '0';
        wait for CLK_PERIOD;

        report "===== Starting FFT Butterfly Tests =====" severity note;

        --------------------------------------------------------------------
        -- TEST #1: Original test - all-real 1.0 × 1.0
        --------------------------------------------------------------------
        run_test(
            test_name => "TEST#1 Real 1.0x1.0",
            xr_val => 32767, xi_val => 0,
            yr_val => 32767, yi_val => 0,
            wr_val => 32767, wi_val => 0,
            exp_out1r => 32766, exp_out1i => 0,
            exp_out2r => 0, exp_out2i => 0,
            clk => clk, valid_in => valid_in,
            xr => xr, xi => xi, yr => yr, yi => yi, wr => wr, wi => wi,
            valid_out => valid_out, out1r => out1r, out1i => out1i, out2r => out2r, out2i => out2i
        );

        --------------------------------------------------------------------
        -- TEST #2: Original test - all-imag 0 + j·1.0
        --------------------------------------------------------------------
        run_test(
            test_name => "TEST#2 Imag j*1.0",
            xr_val => 0, xi_val => 0,
            yr_val => 0, yi_val => 32767,
            wr_val => 32767, wi_val => 0,
            exp_out1r => 0, exp_out1i => 16383,
            exp_out2r => 0, exp_out2i => -16383,
            clk => clk, valid_in => valid_in,
            xr => xr, xi => xi, yr => yr, yi => yi, wr => wr, wi => wi,
            valid_out => valid_out, out1r => out1r, out1i => out1i, out2r => out2r, out2i => out2i
        );

        --------------------------------------------------------------------
        -- TEST #3: Simple Real Addition
        -- x = 0.5, y = 0.25, w = 1.0
        --------------------------------------------------------------------
        run_test(
            test_name => "TEST#3 Real Add",
            xr_val => 16384, xi_val => 0,
            yr_val => 8192, yi_val => 0,
            wr_val => 32767, wi_val => 0,
            exp_out1r => 12288, exp_out1i => 0,
            exp_out2r => 4096, exp_out2i => 0,
            clk => clk, valid_in => valid_in,
            xr => xr, xi => xi, yr => yr, yi => yi, wr => wr, wi => wi,
            valid_out => valid_out, out1r => out1r, out1i => out1i, out2r => out2r, out2i => out2i
        );

        --------------------------------------------------------------------
        -- TEST #4: Complex Twiddle Factor (45°)
        -- x = 0.5 + j0.5, y = 0.5, w = 0.707 - j0.707
        --------------------------------------------------------------------
        run_test(
            test_name => "TEST#4 Complex 45deg",
            xr_val => 16384, xi_val => 16384,
            yr_val => 16384, yi_val => 0,
            wr_val => 23170, wi_val => -23170,
            exp_out1r => 14000, exp_out1i => 2400,
            exp_out2r => 2400, exp_out2i => 14000,
            clk => clk, valid_in => valid_in,
            xr => xr, xi => xi, yr => yr, yi => yi, wr => wr, wi => wi,
            valid_out => valid_out, out1r => out1r, out1i => out1i, out2r => out2r, out2i => out2i
        );

        --------------------------------------------------------------------
        -- TEST #5: Negative Real Values
        -- x = -0.5, y = 0.25, w = 1.0
        --------------------------------------------------------------------
        run_test(
            test_name => "TEST#5 Negative Real",
            xr_val => -16384, xi_val => 0,
            yr_val => 8192, yi_val => 0,
            wr_val => 32767, wi_val => 0,
            exp_out1r => -4096, exp_out1i => 0,
            exp_out2r => -12288, exp_out2i => 0,
            clk => clk, valid_in => valid_in,
            xr => xr, xi => xi, yr => yr, yi => yi, wr => wr, wi => wi,
            valid_out => valid_out, out1r => out1r, out1i => out1i, out2r => out2r, out2i => out2i
        );

        --------------------------------------------------------------------
        -- TEST #6: Pure Imaginary with Complex Twiddle
        -- x = j0.5, y = 0.5 + j0.5, w = -j1.0
        --------------------------------------------------------------------
        --TEST 6 EXPECTED VALUES INCORRECT
        run_test(
            test_name => "TEST#6 Pure Imag Complex",
            xr_val => 0, xi_val => 16384,
            yr_val => 16384, yi_val => 16384,
            wr_val => 0, wi_val => -32767,
            exp_out1r => 8192, exp_out1i => 16384,
            exp_out2r => -8192, exp_out2i => 0,
            clk => clk, valid_in => valid_in,
            xr => xr, xi => xi, yr => yr, yi => yi, wr => wr, wi => wi,
            valid_out => valid_out, out1r => out1r, out1i => out1i, out2r => out2r, out2i => out2i
        );

        --------------------------------------------------------------------
        -- TEST #7: Near-Saturation Test
        -- x = 0.9, y = 0.9, w = 1.0
        --------------------------------------------------------------------
        run_test(
            test_name => "TEST#7 Saturation",
            xr_val => 29491, xi_val => 0,
            yr_val => 29491, yi_val => 0,
            wr_val => 32767, wi_val => 0,
            exp_out1r => 32767, exp_out1i => 0,  -- Should saturate
            exp_out2r => 0, exp_out2i => 0,
            clk => clk, valid_in => valid_in,
            xr => xr, xi => xi, yr => yr, yi => yi, wr => wr, wi => wi,
            valid_out => valid_out, out1r => out1r, out1i => out1i, out2r => out2r, out2i => out2i
        );

        --------------------------------------------------------------------
        -- TEST #8: Zero Input Test
        -- x = 0, y = 0, w = 1.0
        --------------------------------------------------------------------
        run_test(
            test_name => "TEST#8 All Zeros",
            xr_val => 0, xi_val => 0,
            yr_val => 0, yi_val => 0,
            wr_val => 32767, wi_val => 0,
            exp_out1r => 0, exp_out1i => 0,
            exp_out2r => 0, exp_out2i => 0,
            clk => clk, valid_in => valid_in,
            xr => xr, xi => xi, yr => yr, yi => yi, wr => wr, wi => wi,
            valid_out => valid_out, out1r => out1r, out1i => out1i, out2r => out2r, out2i => out2i
        );

        report "===== All Tests Complete =====" severity note;
        wait;
    end process;

end architecture;