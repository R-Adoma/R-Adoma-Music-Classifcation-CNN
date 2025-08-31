library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity fft_controller_tb is
end entity;

architecture testbench of fft_controller_tb is
    
    -- Constants for 8-point FFT
    constant N_POINTS          : integer := 8;
    constant DATA_WIDTH        : integer := 16;
    constant ADDR_WIDTH        : integer := 3;
    constant BUTTERFLY_LATENCY : integer := 4;
    constant BRAM_LATENCY      : integer := 2;
    
    -- Clock period
    constant CLK_PERIOD : time := 10 ns;
    
    -- Clock and reset
    signal clk : std_logic := '0';
    signal rst : std_logic := '1';
    
    -- Control signals
    signal start : std_logic := '0';
    signal done  : std_logic;
    signal busy  : std_logic;
    
    -- Input BRAM interface
    signal input_bram_addr : std_logic_vector(ADDR_WIDTH-1 downto 0);
    signal input_bram_dout : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal input_bram_en   : std_logic;
    
    -- BRAM A interface (dual port)
    signal bram_a_addr_a : std_logic_vector(ADDR_WIDTH-1 downto 0);
    signal bram_a_dout_a : std_logic_vector(2*DATA_WIDTH-1 downto 0);
    signal bram_a_en_a   : std_logic;
    signal bram_a_din_a  : std_logic_vector(2*DATA_WIDTH-1 downto 0);
    signal bram_a_we_a   : std_logic;
    
    signal bram_a_addr_b : std_logic_vector(ADDR_WIDTH-1 downto 0);
    signal bram_a_din_b  : std_logic_vector(2*DATA_WIDTH-1 downto 0);
    signal bram_a_dout_b : std_logic_vector(2*DATA_WIDTH-1 downto 0);
    signal bram_a_we_b   : std_logic;
    signal bram_a_en_b   : std_logic;
    
    -- BRAM B interface (dual port)
    signal bram_b_addr_a : std_logic_vector(ADDR_WIDTH-1 downto 0);
    signal bram_b_din_a  : std_logic_vector(2*DATA_WIDTH-1 downto 0);
    signal bram_b_we_a   : std_logic;
    signal bram_b_en_a   : std_logic;
    signal bram_b_dout_a : std_logic_vector(2*DATA_WIDTH-1 downto 0);
    
    signal bram_b_addr_b : std_logic_vector(ADDR_WIDTH-1 downto 0);
    signal bram_b_din_b  : std_logic_vector(2*DATA_WIDTH-1 downto 0);
    signal bram_b_we_b   : std_logic;
    signal bram_b_en_b   : std_logic;
    signal bram_b_dout_b : std_logic_vector(2*DATA_WIDTH-1 downto 0);
    
    -- BRAM C interface (results)
    signal bram_c_addr : std_logic_vector(ADDR_WIDTH-1 downto 0);
    signal bram_c_din  : std_logic_vector(2*DATA_WIDTH-1 downto 0);
    signal bram_c_we   : std_logic;
    signal bram_c_en   : std_logic;
    
    -- Magnitude module handshaking
    signal results_ready     : std_logic;
    signal magnitude_reading : std_logic := '0';
    signal magnitude_done    : std_logic := '0';
    
    -- Bit-reverse pipe interface
    signal bitrev_valid_in  : std_logic;
    signal bitrev_addr_in   : std_logic_vector(ADDR_WIDTH-1 downto 0);
    signal bitrev_valid_out : std_logic;
    signal bitrev_addr_out  : std_logic_vector(ADDR_WIDTH-1 downto 0);
    
    -- Test input data array
    type input_array_t is array (0 to N_POINTS-1) of std_logic_vector(DATA_WIDTH-1 downto 0);
    signal test_input : input_array_t := (
        x"2000",  -- 0.25 in Q1.15
        x"2000",  -- 0.25 in Q1.15  
        x"0000",  -- 0.0
        x"0000",  -- 0.0
        x"0000",  -- 0.0
        x"0000",  -- 0.0
        x"0000",  -- 0.0
        x"0000"   -- 0.0
    );
    
    -- Memory arrays for BRAM simulation
    type bram_mem_t is array (0 to N_POINTS-1) of std_logic_vector(2*DATA_WIDTH-1 downto 0);
    signal bram_a_mem : bram_mem_t := (others => (others => '0'));
    signal bram_b_mem : bram_mem_t := (others => (others => '0'));
    signal bram_c_mem : bram_mem_t := (others => (others => '0'));
    
    -- Simulation control
    signal sim_done : boolean := false;

begin

    -- Clock generation
    clk_proc: process
    begin
        while not sim_done loop
            clk <= '0';
            wait for CLK_PERIOD/2;
            clk <= '1';
            wait for CLK_PERIOD/2;
        end loop;
        wait;
    end process;
    
    -- Instantiate FFT Controller
    dut: entity work.fft_controller
        generic map (
            N_POINTS          => N_POINTS,
            DATA_WIDTH        => DATA_WIDTH,
            ADDR_WIDTH        => ADDR_WIDTH,
            BUTTERFLY_LATENCY => BUTTERFLY_LATENCY,
            BRAM_LATENCY      => BRAM_LATENCY
        )
        port map (
            clk    => clk,
            rst    => rst,
            start  => start,
            done   => done,
            busy   => busy,
            
            input_bram_addr => input_bram_addr,
            input_bram_dout => input_bram_dout,
            input_bram_en   => input_bram_en,
            
            bram_a_addr_a => bram_a_addr_a,
            bram_a_dout_a => bram_a_dout_a,
            bram_a_en_a   => bram_a_en_a,
            bram_a_din_a  => bram_a_din_a,
            bram_a_we_a   => bram_a_we_a,
            
            bram_a_addr_b => bram_a_addr_b,
            bram_a_din_b  => bram_a_din_b,
            bram_a_dout_b => bram_a_dout_b,
            bram_a_we_b   => bram_a_we_b,
            bram_a_en_b   => bram_a_en_b,
            
            bram_b_addr_a => bram_b_addr_a,
            bram_b_din_a  => bram_b_din_a,
            bram_b_we_a   => bram_b_we_a,
            bram_b_en_a   => bram_b_en_a,
            bram_b_dout_a => bram_b_dout_a,
            
            bram_b_addr_b => bram_b_addr_b,
            bram_b_din_b  => bram_b_din_b,
            bram_b_we_b   => bram_b_we_b,
            bram_b_en_b   => bram_b_en_b,
            bram_b_dout_b => bram_b_dout_b,
            
            bram_c_addr => bram_c_addr,
            bram_c_din  => bram_c_din,
            bram_c_we   => bram_c_we,
            bram_c_en   => bram_c_en,
            
            results_ready     => results_ready,
            magnitude_reading => magnitude_reading,
            magnitude_done    => magnitude_done,
            
            bitrev_valid_in  => bitrev_valid_in,
            bitrev_addr_in   => bitrev_addr_in,
            bitrev_valid_out => bitrev_valid_out,
            bitrev_addr_out  => bitrev_addr_out
        );
    
    -- Bit-reverse pipe
    bitrev_inst: entity work.bitrev_pipe
        generic map (
            M => ADDR_WIDTH
        )
        port map (
            clk       => clk,
            rst       => rst,
            in_valid  => bitrev_valid_in,
            addr_in   => bitrev_addr_in,
            out_valid => bitrev_valid_out,
            addr_out  => bitrev_addr_out
        );
    
    -- Input BRAM simulation
    input_bram_proc: process(clk)
        variable delay_addr : std_logic_vector(ADDR_WIDTH-1 downto 0);
        variable delay_en : std_logic;
        variable addr_int : integer;
    begin
        if rising_edge(clk) then
            if rst = '1' then
                input_bram_dout <= (others => '0');
                delay_addr := (others => '0');
                delay_en := '0';
            else
                -- 1-cycle delay 
                delay_addr := input_bram_addr;
                delay_en := input_bram_en;
                
                if delay_en = '1' then
                    addr_int := to_integer(unsigned(delay_addr));
                    if addr_int >= 0 and addr_int < N_POINTS then
                        input_bram_dout <= test_input(addr_int);
                    else
                        input_bram_dout <= (others => '0');
                    end if;
                end if;
            end if;
        end if;
    end process;
    
    -- BRAM A simulation 
    bram_a_proc: process(clk)
        variable addr_a_int, addr_b_int : integer;
    begin
        if rising_edge(clk) then
            if rst = '1' then
                bram_a_mem <= (others => (others => '0'));
                bram_a_dout_a <= (others => '0');
                bram_a_dout_b <= (others => '0');
            else
                -- Port A operations
                if bram_a_en_a = '1' then
                    addr_a_int := to_integer(unsigned(bram_a_addr_a));
                    if addr_a_int >= 0 and addr_a_int < N_POINTS then
                        if bram_a_we_a = '1' then
                            bram_a_mem(addr_a_int) <= bram_a_din_a;
                        else
                            bram_a_dout_a <= bram_a_mem(addr_a_int);
                        end if;
                    end if;
                end if;
                
                -- Port B operations
                if bram_a_en_b = '1' then
                    addr_b_int := to_integer(unsigned(bram_a_addr_b));
                    if addr_b_int >= 0 and addr_b_int < N_POINTS then
                        if bram_a_we_b = '1' then
                            bram_a_mem(addr_b_int) <= bram_a_din_b;
                        else
                            bram_a_dout_b <= bram_a_mem(addr_b_int);
                        end if;
                    end if;
                end if;
            end if;
        end if;
    end process;
    
    -- BRAM B simulation 
    bram_b_proc: process(clk)
        variable addr_a_int, addr_b_int : integer;
    begin
        if rising_edge(clk) then
            if rst = '1' then
                bram_b_mem <= (others => (others => '0'));
                bram_b_dout_a <= (others => '0');
                bram_b_dout_b <= (others => '0');
            else
                -- Port A operations
                if bram_b_en_a = '1' then
                    addr_a_int := to_integer(unsigned(bram_b_addr_a));
                    if addr_a_int >= 0 and addr_a_int < N_POINTS then
                        if bram_b_we_a = '1' then
                            bram_b_mem(addr_a_int) <= bram_b_din_a;
                        else
                            bram_b_dout_a <= bram_b_mem(addr_a_int);
                        end if;
                    end if;
                end if;
                
                -- Port B operations
                if bram_b_en_b = '1' then
                    addr_b_int := to_integer(unsigned(bram_b_addr_b));
                    if addr_b_int >= 0 and addr_b_int < N_POINTS then
                        if bram_b_we_b = '1' then
                            bram_b_mem(addr_b_int) <= bram_b_din_b;
                        else
                            bram_b_dout_b <= bram_b_mem(addr_b_int);
                        end if;
                    end if;
                end if;
            end if;
        end if;
    end process;
    
    -- BRAM C simulation
    bram_c_proc: process(clk)
        variable addr_int : integer;
    begin
        if rising_edge(clk) then
            if rst = '1' then
                bram_c_mem <= (others => (others => '0'));
            else
                if bram_c_en = '1' and bram_c_we = '1' then
                    addr_int := to_integer(unsigned(bram_c_addr));
                    if addr_int >= 0 and addr_int < N_POINTS then
                        bram_c_mem(addr_int) <= bram_c_din;
                    end if;
                end if;
            end if;
        end if;
    end process;
    
    -- Main test process
    test_proc: process
    begin
        -- Initialize
        report "Starting FFT Controller Testbench";
        rst <= '1';
        start <= '0';
        wait for 100 ns;
        
        -- Release reset
        rst <= '0';
        wait for 50 ns;
        
        -- Start FFT
        report "Starting FFT operation";
        start <= '1';
        wait for CLK_PERIOD;
        start <= '0';
        
        -- Wait for completion
        wait until done = '1' for 10000 ns;
        
        if done = '1' then
            report "FFT completed successfully!";
        else
            report "ERROR: FFT did not complete within timeout!" severity error;
        end if;
        
        -- Simulate magnitude module
        wait for 100 ns;
        magnitude_reading <= '1';
        wait for 200 ns;
        magnitude_reading <= '0';
        magnitude_done <= '1';
        wait for CLK_PERIOD;
        magnitude_done <= '0';
        
        wait for 100 ns;
        report "Test completed";
        sim_done <= true;
        wait;
    end process;

end architecture;