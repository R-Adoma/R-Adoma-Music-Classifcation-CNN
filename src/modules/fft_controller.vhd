library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity fft_controller is
    generic (
--        N_POINTS          : integer := 8;      -- FFT size (must be power of 2)
--        DATA_WIDTH        : integer := 16;     -- Q1.15 sample width  
--        ADDR_WIDTH        : integer := 3;      -- log2(N_POINTS) - user must ensure this is correct
--        BUTTERFLY_LATENCY : integer := 4;      -- Butterfly module processing cycles
--        BRAM_LATENCY      : integer := 2       -- BRAM read latency (from Vivado IP)
        N_POINTS          : integer := 2048;      -- FFT size (must be power of 2)
        DATA_WIDTH        : integer := 16;     -- Q1.15 sample width  
        ADDR_WIDTH        : integer := 11;      -- log2(N_POINTS) - user must ensure this is correct
        BUTTERFLY_LATENCY : integer := 4;      -- Butterfly module processing cycles
        BRAM_LATENCY      : integer := 2       -- BRAM read latency (from Vivado IP)
    );
    port (
        -- Clock and Reset
        clk    : in  std_logic;
        rst    : in  std_logic;
        
        -- Control Interface
        start  : in  std_logic;         -- Pulse to begin FFT processing
        done   : out std_logic;         -- High when FFT complete, results ready
        busy   : out std_logic;         -- High during processing
        
        -- Input BRAM Interface (from Hann window output)
        input_bram_addr : out std_logic_vector(ADDR_WIDTH-1 downto 0);
        input_bram_dout : in  std_logic_vector(DATA_WIDTH-1 downto 0);    -- 16-bit real only
        input_bram_en   : out std_logic;
        
        -- BRAM A Interface - Dual Port (FFT working memory)
        -- Port A: Read x addresses
        bram_a_addr_a : out std_logic_vector(ADDR_WIDTH-1 downto 0);
        bram_a_dout_a : in  std_logic_vector(2*DATA_WIDTH-1 downto 0);
        bram_a_en_a   : out std_logic;
        bram_a_din_a  : out std_logic_vector(2*DATA_WIDTH-1 downto 0);  
        bram_a_we_a   : out std_logic;                                 
        
        -- Port B: Read y addresses OR write during load
        bram_a_addr_b : out std_logic_vector(ADDR_WIDTH-1 downto 0);
        bram_a_din_b  : out std_logic_vector(2*DATA_WIDTH-1 downto 0);
        bram_a_dout_b : in  std_logic_vector(2*DATA_WIDTH-1 downto 0);
        bram_a_we_b   : out std_logic;
        bram_a_en_b   : out std_logic;
        
        -- BRAM B Interface - Dual Port (FFT working memory)
        -- Port A: Write butterfly out1 results
        bram_b_addr_a : out std_logic_vector(ADDR_WIDTH-1 downto 0);
        bram_b_din_a  : out std_logic_vector(2*DATA_WIDTH-1 downto 0);
        bram_b_we_a   : out std_logic;
        bram_b_en_a   : out std_logic;
        bram_b_dout_a : in  std_logic_vector(2*DATA_WIDTH-1 downto 0);  -- Add this

        
        -- Port B: Write butterfly out2 results  
        bram_b_addr_b : out std_logic_vector(ADDR_WIDTH-1 downto 0);
        bram_b_din_b  : out std_logic_vector(2*DATA_WIDTH-1 downto 0);
        bram_b_we_b   : out std_logic;
        bram_b_en_b   : out std_logic;
        bram_b_dout_b : in  std_logic_vector(2*DATA_WIDTH-1 downto 0);  -- Add this

        -- BRAM C Interface - Single Port (Results buffer for magnitude module)
        bram_c_addr : out std_logic_vector(ADDR_WIDTH-1 downto 0);
        bram_c_din  : out std_logic_vector(2*DATA_WIDTH-1 downto 0);
        bram_c_we   : out std_logic;
        bram_c_en   : out std_logic;
        
        -- Magnitude module handshaking for BRAM C
        results_ready     : out std_logic;  -- FFT controller asserts when BRAM C populated
        magnitude_reading : in  std_logic;  -- Magnitude module asserts while reading BRAM C
        magnitude_done    : in  std_logic;  -- Magnitude module pulses when finished reading
        
        -- Bit-Reverse Pipe Interface (for input loading)
        bitrev_valid_in  : out std_logic;                                 -- Start bit-reverse operation
        bitrev_addr_in   : out std_logic_vector(ADDR_WIDTH-1 downto 0);   -- Sequential address input
        bitrev_valid_out : in  std_logic;                                 -- Bit-reversed address ready
        bitrev_addr_out  : in  std_logic_vector(ADDR_WIDTH-1 downto 0)    -- Bit-reversed address output
    );
end entity fft_controller;

architecture behavioral of fft_controller is

    -- =========================================================================
    -- COMPONENT DECLARATIONS (for internal instantiation)
    -- =========================================================================
    
    component fft_butterfly is
        generic (
            DATA_WIDTH : integer := 16
        );
        port (
            clk       : in  std_logic;
            rst       : in  std_logic;
            valid_in  : in  std_logic;
            xr, xi    : in  signed(DATA_WIDTH-1 downto 0);
            yr, yi    : in  signed(DATA_WIDTH-1 downto 0);
            wr, wi    : in  signed(DATA_WIDTH-1 downto 0);
            valid_out : out std_logic;
            out1r     : out signed(DATA_WIDTH-1 downto 0);
            out1i     : out signed(DATA_WIDTH-1 downto 0);
            out2r     : out signed(DATA_WIDTH-1 downto 0);
            out2i     : out signed(DATA_WIDTH-1 downto 0)
        );
    end component;
    
    component twiddle_rom_8pt is
        port (
            clk     : in  std_logic;
            en      : in  std_logic;
            addr    : in  std_logic_vector(ADDR_WIDTH-2 downto 0);  -- log2(N_POINTS/2) bits
            data_re : out std_logic_vector(15 downto 0);
            data_im : out std_logic_vector(15 downto 0)
        );
    end component;

    -- =========================================================================
    -- INTERNAL SIGNALS FOR MODULE INTERFACES
    -- =========================================================================
    
    -- Butterfly module internal signals
    signal butterfly_valid_in  : std_logic := '0';
    signal butterfly_xr        : signed(DATA_WIDTH-1 downto 0) := (others => '0');
    signal butterfly_xi        : signed(DATA_WIDTH-1 downto 0) := (others => '0');
    signal butterfly_yr        : signed(DATA_WIDTH-1 downto 0) := (others => '0');
    signal butterfly_yi        : signed(DATA_WIDTH-1 downto 0) := (others => '0');
    signal butterfly_wr        : signed(DATA_WIDTH-1 downto 0) := (others => '0');
    signal butterfly_wi        : signed(DATA_WIDTH-1 downto 0) := (others => '0');
    signal butterfly_valid_out : std_logic;
    signal butterfly_out1r     : signed(DATA_WIDTH-1 downto 0);
    signal butterfly_out1i     : signed(DATA_WIDTH-1 downto 0);
    signal butterfly_out2r     : signed(DATA_WIDTH-1 downto 0);
    signal butterfly_out2i     : signed(DATA_WIDTH-1 downto 0);
    
    -- Twiddle ROM internal signals
    signal twiddle_en          : std_logic := '0';
    signal twiddle_addr_gen    : std_logic_vector(ADDR_WIDTH-1 downto 0) := (others => '0');
    signal twiddle_addr_delayed: std_logic_vector(ADDR_WIDTH-2 downto 0) := (others => '0');  -- log2(N_POINTS/2) = ADDR_WIDTH-1 bits
    signal twiddle_re          : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal twiddle_im          : std_logic_vector(DATA_WIDTH-1 downto 0);

    -- =========================================================================
    -- PIPELINE PARAMETERS (derived from generics for modularity)
    -- =========================================================================
    
    constant TOTAL_PIPELINE : integer := BUTTERFLY_LATENCY + BRAM_LATENCY + 2; -- +2 for address/twiddle setup
    
    -- =========================================================================
    -- STATE MACHINE AND CONTROL SIGNALS
    -- =========================================================================
    
    type fft_state_t is (IDLE, LOAD_INPUT, PROCESSING, COPY_RESULTS, OUTPUT_READY, DONE_STATE);
    signal state : fft_state_t := IDLE;
    
    -- Stage control
    constant NUM_STAGES : integer := ADDR_WIDTH;  -- log2(N_POINTS)
    signal current_stage : integer range 0 to NUM_STAGES-1 := 0;
    signal stage_complete : std_logic := '0';
    
    signal copy_in_progress : std_logic := '0';  -- BRAM B → BRAM C copy active
    
    -- =========================================================================
    -- DYNAMIC STAGE PARAMETERS 
    -- =========================================================================
    
    signal span : integer range 2 to N_POINTS := 2;
    signal half : integer range 1 to N_POINTS/2 := 1;
    signal groups : integer range 1 to N_POINTS/2 := N_POINTS/2;
    signal butterflies_per_stage : integer range 1 to N_POINTS/2 := N_POINTS/2;
    
    -- =========================================================================
    -- BUTTERFLY TRACKING AND ADDRESS GENERATION
    -- =========================================================================
    
    signal butterfly_counter : integer range 0 to N_POINTS/2-1 := 0;
    signal current_group : integer range 0 to N_POINTS/2-1 := 0;
    signal current_position : integer range 0 to N_POINTS/2-1 := 0;
    signal butterfly_complete : std_logic := '0';
    
    -- Address generation
    signal x_addr : std_logic_vector(ADDR_WIDTH-1 downto 0) := (others => '0');
    signal y_addr : std_logic_vector(ADDR_WIDTH-1 downto 0) := (others => '0');
    
    -- =========================================================================
    -- PIPELINE CONTROL 
    -- =========================================================================
    
    type pipeline_stage_t is array(0 to TOTAL_PIPELINE-1) of std_logic;
    signal pipeline_valid : pipeline_stage_t := (others => '0');
    
    -- Write-back address pipeline (matches total pipeline latency)
    type addr_pipeline_t is array(0 to TOTAL_PIPELINE-1) of std_logic_vector(ADDR_WIDTH-1 downto 0);
    signal x_addr_pipe : addr_pipeline_t := (others => (others => '0'));
    signal y_addr_pipe : addr_pipeline_t := (others => (others => '0'));    
    -- =========================================================================
    -- INPUT LOADING CONTROL
    -- =========================================================================
    
    signal load_counter : integer range 0 to N_POINTS-1 := 0;
    signal load_complete : std_logic := '0';
    
    -- =========================================================================
    -- BRAM INTERFACE SIGNALS
    -- =========================================================================
    
    -- Address pipelines for 2-cycle BRAM latency compensation
    type addr_bram_pipeline_t is array(0 to 1) of std_logic_vector(ADDR_WIDTH-1 downto 0);
    signal x_addr_bram_pipe : addr_bram_pipeline_t := (others => (others => '0'));
    signal y_addr_bram_pipe : addr_bram_pipeline_t := (others => (others => '0'));
    
    -- BRAM data signals
    signal bram_read_data_x : std_logic_vector(2*DATA_WIDTH-1 downto 0);
    signal bram_read_data_y : std_logic_vector(2*DATA_WIDTH-1 downto 0);
    
    -- Copy control signals
    signal copy_counter : integer range 0 to N_POINTS-1 := 0;
    signal copy_complete : std_logic := '0';
    
    -- Ping-pong control (simplified - may not need this anymore)
--    signal ping_pong_sel : std_logic := '0';

-- =========================================================================
    -- PROCESS 10: INPUT LOADING CONTROL SIGNALS
    -- =========================================================================
        
    -- BRAM A write coordination signals (for Process 8)
    signal bram_a_write_enable : std_logic := '0';  -- Write enable during loading
    signal bram_a_write_addr   : std_logic_vector(ADDR_WIDTH-1 downto 0) := (others => '0');  -- Write address
    signal bram_a_write_data   : std_logic_vector(2*DATA_WIDTH-1 downto 0) := (others => '0');  -- Write data
    
    signal input_bram_en_delay1 : std_logic := '0';
    signal input_bram_en_delay2 : std_logic := '0';
    signal bitrev_addr_delay1   : std_logic_vector(ADDR_WIDTH-1 downto 0) := (others => '0');
    signal bitrev_addr_delay2   : std_logic_vector(ADDR_WIDTH-1 downto 0) := (others => '0');
    signal input_bram_en_internal : std_logic := '0';
    
      -- =========================================================================
    -- ADDITIONAL SIGNALS NEEDED IN ARCHITECTURE FOR PROCESS 8
    -- =========================================================================
    
    -- Process 8: Copy control delay registers (for BRAM read latency)
    signal copy_counter_delayed1 : integer range 0 to N_POINTS-1 := 0;
    signal copy_counter_delayed2 : integer range 0 to N_POINTS-1 := 0;
    
    -- Process 8: BRAM data for copy operations
    signal bram_copy_data : std_logic_vector(2*DATA_WIDTH-1 downto 0) := (others => '0');

begin

    -- =========================================================================
    -- COMPONENT INSTANTIATIONS
    -- =========================================================================
    
    -- Butterfly processing unit
    butterfly_inst: fft_butterfly
        generic map (
            DATA_WIDTH => DATA_WIDTH
        )
        port map (
            clk       => clk,
            rst       => rst,
            valid_in  => butterfly_valid_in,
            xr        => butterfly_xr,
            xi        => butterfly_xi,
            yr        => butterfly_yr,
            yi        => butterfly_yi,
            wr        => butterfly_wr,
            wi        => butterfly_wi,
            valid_out => butterfly_valid_out,
            out1r     => butterfly_out1r,
            out1i     => butterfly_out1i,
            out2r     => butterfly_out2r,
            out2i     => butterfly_out2i
        );
    
    -- Twiddle factor ROM
    twiddle_inst: twiddle_rom_8pt
        port map (
            clk     => clk,
            en      => twiddle_en,
            addr    => twiddle_addr_delayed,  -- Pipelined address for timing sync
            data_re => twiddle_re,
            data_im => twiddle_im
        );

    -- =========================================================================
    -- PROCESS IMPLEMENTATIONS WILL GO HERE
    -- =========================================================================
    
    -- =========================================================================
    -- PROCESS 1: MAIN FSM - High-level state control
    -- =========================================================================
    main_fsm_proc: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                state <= IDLE;
                current_stage <= 0;
                done <= '0';
                busy <= '0';
            else
                case state is
                    when IDLE =>
                        done <= '0';
                        busy <= '0';
                        current_stage <= 0;
                        
                        if start = '1' then
                            state <= LOAD_INPUT;
                            busy <= '1';  -- Processing begins
                        end if;
                    
                    when LOAD_INPUT =>
                        busy <= '1';
                        
                        if load_complete = '1' and copy_in_progress = '0' then
                            state <= PROCESSING;
                        end if;
                    
                    when PROCESSING =>
                        busy <= '1';
                        
                        if stage_complete = '1' then
                            if current_stage = NUM_STAGES - 1 then
                                -- All FFT stages complete, start copying results
                                state <= COPY_RESULTS;
                            else
                                current_stage <= current_stage + 1;
                            end if;
                        end if;
                    
                    when COPY_RESULTS =>
                        busy <= '1';
                        
                        -- Copy BRAM B results to BRAM C for magnitude module
                        if copy_complete = '1' then
                            state <= OUTPUT_READY;
                            results_ready <= '1';  -- Signal that BRAM C is populated
                        end if;
                    
                    when OUTPUT_READY =>
                        busy <= '1';  -- Still busy until magnitude acknowledges
                        results_ready <= '1';  -- Keep results_ready asserted
                        
                        -- Wait for magnitude module to finish reading BRAM C
                        if magnitude_done = '1' then
                            state <= DONE_STATE;
                            results_ready <= '0';  -- Clear results_ready
                        end if;
                    
                    
                    when DONE_STATE =>
                        busy <= '0';  
                        done <= '1';  
                        results_ready <= '0';  
                        state <= IDLE;  
                        
                end case;
            end if;
        end if;
    end process;

    -- =========================================================================
    -- PROCESS 3: STAGE PARAMETERS - Dynamic calculation for current stage
    -- Updates whenever current_stage changes (combinational for fast response)
    -- =========================================================================
    stage_params_proc: process(current_stage)
    begin
        -- Calculate stage parameters using radix-2 DIT FFT formulas
        -- These drive address generation and butterfly counting
        
        span <= 2**(current_stage + 1);        -- Distance between butterfly pairs
        half <= 2**current_stage;              -- Butterfly separation (half of span)
        groups <= N_POINTS / (2**(current_stage + 1));  -- Number of butterfly groups
        butterflies_per_stage <= N_POINTS / 2; -- Always N/2 butterflies per stage
        
    end process;

    -- =========================================================================
    -- PROCESS 4: BUTTERFLY COUNTER - Tracks progress within current stage
    -- Generates stage_complete signal for main FSM
    -- =========================================================================
    butterfly_counter_proc: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                butterfly_counter <= 0;
                stage_complete <= '0';
            else
                stage_complete <= '0';
                
                case state is
                    when IDLE | LOAD_INPUT =>
                        butterfly_counter <= 0;
                        
                    when PROCESSING =>
                        if butterfly_complete = '1' then
                            if butterfly_counter = butterflies_per_stage - 1 then
                                stage_complete <= '1';
                                butterfly_counter <= 0; 
                            else
                                butterfly_counter <= butterfly_counter + 1;
                            end if;
                        end if;
                        
                    when OUTPUT_READY | DONE_STATE =>
                        null;
                    when others =>
                        null; 
                end case;
            end if;
        end if;
    end process;

    -- =========================================================================
    -- PROCESS 5: ADDRESS GENERATION - Calculate butterfly pair addresses
    -- Generates x_addr, y_addr, and twiddle indices from stage parameters
    -- =========================================================================
    address_gen_proc: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                current_group <= 0;
                current_position <= 0;
                x_addr <= (others => '0');
                y_addr <= (others => '0');
                twiddle_addr_gen <= (others => '0');
            else
                if state = PROCESSING then
                    -- Calculate group and position within group from butterfly counter
                    -- These use the stage parameters (span, half) from stage_params_proc
                    current_group <= butterfly_counter / half;
                    current_position <= butterfly_counter mod half;
                    
                    -- Generate butterfly pair addresses using DIT algorithm
                    -- x_addr = group * span + position (top butterfly input)
                    -- y_addr = x_addr + half (bottom butterfly input)
                    x_addr <= std_logic_vector(to_unsigned(
                        (butterfly_counter / half) * span + (butterfly_counter mod half), 
                        ADDR_WIDTH));
                    y_addr <= std_logic_vector(to_unsigned(
                        (butterfly_counter / half) * span + (butterfly_counter mod half) + half, 
                        ADDR_WIDTH));
                    
                    -- Generate twiddle factor index
                    -- k = position * (N_POINTS / span) gives correct W^k for this butterfly
                    twiddle_addr_gen <= std_logic_vector(to_unsigned(
                        (butterfly_counter mod half) * (N_POINTS / span), 
                        ADDR_WIDTH));
                else
                    x_addr <= (others => '0');
                    y_addr <= (others => '0');
                    twiddle_addr_gen <= (others => '0');
                end if;
            end if;
        end if;
    end process;

    -- =========================================================================
    -- PROCESS 6: TWIDDLE ADDRESS PIPELINE - 1-cycle delay for BRAM sync
    -- Delays twiddle address to synchronize with 2-cycle BRAM read latency
    -- =========================================================================
    twiddle_addr_pipeline_proc: process(clk)
        constant TWIDDLE_ADDR_WIDTH : integer := ADDR_WIDTH - 1;  -- log2(N_POINTS/2) for twiddle ROM
    begin
        if rising_edge(clk) then
            if rst = '1' then
                twiddle_addr_delayed <= (others => '0');
                twiddle_en <= '0';
            else
                if state = PROCESSING then
                    -- This synchronizes twiddle ROM output with BRAM output
                    twiddle_addr_delayed <= twiddle_addr_gen(TWIDDLE_ADDR_WIDTH-1 downto 0);
                    twiddle_en <= '1';  -- Enable twiddle ROM
                else
                    -- Disable twiddle ROM when not processing
                    twiddle_addr_delayed <= (others => '0');
                    twiddle_en <= '0';
                end if;
            end if;
        end if;
    end process;

    -- =========================================================================
    -- PROCESS 7: PIPELINE CONTROL - Manages butterfly pipeline timing
    -- Tracks TOTAL_PIPELINE cycle latency and generates butterfly_complete
    -- =========================================================================
    pipeline_control_proc: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                pipeline_valid <= (others => '0');
                butterfly_valid_in <= '0';
                butterfly_complete <= '0';
            else
                -- Default: no butterfly completion
                butterfly_complete <= '0';
                
                case state is
                    when IDLE | LOAD_INPUT | OUTPUT_READY | DONE_STATE =>
                        pipeline_valid <= (others => '0');
                        butterfly_valid_in <= '0';
                        
                    when PROCESSING =>
                        -- Shift pipeline valid bits (models the delay through pipeline)
                        for i in TOTAL_PIPELINE-1 downto 1 loop
                            pipeline_valid(i) <= pipeline_valid(i-1);
                        end loop;
                        
                        -- Start new butterfly if not at end of stage (NO MORE BRAM CONFLICTS!)
                        if butterfly_counter < butterflies_per_stage then
                            pipeline_valid(0) <= '1';  -- New butterfly enters pipeline
                            butterfly_valid_in <= '1';  -- Signal to butterfly module
                        else
                            pipeline_valid(0) <= '0';  -- No new butterfly
                            butterfly_valid_in <= '0';
                        end if;
                        
                        -- Generate butterfly_complete when pipeline output is valid
                        if pipeline_valid(TOTAL_PIPELINE-1) = '1' then
                            butterfly_complete <= '1';  
                        end if;
                    when others => NULL;  
                end case;
            end if;
        end if;
    end process;

-- =========================================================================
-- PROCESS 8: BRAM_INTERFACE_PROC - Complete Implementation  
-- =========================================================================
bram_interface_proc: process(clk)
begin
    if rising_edge(clk) then
        if rst = '1' then
            
            -- BRAM A ports
            bram_a_addr_a <= (others => '0');
            bram_a_addr_b <= (others => '0');
            bram_a_din_a <= (others => '0');
            bram_a_din_b <= (others => '0');
            bram_a_we_a <= '0';
            bram_a_we_b <= '0';
            bram_a_en_a <= '0';
            bram_a_en_b <= '0';
            
            -- BRAM B ports
            bram_b_addr_a <= (others => '0');
            bram_b_addr_b <= (others => '0');
            bram_b_din_a <= (others => '0');
            bram_b_din_b <= (others => '0');
            bram_b_we_a <= '0';
            bram_b_we_b <= '0';
            bram_b_en_a <= '0';
            bram_b_en_b <= '0';
            
            -- BRAM C port
            bram_c_addr <= (others => '0');
            bram_c_din <= (others => '0');
            bram_c_we <= '0';
            bram_c_en <= '0';
            
            -- Data multiplexing signals
            bram_read_data_x <= (others => '0');
            bram_read_data_y <= (others => '0');
            bram_copy_data <= (others => '0');
            
        else
            bram_a_we_a <= '0';
            bram_a_we_b <= '0';
            bram_b_we_a <= '0';
            bram_b_we_b <= '0';
            bram_c_we <= '0';
            
            case state is
                when IDLE | OUTPUT_READY | DONE_STATE =>
                    -- Disable all BRAM access when not actively processing
                    bram_a_en_a <= '0';
                    bram_a_en_b <= '0';
                    bram_b_en_a <= '0';
                    bram_b_en_b <= '0';
                    bram_c_en <= '0';
                    
                when LOAD_INPUT =>
                    -- Process 10 handles input loading coordination
                    -- Use its synchronized outputs for BRAM A write
                    
                    -- BRAM A Port B: Write input data at bit-reversed addresses
                    bram_a_addr_b <= bram_a_write_addr;   
                    bram_a_din_b <= bram_a_write_data;     
                    bram_a_we_b <= bram_a_write_enable;    
                    bram_a_en_b <= bram_a_write_enable;    
                    
                    -- BRAM A Port A: Not used during input loading
                    bram_a_en_a <= '0';
                    
                    -- BRAM B: Not used during input loading
                    bram_b_en_a <= '0';
                    bram_b_en_b <= '0';
                    
                    -- BRAM C: Not used during input loading
                    bram_c_en <= '0';
                    
                when PROCESSING =>
                    -- FFT Processing: Ping-pong between BRAM A and BRAM B based on stage
                    
                    -- Determine ping-pong direction based on current stage
                    if (current_stage mod 2) = 0 then
                        -- === EVEN STAGES (0, 2, 4...): BRAM A → BRAM B ===
                        
                        -- Read butterfly inputs from BRAM A (both ports)
                        bram_a_addr_a <= x_addr_bram_pipe(1);  -- Read x data (2-cycle delayed address)
                        bram_a_addr_b <= y_addr_bram_pipe(1);  -- Read y data (2-cycle delayed address)
                        bram_a_en_a <= '1';   -- Enable Port A for reading
                        bram_a_en_b <= '1';   -- Enable Port B for reading  
                        bram_a_we_a <= '0';   -- Read mode (Port A)
                        bram_a_we_b <= '0';   -- Read mode (Port B)
                        
                        -- Write butterfly outputs to BRAM B (both ports)
                        bram_b_addr_a <= x_addr_pipe(TOTAL_PIPELINE-1);  -- Write out1 to x address
                        bram_b_addr_b <= y_addr_pipe(TOTAL_PIPELINE-1);  -- Write out2 to y address
                        bram_b_din_a <= std_logic_vector(butterfly_out1i) & std_logic_vector(butterfly_out1r);  -- Pack out1
                        bram_b_din_b <= std_logic_vector(butterfly_out2i) & std_logic_vector(butterfly_out2r);  -- Pack out2
                        bram_b_we_a <= butterfly_valid_out;  -- Write when butterfly outputs valid
                        bram_b_we_b <= butterfly_valid_out;  -- Write when butterfly outputs valid
                        bram_b_en_a <= '1';   -- Enable Port A
                        bram_b_en_b <= '1';   -- Enable Port B
                        
                        -- Data multiplexing: Route BRAM A outputs to butterfly inputs
                        bram_read_data_x <= bram_a_dout_a;  -- x data from BRAM A Port A
                        bram_read_data_y <= bram_a_dout_b;  -- y data from BRAM A Port B
                        
                    else
                        -- === ODD STAGES (1, 3, 5...): BRAM B → BRAM A ===
                        
                        -- Read butterfly inputs from BRAM B (both ports)
                        bram_b_addr_a <= x_addr_bram_pipe(1);  -- Read x data (2-cycle delayed address)
                        bram_b_addr_b <= y_addr_bram_pipe(1);  -- Read y data (2-cycle delayed address)
                        bram_b_en_a <= '1';   -- Enable Port A for reading
                        bram_b_en_b <= '1';   -- Enable Port B for reading
                        bram_b_we_a <= '0';   -- Read mode (Port A)
                        bram_b_we_b <= '0';   -- Read mode (Port B)
                        
                        -- Write butterfly outputs to BRAM A (both ports)
                        bram_a_addr_a <= x_addr_pipe(TOTAL_PIPELINE-1);  -- Write out1 to x address
                        bram_a_addr_b <= y_addr_pipe(TOTAL_PIPELINE-1);  -- Write out2 to y address
                        bram_a_din_a <= std_logic_vector(butterfly_out1i) & std_logic_vector(butterfly_out1r);  -- Pack out1 (Port A)
                        bram_a_din_b <= std_logic_vector(butterfly_out2i) & std_logic_vector(butterfly_out2r);  -- Pack out2 (Port B)
                        bram_a_we_a <= butterfly_valid_out;  -- Write when butterfly outputs valid
                        bram_a_we_b <= butterfly_valid_out;  -- Write when butterfly outputs valid
                        bram_a_en_a <= '1';   -- Enable Port A for writing
                        bram_a_en_b <= '1';   -- Enable Port B for writing
                        
                        -- Data multiplexing: Route BRAM B outputs to butterfly inputs
                        bram_read_data_x <= bram_b_dout_a;  -- x data from BRAM B Port A
                        bram_read_data_y <= bram_b_dout_b;  -- y data from BRAM B Port B
                        
                    end if;
                    
                    -- BRAM C: Not used during FFT processing
                    bram_c_en <= '0';
                    
                when COPY_RESULTS =>
                    -- Copy FFT results from final destination BRAM to BRAM C
                    -- 2-cycle BRAM read latency
                    
                    -- Determine which BRAM contains final results based on number of stages
                    if ((NUM_STAGES - 1) mod 2) = 0 then
                        -- Even number of stages: Final results in BRAM B
                        -- Read from BRAM B Port A
                        bram_b_addr_a <= std_logic_vector(to_unsigned(copy_counter, ADDR_WIDTH));
                        bram_b_en_a <= '1';   
                        bram_b_we_a <= '0'; 
                        bram_b_en_b <= '0';   
                        
                        bram_a_en_a <= '0';
                        bram_a_en_b <= '0';
                        
                        bram_copy_data <= bram_b_dout_a;
                        
                    else
                        -- Odd number of stages: Final results in BRAM A  
                        -- Read from BRAM A Port A
                        bram_a_addr_a <= std_logic_vector(to_unsigned(copy_counter, ADDR_WIDTH));
                        bram_a_en_a <= '1';  
                        bram_a_we_a <= '0';   
                        bram_a_en_b <= '0';   
                        
                        -- Disable BRAM B during copy
                        bram_b_en_a <= '0';
                        bram_b_en_b <= '0';
                        
                        -- Route BRAM A data for copying
                        bram_copy_data <= bram_a_dout_a;
                        
                    end if;
                    
                    -- BRAM C: Write with 2-cycle delayed addressing and enable
                    -- This aligns BRAM C write with when BRAM read data becomes valid
                    bram_c_addr <= std_logic_vector(to_unsigned(copy_counter_delayed2, ADDR_WIDTH));
                    bram_c_din <= bram_copy_data;  
                    bram_c_en <= '1';    
                    
                    -- Write enable logic: Only when magnitude not reading AND data is valid (2 cycles after read)
                    if magnitude_reading = '0' and copy_counter >= 2 then
                        bram_c_we <= '1'; 
                    else
                        bram_c_we <= '0';  -- Either magnitude reading or data not ready
                    end if;
                    
            end case;
        end if;
    end if;
end process;

-- =========================================================================
-- PROCESS 9: COPY_CONTROL_PROC - BRAM B to BRAM C transfer with delay registers
-- Copies FFT results to magnitude module buffer with conflict avoidance
-- =========================================================================
copy_control_proc: process(clk)
begin
    if rising_edge(clk) then
        if rst = '1' then
            copy_counter <= 0;
            copy_complete <= '0';
            copy_in_progress <= '0';
            
            -- Reset delay registers for Process 8 timing coordination
            copy_counter_delayed1 <= 0;
            copy_counter_delayed2 <= 0;
            
        else
            copy_complete <= '0';
            
            -- Update delay registers every cycle (for Process 8 BRAM timing)
            copy_counter_delayed1 <= copy_counter;
            copy_counter_delayed2 <= copy_counter_delayed1;
            
            case state is
                when COPY_RESULTS =>
                    copy_in_progress <= '1';  
                    
                    if magnitude_reading = '0' then
                        if copy_counter = N_POINTS - 1 then
                            -- Last word copied
                            copy_complete <= '1';
                            copy_counter <= 0;
                            copy_in_progress <= '0';  
                        else
                            -- Copy next word
                            copy_counter <= copy_counter + 1;
                        end if;
                    end if;
                    
                when OUTPUT_READY | DONE_STATE =>
                    -- Ensure copy_in_progress is cleared after copy phase
                    copy_in_progress <= '0';
                    
                when others =>
                    -- Reset copy control when not in copy-related states
                    copy_counter <= 0;
                    copy_in_progress <= '0';
                    
            end case;
        end if;
    end if;
end process;
-- =========================================================================
-- PROCESS 10: INPUT_LOAD_PROC - Input loading coordination with bit-reverse
-- Manages the 4-stage pipeline: counter → bitrev → input_bram → bram_a_write
-- =========================================================================
input_load_proc: process(clk)
    -- Delay registers for BRAM A write timing (2-cycle input BRAM latency)
begin
    if rising_edge(clk) then
        if rst = '1' then
            load_counter <= 0;
            load_complete <= '0';
            
            bitrev_valid_in <= '0';
            bitrev_addr_in <= (others => '0');
            
            input_bram_addr <= (others => '0');
            input_bram_en_internal <= '0';
            
            input_bram_en_delay1 <= '0';
            input_bram_en_delay2 <= '0';
            bitrev_addr_delay1 <= (others => '0');
            bitrev_addr_delay2 <= (others => '0');
            
            bram_a_write_enable <= '0';
            bram_a_write_addr <= (others => '0');
            bram_a_write_data <= (others => '0');
            
        else
            load_complete <= '0';
            
            -- === PIPELINE DELAY REGISTERS (for BRAM A write timing) ===
            -- Shift the delay registers every cycle to track timing
            input_bram_en_delay1 <= input_bram_en_internal;  -- Use internal signal
            input_bram_en_delay2 <= input_bram_en_delay1;
            bitrev_addr_delay1 <= bitrev_addr_out;
            bitrev_addr_delay2 <= bitrev_addr_delay1;
            
            case state is
                when IDLE =>
                    -- Reset loading controls when idle
                    load_counter <= 0;
                    bitrev_valid_in <= '0';
                    input_bram_en_internal <= '0';
                    
                when LOAD_INPUT =>
                    -- === STAGE 1: Sequential Counter → Bit-Reverse Pipe ===
                    if load_counter < N_POINTS then
                        -- Drive bit-reverse pipe with sequential address
                        bitrev_addr_in <= std_logic_vector(to_unsigned(load_counter, ADDR_WIDTH));
                        bitrev_valid_in <= '1';
                        
                        -- Increment counter for next cycle
                        load_counter <= load_counter + 1;
                    else
                        -- All addresses sent to bit-reverse pipe
                        bitrev_valid_in <= '0';
                    end if;
                    
                    -- === STAGE 3: Input BRAM Read (2 cycles after bit-reverse) ===
                    -- Use bit-reversed address to read input BRAM
                    if bitrev_valid_out = '1' then
                        input_bram_addr <= bitrev_addr_out;  -- Bit-reversed address
                        input_bram_en_internal <= '1';      -- Enable input BRAM read (internal)
                    else
                        input_bram_en_internal <= '0';
                    end if;
                    
                    -- === COMPLETION DETECTION ===
                    -- Load complete when:
                    -- 1. All sequential addresses sent to bit-reverse (load_counter = N_POINTS)
                    -- 2. Pipeline has drained (no more valid outputs from bit-reverse)
                    -- 3. Final BRAM write will complete in this cycle or next
                    
                    if load_counter >= N_POINTS and bitrev_valid_out = '0' then
                        -- All data through pipeline, loading complete
                        load_complete <= '1';
                    end if;
                    
                    -- === STAGE 4: BRAM A Write Coordination ===
                    -- Generate write signals for Process 8 (2 cycles after input_bram_en)
                    bram_a_write_enable <= input_bram_en_delay2;  -- Write enable (4 cycles after counter)
                    bram_a_write_addr <= bitrev_addr_delay2;      -- Write address (bit-reversed)
                    bram_a_write_data <= x"0000" & input_bram_dout; -- Pack real data (imag=0, real=data)
                    
                when others =>
                    -- Disable input loading when not in LOAD_INPUT state
                    bitrev_valid_in <= '0';
                    input_bram_en_internal <= '0';
                    
                    -- Clear BRAM A write signals  
                    bram_a_write_enable <= '0';
                    bram_a_write_addr <= (others => '0');
                    bram_a_write_data <= (others => '0');
                    
            end case;
        end if;
    end if;
end process;
-- =========================================================================
-- PROCESS 11: WRITEBACK_PIPELINE_PROC - Address pipeline management
-- Creates read pipelines (2-cycle) and write pipelines (TOTAL_PIPELINE-cycle)
-- for butterfly input/output timing coordination
-- =========================================================================
writeback_pipeline_proc: process(clk)
begin
    if rising_edge(clk) then
        if rst = '1' then
            -- Reset all pipeline arrays to zero addresses
            
            -- Read pipelines (2-cycle for BRAM read latency)
            for i in 0 to 1 loop
                x_addr_bram_pipe(i) <= (others => '0');
                y_addr_bram_pipe(i) <= (others => '0');
            end loop;
            
            -- Write pipelines (TOTAL_PIPELINE-cycle for butterfly latency)
            for i in 0 to TOTAL_PIPELINE-1 loop
                x_addr_pipe(i) <= (others => '0');
                y_addr_pipe(i) <= (others => '0');
            end loop;
            
        else
            case state is
                when PROCESSING =>
                    -- === READ PIPELINES (for butterfly inputs) ===
                    -- Shift read pipeline arrays (2 stages)
                    x_addr_bram_pipe(1) <= x_addr_bram_pipe(0);  -- Stage 1 → Stage 1 (output)
                    x_addr_bram_pipe(0) <= x_addr;               -- New address → Stage 0
                    
                    y_addr_bram_pipe(1) <= y_addr_bram_pipe(0);  -- Stage 1 → Stage 1 (output)  
                    y_addr_bram_pipe(0) <= y_addr;               -- New address → Stage 0
                    
                    -- === WRITE PIPELINES (for butterfly outputs) ===
                    -- Shift write pipeline arrays (TOTAL_PIPELINE stages)
                    for i in TOTAL_PIPELINE-1 downto 1 loop
                        x_addr_pipe(i) <= x_addr_pipe(i-1);      -- Shift each stage
                        y_addr_pipe(i) <= y_addr_pipe(i-1);      -- Shift each stage
                    end loop;
                    
                    -- Load new addresses into pipeline stage 0
                    x_addr_pipe(0) <= x_addr;                    -- New x address
                    y_addr_pipe(0) <= y_addr;                    -- New y address
                
                when others =>
                    null;  -- No action - preserve pipeline contents
                    
            end case;
        end if;
    end if;
end process;

-- X input (top butterfly): Unpack complex data from BRAM read
butterfly_xr <= signed(bram_read_data_x(DATA_WIDTH-1 downto 0));           -- Real part [15:0]
butterfly_xi <= signed(bram_read_data_x(2*DATA_WIDTH-1 downto DATA_WIDTH)); -- Imag part [31:16]

-- Y input (bottom butterfly): Unpack complex data from BRAM read  
butterfly_yr <= signed(bram_read_data_y(DATA_WIDTH-1 downto 0));           -- Real part [15:0]
butterfly_yi <= signed(bram_read_data_y(2*DATA_WIDTH-1 downto DATA_WIDTH)); -- Imag part [31:16]

-- Twiddle factor inputs: Connect to ROM outputs
butterfly_wr <= signed(twiddle_re);  -- Twiddle real part
butterfly_wi <= signed(twiddle_im);  -- Twiddle imaginary part

input_bram_en <= input_bram_en_internal;

end architecture behavioral;