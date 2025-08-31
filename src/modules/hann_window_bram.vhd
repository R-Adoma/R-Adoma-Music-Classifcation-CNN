library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity hann_window_pipelined is
    generic (
        DATA_WIDTH : integer := 16;
        FRAME_SIZE : integer := 2048
    );
    port (
        clk         : in  std_logic;
        rst         : in  std_logic;
        
        -- Input stream interface
        valid_in    : in  std_logic;
        sample_in   : in  signed(DATA_WIDTH-1 downto 0);
        
        -- Output stream interface (for debug)
        valid_out   : out std_logic;
        sample_out  : out signed(DATA_WIDTH-1 downto 0);
        
        -- Frame synchronization
        frame_start : in  std_logic;  
        frame_end   : out std_logic;  
        
        -- ROM Interface for coefficient reading (1-cycle delay)
        rom_addr    : out std_logic_vector(10 downto 0);
        rom_data    : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        
        -- BRAM Interface for output writing
        bram_we     : out std_logic;
        bram_addr   : out std_logic_vector(10 downto 0);
        bram_data   : out std_logic_vector(DATA_WIDTH-1 downto 0)
    );
end entity;

architecture Behavioral of hann_window_pipelined is
    
    -- Address counters
    signal input_counter  : unsigned(10 downto 0) := (others => '0');
    signal output_counter : unsigned(10 downto 0) := (others => '0');
    signal output_active  : std_logic := '0';
    
    -- 2-Stage Pipeline
    -- Stage 1: Input capture, ROM address generation
    signal stage1_valid    : std_logic := '0';
    signal stage1_sample   : signed(DATA_WIDTH-1 downto 0) := (others => '0');
    signal stage1_addr     : unsigned(10 downto 0) := (others => '0');
    signal stage1_is_last  : std_logic := '0';
    
    -- Stage 2: Multiply with ROM coefficient, prepare BRAM write
    signal stage2_valid    : std_logic := '0';
    signal stage2_result   : signed(DATA_WIDTH-1 downto 0) := (others => '0');
    signal stage2_is_last  : std_logic := '0';
    
    -- Stage 3: BRAM write 
    signal stage3_valid    : std_logic := '0';
    signal stage3_result   : signed(DATA_WIDTH-1 downto 0) := (others => '0');
    signal stage3_is_last  : std_logic := '0';
    
    -- Pipeline control
    signal pipeline_enable : std_logic;

begin

    pipeline_enable <= valid_in;

    ----------------------------------------------------------------------------
    -- Input Counter Management
    -- Simple counter that wraps at FRAME_SIZE
    ----------------------------------------------------------------------------
    input_counter_proc: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                input_counter <= (others => '0');
            elsif valid_in = '1' then
                if input_counter = FRAME_SIZE - 1 then
                    input_counter <= (others => '0');
                else
                    input_counter <= input_counter + 1;
                end if;
            end if;
        end if;
    end process;

    ----------------------------------------------------------------------------
    -- Output Frame Control
    -- Manages BRAM write addressing (follows pipeline delay)
    ----------------------------------------------------------------------------
    output_frame_proc: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                output_counter <= (others => '0');
                output_active  <= '0';
            else
                if stage1_valid = '1' and output_active = '0' then
                    output_counter <= (others => '0');
                    output_active  <= '1';
                elsif output_active = '1' and stage3_valid = '1' then
                    if output_counter = FRAME_SIZE - 1 then
                        output_counter <= (others => '0');
                        output_active  <= '0';
                    else
                        output_counter <= output_counter + 1;
                    end if;
                end if;
            end if;
        end if;
    end process;

    ----------------------------------------------------------------------------
    -- ROM Interface
    -- Address ROM with current input counter for 1-cycle delay compensation
    ----------------------------------------------------------------------------
    rom_addr <= std_logic_vector(input_counter);

    ----------------------------------------------------------------------------
    -- Stage 1: Input Sample Capture and ROM Address Generation
    -- Captures input samples and sets up ROM read for next cycle
    ----------------------------------------------------------------------------
    stage1_proc: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                stage1_valid   <= '0';
                stage1_sample  <= (others => '0');
                stage1_addr    <= (others => '0');
                stage1_is_last <= '0';
            else
                -- Capture input and prepare for next stage
                stage1_valid  <= pipeline_enable;
                stage1_sample <= sample_in;
                stage1_addr   <= input_counter;
                
                -- Mark last sample of frame
                if pipeline_enable = '1' and input_counter = FRAME_SIZE - 1 then
                    stage1_is_last <= '1';
                else
                    stage1_is_last <= '0';
                end if;
            end if;
        end if;
    end process;

    ----------------------------------------------------------------------------
    -- Stage 2: Windowing Multiplication and BRAM Write Preparation
    -- Multiplies sample with ROM coefficient and prepares BRAM write
    ----------------------------------------------------------------------------
    stage2_proc: process(clk)
        variable mult_result : signed(2*DATA_WIDTH-1 downto 0);
        variable rom_coeff   : signed(DATA_WIDTH-1 downto 0);
    begin
        if rising_edge(clk) then
            if rst = '1' then
                stage2_valid   <= '0';
                stage2_result  <= (others => '0');
                stage2_is_last <= '0';
            else
                -- Pipeline stage 2: multiply and prepare output
                stage2_valid   <= stage1_valid;
                stage2_is_last <= stage1_is_last;
                
                if stage1_valid = '1' then 
                    -- Convert ROM data to signed coefficient
                    rom_coeff := signed(rom_data);
                    
                    -- Multiply sample by Hann coefficient
                    mult_result := stage1_sample * rom_coeff;
                    
                    -- Scale result back to original width (Q1.15 format)
                    stage2_result <= resize(shift_right(mult_result, 15), DATA_WIDTH);
                else
                    stage2_result <= (others => '0');
                end if;
            end if;
        end if;
    end process;

    ----------------------------------------------------------------------------
    -- Stage 3: BRAM Write
    -- Delays the result by one cycle to ensure proper timing
    ----------------------------------------------------------------------------
    stage3_proc: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                stage3_valid   <= '0';
                stage3_result  <= (others => '0');
                stage3_is_last <= '0';
            else
                -- Simply pass through stage2 results with 1-cycle delay
                stage3_valid   <= stage2_valid;
                stage3_result  <= stage2_result;
                stage3_is_last <= stage2_is_last;
            end if;
        end if;
    end process;

    ----------------------------------------------------------------------------
    -- BRAM Write Interface
    -- Writes processed samples to BRAM using stage3 
    ----------------------------------------------------------------------------
    bram_we   <= stage3_valid;  -- Write enable when we have valid output
    bram_addr <= std_logic_vector(output_counter);
    bram_data <= std_logic_vector(stage3_result);  

    ----------------------------------------------------------------------------
    -- Output Assignments 
    ----------------------------------------------------------------------------
    valid_out  <= stage3_valid;
    sample_out <= stage3_result;
    frame_end  <= stage3_valid and stage3_is_last;

end architecture;