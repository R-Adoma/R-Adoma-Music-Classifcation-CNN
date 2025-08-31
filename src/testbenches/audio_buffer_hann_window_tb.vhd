library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use ieee.std_logic_textio.all;

entity audio_hann_tb is
end entity;

architecture sim of audio_hann_tb is

    -- Test parameters
    constant DATA_WIDTH : integer := 16;
    constant FRAME_SIZE : integer := 8;  -- Small for testing
    constant CLK_PERIOD : time := 10 ns;

    -- Clock and reset
    signal clk : std_logic := '0';
    signal rst : std_logic := '1';
    
    -- Audio buffer signals
    signal audio_valid_in     : std_logic := '0';
    signal audio_sample_in    : signed(DATA_WIDTH-1 downto 0) := (others => '0');
    signal audio_ready_out    : std_logic;
    signal audio_frame_complete : std_logic;
    signal audio_processing_busy : std_logic;
    
    -- Interface between audio buffer and Hann window
    signal stream_valid       : std_logic;
    signal stream_data        : signed(DATA_WIDTH-1 downto 0);
    signal stream_frame_start : std_logic;
    signal stream_ready       : std_logic := '1';
    
    -- Hann window signals
    signal hann_valid_out     : std_logic;
    signal hann_sample_out    : signed(DATA_WIDTH-1 downto 0);
    signal hann_frame_end     : std_logic;
    
    -- ROM interface (simplified for testing)
    signal rom_addr           : std_logic_vector(10 downto 0);
    signal rom_data           : std_logic_vector(DATA_WIDTH-1 downto 0);
    
    -- BRAM interface
    signal bram_we            : std_logic;
    signal bram_addr          : std_logic_vector(10 downto 0);
    signal bram_data          : std_logic_vector(DATA_WIDTH-1 downto 0);
    
    -- Test control
    signal test_done : boolean := false;
    
    -- Test data
    type test_frame_t is array (0 to FRAME_SIZE-1) of integer;
    signal test_frame1 : test_frame_t := (1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000);
    
    -- Simple ROM data (approximated Hann coefficients scaled to 16-bit)
    type rom_array_t is array (0 to FRAME_SIZE-1) of integer;
    signal rom_coeffs : rom_array_t := (0, 9512, 25080, 32767, 25080, 9512, 0, 0);

begin

    -- Clock generation
    clk <= not clk after CLK_PERIOD/2 when not test_done;

    -- Audio Buffer instantiation
    audio_buf: entity work.audio_buffer
        generic map (
            DATA_WIDTH => DATA_WIDTH,
            FRAME_SIZE => FRAME_SIZE
        )
        port map (
            clk                => clk,
            rst                => rst,
            valid_in           => audio_valid_in,
            sample_in          => audio_sample_in,
            ready_out          => audio_ready_out,
            stream_valid       => stream_valid,
            stream_data        => stream_data,
            stream_frame_start => stream_frame_start,
            stream_ready       => stream_ready,
            frame_complete     => audio_frame_complete,
            processing_busy    => audio_processing_busy
        );

    -- Hann Window instantiation
    hann_win: entity work.hann_window_pipelined
        generic map (
            DATA_WIDTH => DATA_WIDTH,
            FRAME_SIZE => FRAME_SIZE
        )
        port map (
            clk         => clk,
            rst         => rst,
            valid_in    => stream_valid,
            sample_in   => stream_data,
            valid_out   => hann_valid_out,
            sample_out  => hann_sample_out,
            frame_start => stream_frame_start,
            frame_end   => hann_frame_end,
            rom_addr    => rom_addr,
            rom_data    => rom_data,
            bram_we     => bram_we,
            bram_addr   => bram_addr,
            bram_data   => bram_data
        );

    -- Simple ROM simulation
    rom_proc: process(clk)
        variable addr_int : integer;
    begin
        if rising_edge(clk) then
            addr_int := to_integer(unsigned(rom_addr));
            if addr_int < FRAME_SIZE then
                rom_data <= std_logic_vector(to_signed(rom_coeffs(addr_int), DATA_WIDTH));
            else
                rom_data <= (others => '0');
            end if;
        end if;
    end process;

    -- Test stimulus
    stimulus_proc: process
    begin
        -- Reset
        rst <= '1';
        audio_valid_in <= '0';
        audio_sample_in <= (others => '0');
        stream_ready <= '1';
        
        wait for 5 * CLK_PERIOD;
        rst <= '0';
        wait for 2 * CLK_PERIOD;
        
        report "=== Starting Audio Buffer + Hann Window Test ===";
        
        -- Send test frame to audio buffer
        report "--- Sending Frame to Audio Buffer ---";
        for i in 0 to FRAME_SIZE-1 loop
            wait until rising_edge(clk);
            audio_valid_in <= '1';
            audio_sample_in <= to_signed(test_frame1(i), DATA_WIDTH);
            report "Sending sample " & integer'image(i) & ": " & integer'image(test_frame1(i));
        end loop;
        
        -- Stop sending
        wait until rising_edge(clk);
        audio_valid_in <= '0';
        audio_sample_in <= (others => '0');
        
        report "--- Waiting for Audio Buffer to start streaming ---";
        
        -- Wait for frame complete signal
        wait until audio_frame_complete = '1';
        report "Audio buffer frame complete detected!";
        
        -- Wait for streaming to start
        wait until stream_valid = '1';
        report "Audio buffer started streaming!";
        
        -- Wait for processing to complete
        wait until hann_frame_end = '1';
        report "Hann window processing complete!";
        
        wait for 10 * CLK_PERIOD;
        
        report "=== Test Complete ===";
        test_done <= true;
        wait;
    end process;

    -- Enhanced audio buffer state monitor
    audio_state_monitor: process(clk)
        variable sample_count : integer := 0;
        variable prev_stream_valid : std_logic := '0';
    begin
        if rising_edge(clk) and not test_done then
            -- Monitor state changes
            if audio_processing_busy = '1' and prev_stream_valid = '0' and stream_valid = '1' then
                report "*** Audio Buffer Started Streaming ***";
                sample_count := 0;
            end if;
            
            if stream_frame_start = '1' then
                report "*** Audio Buffer Frame Start Signal ***";
            end if;
            
            if stream_valid = '1' then
                report "Audio Buffer Stream Output " & integer'image(sample_count) & 
                       ": " & integer'image(to_integer(stream_data));
                sample_count := sample_count + 1;
            end if;
            
            if audio_processing_busy = '1' and stream_valid = '0' and prev_stream_valid = '1' then
                report "*** Audio Buffer Finished Streaming ***";
            end if;
            
            prev_stream_valid := stream_valid;
        end if;
    end process;

    -- Monitor Hann window output
    hann_monitor: process(clk)
        variable hann_count : integer := 0;
        variable expected_result : integer;
        variable actual_result : integer;
    begin
        if rising_edge(clk) and not test_done then
            if hann_valid_out = '1' then
                actual_result := to_integer(hann_sample_out);
                -- Calculate expected result: input * coefficient / 2^15
                if hann_count < FRAME_SIZE then
                    expected_result := (test_frame1(hann_count) * rom_coeffs(hann_count)) / 32768;
                    
                    report "Hann Output " & integer'image(hann_count) & 
                           ": Got " & integer'image(actual_result) &
                           ", Expected ~" & integer'image(expected_result) &
                           " (Input=" & integer'image(test_frame1(hann_count)) &
                           ", Coeff=" & integer'image(rom_coeffs(hann_count)) & ")";
                end if;
                
                hann_count := hann_count + 1;
            end if;
            
            if hann_frame_end = '1' then
                report "*** Hann Window Frame Complete ***";
                hann_count := 0;
            end if;
        end if;
    end process;

    -- BRAM write monitor
    bram_monitor: process(clk)
    begin
        if rising_edge(clk) and not test_done then
            if bram_we = '1' then
                report "BRAM Write[" & integer'image(to_integer(unsigned(bram_addr))) & 
                       "] = " & integer'image(to_integer(signed(bram_data)));
            end if;
        end if;
    end process;

    -- Status monitor
    status_monitor: process(clk)
    begin
        if rising_edge(clk) and not test_done then
            if audio_frame_complete = '1' then
                report "### Audio Buffer Frame Complete Signal ###";
            end if;
            
            if audio_processing_busy = '1' then
            end if;
        end if;
    end process;

end architecture;