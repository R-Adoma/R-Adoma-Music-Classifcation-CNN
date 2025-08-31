library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity audio_buffer is
    generic (
        DATA_WIDTH : integer := 16;
        FRAME_SIZE : integer := 2048
    );
    port (
        clk         : in  std_logic;
        rst         : in  std_logic;
        
        -- Input interface (from Zynq PS)
        valid_in    : in  std_logic;
        sample_in   : in  signed(DATA_WIDTH-1 downto 0);
        ready_out   : out std_logic;
        
        -- Output interface (to Hann window module)
        stream_valid      : out std_logic;
        stream_data       : out signed(DATA_WIDTH-1 downto 0);
        stream_frame_start : out std_logic;
        stream_ready      : in  std_logic := '1';  -- Back-pressure from Hann module
        
        -- Status signals
        frame_complete    : out std_logic;
        processing_busy   : out std_logic
    );
end entity;

architecture Behavioral of audio_buffer is
    -- Frame storage
    type frame_buffer is array (0 to FRAME_SIZE-1) of signed(DATA_WIDTH-1 downto 0);
    signal buffer_mem : frame_buffer := (others => (others => '0'));
    
    -- State machine
    type state_t is (IDLE, COLLECTING, STREAMING);
    signal state : state_t := IDLE;
    
    -- Counters
    signal write_ptr : integer range 0 to FRAME_SIZE-1 := 0;
    signal read_ptr  : integer range 0 to FRAME_SIZE-1 := 0;
    
    -- Control signals
    signal frame_complete_int : std_logic := '0';
    signal stream_valid_int : std_logic := '0';
    signal stream_frame_start_int : std_logic := '0';

begin

    main_proc: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                state <= IDLE;
                write_ptr <= 0;
                read_ptr <= 0;
                stream_valid_int <= '0';
                stream_frame_start_int <= '0';
                frame_complete_int <= '0';
            else
                -- Default values
                stream_frame_start_int <= '0';
                frame_complete_int <= '0';
                
                case state is
                    when IDLE =>
                        stream_valid_int <= '0';
                        if valid_in = '1' then
                            buffer_mem(0) <= sample_in;
                            write_ptr <= 1;
                            read_ptr <= 0;  -- Reset read pointer
                            state <= COLLECTING;
                        end if;
                    
                    when COLLECTING =>
                        stream_valid_int <= '0';
                        if valid_in = '1' then
                            buffer_mem(write_ptr) <= sample_in;
                            if write_ptr = FRAME_SIZE - 1 then
                                frame_complete_int <= '1';
                                read_ptr <= 0;
                                stream_frame_start_int <= '1';  -- Signal frame start
                                stream_valid_int <= '1';       -- Start streaming immediately
                                state <= STREAMING;
                            else
                                write_ptr <= write_ptr + 1;
                            end if;
                        end if;
                    
                    when STREAMING =>
                        stream_valid_int <= '1';
                        
                        -- Clear frame start after first cycle
                        if read_ptr > 0 then
                            stream_frame_start_int <= '0';
                        end if;
                        
                        -- Advance only when downstream is ready
                        if stream_ready = '1' then
                            if read_ptr = FRAME_SIZE - 1 then
                                write_ptr <= 0;
                                stream_valid_int <= '0';
                                state <= IDLE;
                            else
                                read_ptr <= read_ptr + 1;
                            end if;
                        end if;
                end case;
            end if;
        end if;
    end process;

    -- Output assignments
    stream_data <= buffer_mem(read_ptr);
    stream_valid <= stream_valid_int;
    stream_frame_start <= stream_frame_start_int;
    frame_complete <= frame_complete_int;
    ready_out <= '1' when (state = IDLE or state = COLLECTING) else '0';
    processing_busy <= '1' when (state = STREAMING) else '0';

end architecture;