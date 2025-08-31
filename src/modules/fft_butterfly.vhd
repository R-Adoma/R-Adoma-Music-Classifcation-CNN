library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity fft_butterfly is
    generic (
        DATA_WIDTH : integer := 16  -- Q1.15 format (Bit width of each real/imag sample)
    );
    port (
        clk: in std_logic; -- system clock
        rst: in std_logic; -- synchronous reset
        
        -- stage 1 inputs
        valid_in    : in std_logic; --pulses high to latch new data
        xr, xi      : in signed(DATA_WIDTH-1 downto 0);
        yr, yi      : in signed(DATA_WIDTH-1 downto 0);
        wr, wi      : in signed(DATA_WIDTH-1 downto 0);
        
        -- stage 4 outputs
        valid_out : out std_logic;   -- goes high 4 cycles after valid_in
        out1r     : out signed(DATA_WIDTH-1 downto 0);
        out1i     : out signed(DATA_WIDTH-1 downto 0);
        out2r     : out signed(DATA_WIDTH-1 downto 0);
        out2i     : out signed(DATA_WIDTH-1 downto 0)
    );
end entity;

architecture Behavioral of fft_butterfly is
    -- === Stage-1 registers ===
    signal xr_r1, xi_r1    : signed(DATA_WIDTH-1 downto 0);
    signal yr_r1, yi_r1    : signed(DATA_WIDTH-1 downto 0);
    signal wr_r1, wi_r1    : signed(DATA_WIDTH-1 downto 0);
    signal vld_r1          : std_logic;
    -- === Stage-2 registers ===
    signal xr_r2, xi_r2    : signed(DATA_WIDTH-1 downto 0);
    signal m_r_r2, m_i_r2: signed(2*DATA_WIDTH-1 downto 0); 
    signal vld_r2: std_logic;
    -- === Stage -2b registers ===
    signal xr_r2b, xi_r2b  : signed(DATA_WIDTH-1 downto 0);  
    signal m_r_s  : signed(DATA_WIDTH-1 downto 0);
    signal m_i_s  : signed(DATA_WIDTH-1 downto 0);
    signal vld_2b : std_logic;
    -- === Stage-3 registers ===
    signal add_r_r3, add_i_r3: signed(DATA_WIDTH downto 0);
    signal sub_r_r3, sub_i_r3 : signed(DATA_WIDTH downto 0);
    signal vld_r3             : std_logic;
    -- === Stage 4 registers ===
    signal out1r_r4, out1i_r4  : signed(DATA_WIDTH-1 downto 0);
    signal out2r_r4, out2i_r4  : signed(DATA_WIDTH-1 downto 0);
    signal vld_r4              : std_logic;
begin
    ----------------------------------------------------------------------------
    --Stage 1: sample inputs
    ----------------------------------------------------------------------------
    stage_1_proc: process(clk) is
    begin
        if rising_edge(clk) then
            if rst = '1' then
                -- reset all stage-1 registers to zero 
                xr_r1   <= (others => '0');
                xi_r1   <= (others => '0');
                yr_r1   <= (others => '0');
                yi_r1   <= (others => '0');
                wr_r1   <= (others => '0');
                wi_r1   <= (others => '0');
                vld_r1  <= '0';
            else
                --latch 6 signed inputs + valid flag
                xr_r1   <= xr;
                xi_r1   <= xi;
                yr_r1   <= yr;
                yi_r1   <= yi;
                wr_r1   <= wr;
                wi_r1   <= wi;
                vld_r1  <= valid_in;        
            end if;
        end if; 
    end process;
    ----------------------------------------------------------------------------
    -- Stage 2: do (yr + j.yi) * (wr + j.wi)
    ----------------------------------------------------------------------------
    stage_2_proc: process(clk) is
    begin
        if rising_edge(clk) then
            if rst = '1' then
                xr_r2  <= (others => '0');
                xi_r2  <= (others => '0');
                m_r_r2  <= (others => '0');
                m_i_r2  <= (others => '0');
                vld_r2  <= '0';
            else
                 -- Real part = yr*wr - yi*wi
                 m_r_r2  <= yr_r1 * wr_r1 - yi_r1 * wi_r1;
                 -- Imag part = yr*wi + yi*wr
                 m_i_r2  <= yr_r1 * wi_r1 + yi_r1 * wr_r1;
                 -- Propagate the valid flag
                 vld_r2  <= vld_r1;
                -- forward xr/xi into Stage 3
                xr_r2  <= xr_r1;
                xi_r2  <= xi_r1;               
            end if;
        end if;
    end process;
-------------------------------------------------------------------------------
-- Stage 2b: shift-right by 15, truncate back to 16 bits (Q1.15)
-------------------------------------------------------------------------------
stage_2b_proc: process(clk) is
begin
  if rising_edge(clk) then
    if rst = '1' then
      xr_r2b <= (others => '0');
      xi_r2b <= (others => '0');
      m_r_s  <= (others => '0');
      m_i_s  <= (others => '0');
      vld_2b <= '0';
    else
      xr_r2b <= xr_r2;
      xi_r2b <= xi_r2;
      -- Arithmetic shift-right by 15 to convert from Q17.15 → Q1.15,
      -- then truncate down to DATA_WIDTH bits.
      m_r_s  <= resize( shift_right(m_r_r2, 15), DATA_WIDTH );
      m_i_s  <= resize( shift_right(m_i_r2, 15), DATA_WIDTH );
      vld_2b <= vld_r2;
    end if;
  end if;
end process;
   ----------------------------------------------------------------------------
  -- Stage 3: add/subtract
  --   A_r = xr_r2b + m_r_s
  --   A_i = xi_r2b + m_i_s
  --   S_r = xr_r2b - m_r_s
  --   S_i = xi_r2b - m_i_s
  ----------------------------------------------------------------------------
stage_3_proc: process(clk) is 
begin
  if rising_edge(clk) then
    if rst = '1' then
      add_r_r3 <= (others => '0');
      add_i_r3 <= (others => '0');
      sub_r_r3 <= (others => '0');
      sub_i_r3 <= (others => '0');
      vld_r3   <= '0';
    else
      add_r_r3 <= resize(xr_r2b, DATA_WIDTH+1) + resize(m_r_s, DATA_WIDTH+1);
      add_i_r3 <= resize(xi_r2b, DATA_WIDTH+1) + resize(m_i_s, DATA_WIDTH+1);
      sub_r_r3 <= resize(xr_r2b, DATA_WIDTH+1) - resize(m_r_s, DATA_WIDTH+1);
      sub_i_r3 <= resize(xi_r2b, DATA_WIDTH+1) - resize(m_i_s, DATA_WIDTH+1);
      vld_r3   <= vld_2b;
    end if;
  end if;
end process;
-------------------------------------------------------------------------------
-- Stage 4: single-bit shift → saturate → truncate → register outputs
-------------------------------------------------------------------------------
stage_4_proc: process(clk) is
  -- 16-bit full-scale limits for Q1.15 (after divide-by-2)
  constant MAX_POS_16 : integer := 2**(DATA_WIDTH-1) - 1;  -- 32767
  constant MAX_NEG_16 : integer := -2**(DATA_WIDTH-1);     -- -32768
  variable tmp : signed(DATA_WIDTH downto 0);  -- 17-bit intermediate
  variable final_val : integer;
begin
  if rising_edge(clk) then
    if rst = '1' then
      out1r_r4 <= (others => '0');
      out1i_r4 <= (others => '0');
      out2r_r4 <= (others => '0');
      out2i_r4 <= (others => '0');
      vld_r4   <= '0';

    elsif vld_r3 = '1' then
      -- Process out1r (add_r_r3)
      tmp := shift_right(add_r_r3, 1);  -- Divide by 2
      final_val := to_integer(tmp);
      if final_val > MAX_POS_16 then
        out1r_r4 <= to_signed(MAX_POS_16, DATA_WIDTH);
      elsif final_val < MAX_NEG_16 then
        out1r_r4 <= to_signed(MAX_NEG_16, DATA_WIDTH);
      else
        out1r_r4 <= to_signed(final_val, DATA_WIDTH);
      end if;

      -- Process out1i (add_i_r3)
      tmp := shift_right(add_i_r3, 1);
      final_val := to_integer(tmp);
      if final_val > MAX_POS_16 then
        out1i_r4 <= to_signed(MAX_POS_16, DATA_WIDTH);
      elsif final_val < MAX_NEG_16 then
        out1i_r4 <= to_signed(MAX_NEG_16, DATA_WIDTH);
      else
        out1i_r4 <= to_signed(final_val, DATA_WIDTH);
      end if;

      -- Process out2r (sub_r_r3)
      tmp := shift_right(sub_r_r3, 1);
      final_val := to_integer(tmp);
      if final_val > MAX_POS_16 then
        out2r_r4 <= to_signed(MAX_POS_16, DATA_WIDTH);
      elsif final_val < MAX_NEG_16 then
        out2r_r4 <= to_signed(MAX_NEG_16, DATA_WIDTH);
      else
        out2r_r4 <= to_signed(final_val, DATA_WIDTH);
      end if;

      -- Process out2i (sub_i_r3)
      tmp := shift_right(sub_i_r3, 1);
      final_val := to_integer(tmp);
      if final_val > MAX_POS_16 then
        out2i_r4 <= to_signed(MAX_POS_16, DATA_WIDTH);
      elsif final_val < MAX_NEG_16 then
        out2i_r4 <= to_signed(MAX_NEG_16, DATA_WIDTH);
      else
        out2i_r4 <= to_signed(final_val, DATA_WIDTH);
      end if;

      vld_r4 <= '1';
    else
      vld_r4 <= '0';
    end if;
  end if;
end process;

  ----------------------------------------------------------------------------
  -- Output port mappings
  ----------------------------------------------------------------------------
  valid_out <= vld_r4;
  out1r     <= out1r_r4;
  out1i     <= out1i_r4;
  out2r     <= out2r_r4;
  out2i     <= out2i_r4;
end architecture;