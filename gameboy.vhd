library IEEE;  
use IEEE.STD_LOGIC_1164.ALL;  
--use IEEE.STD_LOGIC_ARITH.ALL;  
--use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity gameboy is  
  port(CLOCK_50  : in std_logic;
       VGA_R : out std_logic_VECTOR (7 downto 0);
       VGA_G : out std_logic_VECTOR (7 downto 0);
       VGA_B : out std_logic_VECTOR (7 downto 0);
       VGA_HS    : out std_logic;
       VGA_VS   : out std_logic;
		 VGA_CLK : out std_logic;
		 VGA_BLANK_N : out std_logic;
		 VGA_SYNC_N : out std_logic;
		 LEDR : out std_logic_vector(9 downto 0));
end gameboy;

architecture Behavioral of gameboy is

signal clk25              : std_logic;  
signal outbyte : std_logic_vector (7 downto 0);
signal nullsig :  STD_LOGIC_VECTOR (7 DOWNTO 0);
	signal myrow     : integer;
	signal col     : integer;
	signal dispen  : std_logic;
component memoryfirst
	PORT
	(
		data		: IN STD_LOGIC_VECTOR (7 DOWNTO 0);
		rdaddress		: IN STD_LOGIC_VECTOR (15 DOWNTO 0);
		rdclock		: IN STD_LOGIC ;
		wraddress		: IN STD_LOGIC_VECTOR (15 DOWNTO 0);
		wrclock		: IN STD_LOGIC  := '1';
		wren		: IN STD_LOGIC  := '0';
		q		: OUT STD_LOGIC_VECTOR (7 DOWNTO 0)
	);
end component;

COMPONENT vga_controller
	GENERIC(
    h_pulse  :  INTEGER   := 208;   --horiztonal sync pulse width in pixels
    h_bp     :  INTEGER   := 336;   --horiztonal back porch width in pixels
    h_pixels :  INTEGER   := 1920;  --horiztonal display width in pixels
    h_fp     :  INTEGER   := 128;   --horiztonal front porch width in pixels
    h_pol    :  STD_LOGIC := '0';   --horizontal sync pulse polarity (1 = positive, 0 = negative)
    v_pulse  :  INTEGER   := 3;     --vertical sync pulse width in rows
    v_bp     :  INTEGER   := 38;    --vertical back porch width in rows
    v_pixels :  INTEGER   := 1200;  --vertical display width in rows
    v_fp     :  INTEGER   := 1;     --vertical front porch width in rows
    v_pol    :  STD_LOGIC := '1');  --vertical sync pulse polarity (1 = positive, 0 = negative)
	PORT(
		pixel_clk : IN std_logic;
		reset_n : IN std_logic;          
		h_sync : OUT std_logic;
		v_sync : OUT std_logic;
		disp_ena : OUT std_logic;
		column : OUT INTEGER;
		row : OUT INTEGER
		);
	END COMPONENT;

begin
VGA_CLK <= clk25;
VGA_BLANK_N <='1' ;
VGA_SYNC_N <= '0';
-- generate a 25Mhz clock
process (CLOCK_50)  
begin
  
  
  if CLOCK_50'event and CLOCK_50='1' then
    if (clk25 = '0') then
      clk25 <= '1';
    else
      clk25 <= '0';
    end if;
  end if;
end process;

process (clk25)  
begin  
  LEDR (7 downto 0) <= outbyte;
  if clk25'event and clk25 = '1' then
    if dispen='1' then

     --here you paint!!
      VGA_R <= outbyte;
       VGA_G <= std_LOGIC_VECTOR(to_unsigned(col,8));
       VGA_B <= std_LOGIC_VECTOR(to_unsigned(myrow,8));

    else
       VGA_R <= "00000000";
       VGA_G <="00000000";
       VGA_B<= "00000000";
    end if;
  end if;
end process;



memoryfirst_inst : memoryfirst PORT MAP (
		data	 => "00000000",
		rdaddress	 => std_LOGIC_VECTOR(to_unsigned(myrow,16)),
		rdclock	 => clk25,
		wraddress	 => "0000000000000000",
		wrclock	 => '0',
		wren	 => '0',
		q	 => outbyte
		
	);
	
	
Inst_vga_controller: vga_controller   
 GENERIC MAP (
    h_pulse  => 96,   --horiztonal sync pulse width in pixels
    h_bp     => 48,   --horiztonal back porch width in pixels
    h_pixels => 640,  --horiztonal display width in pixels
    h_fp     => 16,   --horiztonal front porch width in pixels
    h_pol    => '0',   --horizontal sync pulse polarity (1 = positive, 0 = negative)
    v_pulse  => 2,     --vertical sync pulse width in rows
    v_bp     => 33,    --vertical back porch width in rows
    v_pixels => 480,  --vertical display width in rows
    v_fp     => 10,     --vertical front porch width in rows
    v_pol    => '0')
PORT MAP(
		pixel_clk => clk25,
		reset_n => '1',
		h_sync => VGA_HS ,
		v_sync => VGA_VS,
		disp_ena => dispen,
		column => col,
		row => myrow
	);


end Behavioral;  