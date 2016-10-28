library IEEE;  
use IEEE.STD_LOGIC_1164.ALL;  
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
		 LEDR : out std_logic_vector(9 downto 0);
		 KEY : in std_logic_vector(3 downto 0));
end gameboy;

architecture Behavioral of gameboy is

signal clk25              : std_logic;  
signal clk12              : std_logic;  
signal clk50              : std_logic;
signal reset   : std_logic;
signal ahrst   : std_logic;
signal outbyte : std_logic_vector (7 downto 0);
signal memaddr : std_logic_vector (15 downto 0);
signal inbyte : std_logic_vector (7 downto 0);
signal memwraddr : std_logic_vector (15 downto 0);
signal pixel : std_logic_vector (1 downto 0);
signal row     : integer range 0 to 1000;
signal col     : integer range 0 to 1000;
signal row_mul : integer range 0 to 1000;
signal col_mul : integer range 0 to 1000;
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
		column : OUT INTEGER range 0 to 1000;
		row : OUT INTEGER range 0 to 1000
		);
	END COMPONENT;
	
COMPONENT pll
	PORT
	(
		refclk		:	 IN STD_LOGIC;
		rst		:	 IN STD_LOGIC;
		outclk_0		:	 OUT STD_LOGIC;
		outclk_1		:	 OUT STD_LOGIC;
		outclk_2		:	 OUT STD_LOGIC
	);
END COMPONENT;

COMPONENT tilemap is
  generic(xoffset : integer := 100;
          yoffset : integer := 0;
			 screen_width : integer := 160;
			 screen_height : integer := 144);
  port(clk     : in std_logic;
       rst     : in std_logic;
		 memaddr : out std_logic_vector(15 downto 0);
		 memdat  : in std_logic_vector(7 downto 0);
		 xpos    : in integer range 0 to 1000;
		 ypos    : in integer range 0 to 1000;
		 pixel   : out std_logic_vector(1 downto 0)
		 );
END COMPONENT;

begin
VGA_CLK <= clk25;
VGA_BLANK_N <='1' ;
VGA_SYNC_N <= '0';
reset <= KEY(0);
ahrst <= not reset;
col_mul <= col/2;
row_mul <= row/2;

process (clk25)  
begin  
  LEDR (7 downto 0) <= outbyte;
  if rising_edge(clk25) then
    if dispen='1' then

      --here you paint!!
       VGA_R <= pixel & pixel & pixel & pixel;
       VGA_G <= pixel & pixel & pixel & pixel;
       VGA_B <= pixel & pixel & pixel & pixel;

    else
       VGA_R <= "00000000";
       VGA_G <="00000000";
       VGA_B <= "00000000";
    end if;
  end if;
end process;

process (clk12)
variable counter : unsigned(31 downto 0);
begin
  if reset = '0' then
    counter := (others => '0');
  elsif rising_edge(clk12) then
    counter := counter + 1;
	 inbyte <= std_logic_vector(counter(31 downto 24));
	 memwraddr <= x"FF43";
  end if;
end process;

pll_inst : pll PORT MAP (
  refclk => CLOCK_50,
  rst => ahrst,
  outclk_0 => clk50,
  outclk_1 => clk25,
  outclk_2 => clk12
);

tilemap_inst : tilemap PORT MAP (
  clk => clk12,
  rst => reset,
  memaddr => memaddr,
  memdat => outbyte,
  xpos => col_mul,
  ypos => row_mul,
  pixel => pixel);

memoryfirst_inst : memoryfirst PORT MAP (
		data	 => inbyte,
		rdaddress	 => memaddr,
		rdclock	 => clk50,
		wraddress	 => memwraddr,
		wrclock	 => clk12,
		wren	 => reset,
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
		reset_n => reset,
		h_sync => VGA_HS ,
		v_sync => VGA_VS,
		disp_ena => dispen,
		column => col,
		row => row
	);


end Behavioral;  