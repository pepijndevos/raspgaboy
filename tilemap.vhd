library IEEE;  
use IEEE.STD_LOGIC_1164.ALL;  
use IEEE.NUMERIC_STD.ALL;

entity tilemap is
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
end tilemap;

architecture bhv of tilemap is
  FUNCTION tile_nr_addr (xpos:integer; ypos:integer) RETURN unsigned IS
    variable tilex : integer; 
	 variable tiley : integer;
  BEGIN
    tilex := xpos / 8;   
	 tiley := ypos / 8;
	 return x"9800" + tiley*32 + tilex;  --tile number address
  END tile_nr_addr;
  
  FUNCTION tile_data (tilenr:unsigned; ypos:integer) RETURN unsigned IS
    variable tilestart: unsigned( 15 downto 0);
    variable tilerow : unsigned (2 downto 0); 
  BEGIN
    tilestart := x"8000" + 16*tilenr ;     --start of tile
	 tilerow := to_unsigned(ypos,3);
	 return tilestart + tilerow*2; 
  END tile_data;
begin
process (clk, rst)
variable tile     : std_logic_vector(15 downto 0);
variable tile_int : std_logic_vector(15 downto 0);
variable rowaddr  : unsigned(15 downto 0);
variable tilecol  : unsigned(2 downto 0);
begin
  if(rst = '0') then
    tile := (others => '0');
    tile_int := (others => '0');
	 rowaddr := (others => '0');
  elsif rising_edge(clk) then
    if xpos > xoffset and xpos < xoffset+screen_width  and
	    ypos > yoffset and ypos < yoffset+screen_height then
	   tilecol := to_unsigned(xpos-xoffset, 3);
      case tilecol is
		  when "000" =>
		    pixel <= tile(0) & tile(8);
			 rowaddr := tile_nr_addr(xpos-xoffset, ypos-yoffset);
			 memaddr <= std_logic_vector(rowaddr);
		  when "001" =>
		    pixel <= tile(1) & tile(9);
		  when "010" =>
		    pixel <= tile(2) & tile(10);
			 rowaddr := tile_data(unsigned(memdat), ypos-yoffset);
			 memaddr <= std_logic_vector(rowaddr);
		  when "011" =>
		    pixel <= tile(3) & tile(11);
		  when "100" =>
		    pixel <= tile(4) & tile(12);
			 tile_int(7 downto 0) := memdat;
			 memaddr <= std_logic_vector(rowaddr+1);
		  when "101" =>
		    pixel <= tile(5) & tile(13);
		  when "110" =>
		    pixel <= tile(6) & tile(14);
		  when "111" =>
		    pixel <= tile(7) & tile(15);
			 tile_int(15 downto 8) := memdat;
			 tile := tile_int;
		  when others =>
		    pixel <= "10";
		end case;
	 else
	   pixel <= "10";
	 end if;
  end if;
end process;
end;