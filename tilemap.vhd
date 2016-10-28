library IEEE;  
use IEEE.STD_LOGIC_1164.ALL;  
use IEEE.NUMERIC_STD.ALL;

entity tilemap is
  generic(xoffset : integer := 100;
          yoffset : integer := 20;
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
  
  FUNCTION tile_data (tilenr:unsigned; ypos:integer; tileset:std_logic) RETURN unsigned IS
    variable tilestart: unsigned( 15 downto 0);
    variable tilerow : unsigned (2 downto 0); 
  BEGIN
  if tileset='1' then
    tilestart := x"8000" + 16*tilenr ;     --start of tile
  else
    tilestart := x"8800" + 16*(tilenr xor x"80") ;     --start of tile
  end if; 
    tilerow := to_unsigned(ypos,3);
    return tilestart + tilerow*2;
  END tile_data;
begin
process (clk, rst)
variable tile     : std_logic_vector(15 downto 0);
variable tile_int : std_logic_vector(15 downto 0);
variable rowaddr  : unsigned(15 downto 0);
variable tilecol  : integer range 0 to 7;
variable LCDC	   : std_logic_vector(7 downto 0);
variable SCY	   : integer range 0 to 255;
variable SCX	   : integer range 0 to 255;
variable realx    : integer range -xoffset to 255;
variable realy    : integer range -yoffset to 255;
begin
  if(rst = '0') then
    tile := (others => '0');
    tile_int := (others => '0');
	 rowaddr := (others => '0');
  elsif rising_edge(clk) then
    realx := xpos-xoffset+SCX;
    realy := ypos-yoffset+SCY;
	 tilecol := realx rem 8;
    if xpos >= xoffset and xpos < xoffset+screen_width  and
	    ypos >= yoffset and ypos < yoffset+screen_height then
		pixel <= not (tile(15-tilecol) & tile(7-tilecol));
	 end if;
	 
	 if xpos >= xoffset-16 and xpos < xoffset+screen_width  and
	    ypos >= yoffset and ypos < yoffset+screen_height then
      case tilecol is
		  when 4 =>
			 rowaddr := tile_nr_addr(realx, realy);
			 memaddr <= std_logic_vector(rowaddr);
		  when 5 =>
			 rowaddr := tile_data(unsigned(memdat), realy, LCDC(4));
			 memaddr <= std_logic_vector(rowaddr);
		  when 6 =>
			 tile_int(7 downto 0) := memdat;
			 memaddr <= std_logic_vector(rowaddr+1);
		  when 7 =>
			 tile_int(15 downto 8) := memdat;
			 tile := tile_int;
		  when others =>
		end case;
	 elsif ypos >= yoffset and ypos < yoffset+screen_height then
	   pixel <= "00";
		tile := (others => '0');
      tile_int := (others => '0');
		case xpos is
			when 0  =>
			  memaddr<=x"FF40";
			when 1  =>
			  LCDC:=memdat;
			  memaddr<=x"FF42";
			when 2  =>
			  SCY:=to_integer(unsigned(memdat));
			  memaddr<=x"FF43";
			when 3  =>
			  SCX:=to_integer(unsigned(memdat));
			when others => 
		end case;
	 end if;
  end if;
end process;
end;