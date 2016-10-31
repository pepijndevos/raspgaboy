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
		 
		 oam_rd_dat : in std_logic_vector (31 downto 0); -- 4 bytes
		 oam_rd_addr : out std_logic_vector (5 downto 0);

		 reg_rd_dat : in std_logic_vector (7 downto 0); -- 1 byte
		 reg_rd_addr : out std_logic_vector (7 downto 0);

		 tdat_rd_dat : in std_logic_vector (15 downto 0); -- 2 bytes
		 tdat_rd_addr : out std_logic_vector (11 downto 0);

		 tmap_rd_dat : in std_logic_vector (7 downto 0); -- 1 bytes
		 tmap_rd_addr : out std_logic_vector (10 downto 0);

		 xpos    : in integer range 0 to 1000;
		 ypos    : in integer range 0 to 1000;
		 pixel   : out std_logic_vector(1 downto 0)
		 );
end tilemap;

architecture bhv of tilemap is
  type pixel_t is (BLANK, BG, WINDOW, SPRITE);
  type sprite_t is array(0 to 9) of std_logic_vector(31 downto 0);
  FUNCTION tile_nr_addr (xpos:integer; ypos:integer; tilemap:std_logic) RETURN unsigned IS
    variable tilex : integer; 
	 variable tiley : integer;
	 variable baseaddr : unsigned(10 downto 0);
  BEGIN
    tilex := xpos / 8;   
	 tiley := ypos / 8;
	 if tilemap = '0' then
	   baseaddr := to_unsigned(0, baseaddr'length); --x"9800"
	 else
	   baseaddr := to_unsigned(1024, baseaddr'length); -- x"9C00"
	 end if;
	 return baseaddr + tiley*32 + tilex;  --tile number address
  END tile_nr_addr;
  
  FUNCTION tile_data (tilenr:unsigned; ypos:integer; tileset:std_logic) RETURN unsigned IS
    variable tilestart: unsigned(11 downto 0);
    variable tilerow : unsigned (2 downto 0); 
  BEGIN
  if tileset='1' then
    tilestart := resize(8*tilenr, tilestart'length); -- x"8000"
  else
    tilestart := resize(x"400" + 8*(tilenr xor x"80"), tilestart'length); -- x"8800"
  end if; 
    tilerow := to_unsigned(ypos,3);
    return tilestart + tilerow;
  END tile_data;
  
  FUNCTION get_pixel_type (xpos:integer; ypos:integer;
                       SCX:integer; SCY:integer;
							  WX:integer; WY: integer;
							  LCDC:std_logic_vector; cur_spr:integer) RETURN pixel_t IS
  BEGIN
    if cur_spr /= -1 then
	   return SPRITE;
	 elsif LCDC(5) = '1' and xpos >= WX and ypos >= WY then
	   return WINDOW;
	 elsif LCDC(0) = '1' then
	   return BG;
	 else
	   return BLANK;
	 end if;
  END get_pixel_type;
  
  FUNCTION get_current_sprite (screenx:integer; sprites:sprite_t) RETURN integer IS
  BEGIN
    for i in 0 to 9 loop
	   if screenx >= unsigned(sprites(i)(15 downto 8))-8 and
		   screenx <  unsigned(sprites(i)(15 downto 8)) then
		  return i;
		end if;
	 end loop;
	 return -1; -- where is my option type
  END get_current_sprite;
 
--  Bit 7 - LCD Display Enable             (0=Off, 1=On)
--  Bit 6 - Window Tile Map Display Select (0=9800-9BFF, 1=9C00-9FFF)
--  Bit 5 - Window Display Enable          (0=Off, 1=On)
--  Bit 4 - BG & Window Tile Data Select   (0=8800-97FF, 1=8000-8FFF)
--  Bit 3 - BG Tile Map Display Select     (0=9800-9BFF, 1=9C00-9FFF)
--  Bit 2 - OBJ (Sprite) Size              (0=8x8, 1=8x16)
--  Bit 1 - OBJ (Sprite) Display Enable    (0=Off, 1=On)
--  Bit 0 - BG Display (for CGB see below) (0=Off, 1=On)
	signal LCDC	     : std_logic_vector(7 downto 0);
	signal SCY	     : integer range 0 to 255;
	signal SCX	     : integer range 0 to 255;
	signal WY		     : integer range 0 to 255;
	signal WX		     : integer range 0 to 255;
   -- pseudo-register
   signal SPY	     : integer range 0 to 255;
	signal SPX	     : integer range 0 to 255;
	signal SPN	     : unsigned(7 downto 0);
--  Bit7   OBJ-to-BG Priority (0=OBJ Above BG, 1=OBJ Behind BG color 1-3)
--         (Used for both BG and Window. BG color 0 is always behind OBJ)
--  Bit6   Y flip          (0=Normal, 1=Vertically mirrored)
--  Bit5   X flip          (0=Normal, 1=Horizontally mirrored)
--  Bit4   Palette number  **Non CGB Mode Only** (0=OBP0, 1=OBP1)
--  Bit3   Tile VRAM-Bank  **CGB Mode Only**     (0=Bank 0, 1=Bank 1)
--  Bit2-0 Palette number  **CGB Mode Only**     (OBP0-7)
	signal SPF	     : std_logic_vector(7 downto 0);
	
	signal bgx        : integer range -255 to 1000;
	signal bgy        : integer range -255 to 1000;
	signal windowx    : integer range -255 to 1000;
	signal windowy    : integer range -255 to 1000;
	signal screenx    : integer range -255 to 1000;
	signal screeny    : integer range -255 to 1000;
	signal spritex    : integer range -255 to 1000;
	signal spritey    : integer range -255 to 1000;
	
	signal tilecol    : integer range 0 to 7;
	signal windcol    : integer range 0 to 7;
	signal scrncol    : integer range 0 to 7;
	signal sprtcol    : integer range 0 to 7;
	
	signal drawing    : boolean;
   signal tile       : std_logic_vector(15 downto 0);
	signal pixel_type : pixel_t;
	signal sprite_lst : sprite_t;
	signal cur_sprite : integer range -1 to 9;
begin
  drawing <= xpos >= xoffset and xpos < xoffset+screen_width and
	          ypos >= yoffset and ypos < yoffset+screen_height;
  cur_sprite <= get_current_sprite(screenx, sprite_lst);
  pixel_type <= get_pixel_type(xpos+1, ypos, SCX, SCY, WX, WY, LCDC, cur_sprite);

  screenx <= xpos-xoffset;
  screeny <= ypos-yoffset;
  bgx <= screenx+SCX;
  bgy <= screeny+SCY;
  windowx <= screenx-WX+7;
  windowy <= screeny-WY;
  
  SPY <= to_integer(unsigned(sprite_lst(cur_sprite)(7 downto 0)))
         when cur_sprite /= -1 else 0;
  SPX <= to_integer(unsigned(sprite_lst(cur_sprite)(15 downto 8)))
         when cur_sprite /= -1 else 0;
  SPN <= unsigned(sprite_lst(cur_sprite)(23 downto 16))
         when cur_sprite /= -1 else x"00";
  SPF <= sprite_lst(cur_sprite)(31 downto 24)
         when cur_sprite /= -1 else x"00";
			
  spritex <= screenx-SPX+8;
  spritey <= screeny-SPY+16;
			
  tilecol <= bgx mod 8;
  windcol <= windowx mod 8;
  scrncol <= screenx mod 8;
  sprtcol <= spritex mod 8;
  
  process(pixel_type, bgx, bgy, windowx, windowy, LCDC, SPN, spritey)
  begin
    case pixel_type is
        when BG =>
		    tmap_rd_addr <= std_logic_vector(tile_nr_addr(bgx+1, bgy, LCDC(3)));
			 tdat_rd_addr <= std_logic_vector(tile_data(unsigned(tmap_rd_dat), bgy, LCDC(4)));
        when WINDOW =>
		    tmap_rd_addr <= std_logic_vector(tile_nr_addr(windowx+1, windowy, LCDC(6)));
			 tdat_rd_addr <= std_logic_vector(tile_data(unsigned(tmap_rd_dat), windowy, LCDC(4)));
        when SPRITE =>
		    tdat_rd_addr <= std_logic_vector(tile_data(SPN, spritey, '1')); -- x"8000"
        when BLANK =>
    end case;
  end process;
  tile <= tdat_rd_dat;

process (clk, rst)
variable sprite_counter : integer range 0 to 9;
begin
  if(rst = '0') then
	 LCDC <= (others => '0');
	 SCY <= 0;
	 SCX <= 0;
	 WY <= 0;
	 WX <= 0;
  elsif rising_edge(clk) then

    if drawing then
	   case pixel_type is
        when BG =>
		    pixel <= not (tile(15-tilecol) & tile(7-tilecol));
        when WINDOW =>
		    pixel <= not (tile(15-windcol) & tile(7-windcol));
        when SPRITE =>
		    if SPF(5) = '0' then -- check horizontal flip
		      pixel <= not (tile(15-sprtcol) & tile(7-sprtcol));
			 else
			   pixel <= not (tile(8+sprtcol) & tile(sprtcol));
			 end if;
        when BLANK =>
      end case;
	 else
	   pixel <= "10";
	 end if;

	 if ypos >= yoffset and ypos < yoffset+screen_height then
		case xpos is
			when 0  =>
			  reg_rd_addr<=x"40";
			  oam_rd_addr<=(others=>'0');
			  sprite_counter:=0;
			  sprite_lst <= (others => (others => '0'));
			when 1  =>
			  LCDC<=reg_rd_dat;
			  reg_rd_addr<=x"42";
			when 2  =>
			  SCY<=to_integer(unsigned(reg_rd_dat));
			  reg_rd_addr<=x"43";
			when 3  =>
			  SCX<=to_integer(unsigned(reg_rd_dat));
			  reg_rd_addr<=x"4A";
			when 4  =>
			  WY<=to_integer(unsigned(reg_rd_dat));
			  reg_rd_addr<=x"4B";
			when 5  =>
			  WX<=to_integer(unsigned(reg_rd_dat));
			when others => 
		end case;
		if xpos > 0 and xpos < 41 then
		  if screeny >= unsigned(oam_rd_dat(7 downto 0))-16 and
		     screeny < unsigned(oam_rd_dat(7 downto 0))-8 then
		    sprite_lst(sprite_counter) <= oam_rd_dat;
			 sprite_counter := sprite_counter+1;
		  end if;
		  oam_rd_addr <= std_logic_vector(to_unsigned(xpos, 6));
		end if;
	 end if;
  end if;
end process;
end;