library IEEE;  
use IEEE.STD_LOGIC_1164.ALL;  
use IEEE.NUMERIC_STD.ALL;

entity tilemap is
  generic(xoffset : integer := 0;
          yoffset : integer := 0;
			 screen_width : integer := 160;
			 screen_height : integer := 144);
  port(clk     : in std_logic;
       fastclk : in std_logic;
       rst     : in std_logic;
		 
		 oam_rd_dat : in std_logic_vector (31 downto 0); -- 4 bytes
		 oam_rd_addr : out std_logic_vector (5 downto 0);

		 reg_rd_dat : in std_logic_vector (7 downto 0); -- 1 byte
		 reg_rd_addr : out std_logic_vector (7 downto 0);

		 tdat_rd_dat : in std_logic_vector (15 downto 0); -- 2 bytes
		 tdat_rd_addr : out std_logic_vector (11 downto 0);

		 tmap_rd_dat : in std_logic_vector (7 downto 0); -- 1 bytes
		 tmap_rd_addr : out std_logic_vector (10 downto 0);

		 xpos_in    : in integer range 0 to 1000;
		 ypos_in    : in integer range 0 to 1000;
		 pixel   : out std_logic_vector(23 downto 0)
		 );
end tilemap;

architecture bhv of tilemap is
  type pixel_t is (BLANK, BG, WINDOW);
  type addrmux_t is (SPRITESEL, TILESEL);
  type sprite_t is array(0 to 9) of std_logic_vector(31 downto 0);
 
  type lut_color is  array (integer range 0 to 3) of unsigned (23 downto 0);
  constant lutobj0 : lut_color := (x"ffffff", x"ff82ab", x"ab4e00", x"060002");
  constant lutobj1 : lut_color := (x"ffffff", x"a3ff00", x"48a45d", x"060002");
  constant lutbbp  : lut_color := (x"fffdea", x"00c585", x"0900ff", x"060002");
  
  
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
  BEGIN
    if tileset='1' then
      tilestart := resize(8*tilenr, tilestart'length); -- x"8000"
    else
      tilestart := resize(x"400" + 8*(tilenr xor x"80"), tilestart'length); -- x"8800"
    end if; 
    return tilestart + ypos;
  END tile_data;
  
  FUNCTION get_pixel_type (xpos:integer; ypos:integer;
							  WX:integer; WY: integer;
							  LCDC:std_logic_vector) RETURN pixel_t IS
  BEGIN
    if LCDC(7) = '1' and LCDC(5) = '1' and xpos+7 >= WX and ypos >= WY then
	   return WINDOW;
	 elsif LCDC(7) = '1' and LCDC(0) = '1' then
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

  FUNCTION get_palette_color (pixel_tmp:std_logic_vector(1 downto 0); palette_type:std_logic_vector(7 downto 0) ) RETURN std_logic_vector IS
  BEGIN
	case pixel_tmp is
	when "00" => RETURN palette_type(1 downto 0);
	when "01" => RETURN palette_type(3 downto 2);
	when "10" => RETURN palette_type(5 downto 4);
	when "11" => RETURN palette_type(7 downto 6);	
	when others => RETURN "00";    -- also the case for when "00" because lower two bits are not used (transparent)
	END CASE; 
  end get_palette_color;
  
  FUNCTION get_palette_color_rgb (pixel:std_logic_vector(1 downto 0);
                                  palette_type:std_logic_vector(7 downto 0);
											 rgb_palette:lut_color ) RETURN unsigned IS
  variable pixel_tmp : std_logic_vector(1 downto 0);
  BEGIN
   pixel_tmp := get_palette_color(pixel, palette_type);
	case pixel_tmp is
   when "00" => RETURN rgb_palette(0);
	when "01" => RETURN rgb_palette(1);
	when "10" => RETURN rgb_palette(2);
	when "11" => RETURN rgb_palette(3);	
	when others => RETURN "000000000000000000000000";    -- also the case for when "00" because lower two bits are not used (transparent)
	END CASE; 
  end get_palette_color_rgb;

 
--  Bit 7 - LCD Display Enable             (0=Off, 1=On)
--  Bit 6 - Window Tile Map Display Select (0=9800-9BFF, 1=9C00-9FFF)
--  Bit 5 - Window Display Enable          (0=Off, 1=On)
--  Bit 4 - BG & Window Tile Data Select   (0=8800-97FF, 1=8000-8FFF)
--  Bit 3 - BG Tile Map Display Select     (0=9800-9BFF, 1=9C00-9FFF)
--  Bit 2 - OBJ (Sprite) Size              (0=8x8, 1=8x16)
--  Bit 1 - OBJ (Sprite) Display Enable    (0=Off, 1=On)
--  Bit 0 - BG Display (for CGB see below) (0=Off, 1=On)
	signal LCDC	     : std_logic_vector(7 downto 0);
	signal bbp       : std_logic_vector(7 downto 0);
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
	signal obp0	     : std_logic_vector(7 downto 0);
	signal obp1      : std_logic_vector(7 downto 0);
	
   signal xpos       : integer range 0 to 1000;
	signal ypos       : integer range 0 to 1000;
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
	
   signal tilerow    : integer range 0 to 7;
	signal windrow    : integer range 0 to 7;
	signal scrnrow    : integer range 0 to 7;
	signal sprtrowi   : integer range 0 to 15;
	signal sprtrow    : integer range 0 to 15;
	
	signal drawing    : boolean;
   signal tile       : std_logic_vector(15 downto 0);
   signal sprite     : std_logic_vector(15 downto 0);
	signal cur_type : pixel_t;
	signal nxt_type : pixel_t;
	signal sprite_lst : sprite_t;
	signal addr_select: addrmux_t;
	signal cur_sprite : integer range -1 to 9;
	signal nxt_sprite : integer range -1 to 9;
	signal sprite_size : integer range  8 to 16;
	
begin
  xpos <= xpos_in/4;
  ypos <= ypos_in/3;
  
  drawing <= xpos >= xoffset and xpos < xoffset+screen_width and
	          ypos >= yoffset and ypos < yoffset+screen_height;
  cur_sprite <= get_current_sprite(screenx, sprite_lst);
  nxt_sprite <= get_current_sprite(screenx+1, sprite_lst);
  cur_type <= get_pixel_type(xpos, ypos, WX, WY, LCDC);
  nxt_type <= get_pixel_type(xpos+1, ypos, WX, WY, LCDC);

  screenx <= xpos-xoffset;
  screeny <= ypos-yoffset;
  bgx <= (screenx+SCX) mod 256;
  bgy <= (screeny+SCY) mod 256;
  windowx <= screenx-WX+7;
  windowy <= screeny-WY;
  
  SPY <= to_integer(unsigned(sprite_lst(cur_sprite)(7 downto 0)))
         when cur_sprite /= -1 else 0;
  SPX <= to_integer(unsigned(sprite_lst(cur_sprite)(15 downto 8)))
         when cur_sprite /= -1 else 0;
			
  SPN <= unsigned(sprite_lst(nxt_sprite)(23 downto 16))
         when nxt_sprite /= -1 else x"00";
  SPF <= sprite_lst(nxt_sprite)(31 downto 24)
         when nxt_sprite /= -1 else x"00";
			
  spritex <= screenx-SPX+8;
  spritey <= screeny-SPY+16;
  sprite_size <= 16 when LCDC(2) = '1' else 8; -- 8x8 or 8x16 sprites
  
  tilecol <= bgx mod 8;
  windcol <= windowx mod 8;
  scrncol <= screenx mod 8;
  sprtcol <= spritex mod 8;
  
  tilerow  <= bgy mod 8;
  windrow  <= windowy mod 8;
  scrnrow  <= screeny mod 8;
  sprtrowi <= spritey mod sprite_size;
  sprtrow  <= sprtrowi when SPF(6) = '0' else sprite_size-1-sprtrowi;
  
  process(nxt_type, tmap_rd_dat, addr_select, LCDC, bgx, bgy, windowx, windowy, SPN, sprtrow)
  variable tile_addr : std_logic_vector (11 downto 0);
  variable sprite_addr : std_logic_vector (11 downto 0);
  begin
    sprite_addr := std_logic_vector(tile_data(SPN, sprtrow, '1')); -- x"8000"
    case nxt_type is
        when BG =>
		    tmap_rd_addr <= std_logic_vector(tile_nr_addr((bgx+1) mod 256, bgy, LCDC(3)));
			 tile_addr    := std_logic_vector(tile_data(unsigned(tmap_rd_dat), tilerow, LCDC(4)));
        when WINDOW =>
		    tmap_rd_addr   <= std_logic_vector(tile_nr_addr(windowx+1, windowy, LCDC(6)));
			 tile_addr      := std_logic_vector(tile_data(unsigned(tmap_rd_dat), windrow, LCDC(4)));
        when BLANK =>
		    tmap_rd_addr <= (others => '0');
			 tile_addr    := (others => '0');
    end case;
	 case addr_select is
	   when SPRITESEL =>
		  tdat_rd_addr <= sprite_addr;
		when TILESEL =>
		  tdat_rd_addr <= tile_addr;
	 end case;
  end process;

  process (fastclk, rst)
  variable subpx : integer range 0 to 3;
  begin
    if(rst = '0') then
      addr_select <= TILESEL;
    elsif rising_edge(fastclk) then
      subpx := xpos_in mod 4;
	   if nxt_sprite /= -1 and subpx < 2 then
	     addr_select <= SPRITESEL;
	   else
	     addr_select <= TILESEL;
	   end if;
	   case addr_select is
	     when SPRITESEL =>
		    sprite <= tdat_rd_dat;
		  when TILESEL =>
          tile <= tdat_rd_dat;
	   end case;
    end if;
  end process;

process (clk, rst)
variable sprite_counter : integer range 0 to 9;
variable pixel_tmp : std_logic_vector (1 downto 0);

begin
  if(rst = '0') then
	 LCDC <= (others => '0');
	 SCY <= 0;
	 SCX <= 0;
	 WY <= 0;
	 WX <= 0;
  elsif rising_edge(clk) then

    if drawing then
	   --if SPF(5) = '0' then -- check horizontal flip
		if cur_sprite /= -1 and sprite_lst(cur_sprite)(29) = '0' then
		  pixel_tmp := (sprite(15-sprtcol) & sprite(7-sprtcol));
		elsif cur_sprite /= -1 then
		  pixel_tmp := (sprite(8+sprtcol) & sprite(sprtcol));
		end if;
	   if cur_sprite /= -1 and pixel_tmp /= "00" then
			--if spf(4) = '0' then 
			if sprite_lst(cur_sprite)(28) = '0' then
					pixel <=  std_logic_vector(get_palette_color_rgb(pixel_tmp,obp0, lutobj0));
		   else 
					pixel <=  std_logic_vector(get_palette_color_rgb(pixel_tmp,obp1,lutobj1));
					--pixel <= not get_palette_color(pixel_tmp,obp1);
		  end if;
		else
	     case cur_type is
          when BG =>
					pixel <=  std_logic_vector(get_palette_color_rgb((tile(15-tilecol) & tile(7-tilecol)),bbp,lutbbp));
          when WINDOW =>
					pixel <=  std_logic_vector(get_palette_color_rgb((tile(15-windcol) & tile(7-windcol)),bbp,lutbbp));
          when BLANK =>
		      pixel <= "000000000000000000000011";
        end case;
		end if;
	 else
	   pixel <= "000000000000000000000010";
	 end if;

	 if ypos >= yoffset and ypos < yoffset+screen_height then
		case xpos is
			when 160  =>
			  reg_rd_addr<=x"40";
			  oam_rd_addr<=(others=>'0');
			  sprite_counter:=0;
			  sprite_lst <= (others => (others => '0'));
			when 161  =>
			  LCDC<=reg_rd_dat;
			  reg_rd_addr<=x"42";
			when 162  =>
			  SCY<=to_integer(unsigned(reg_rd_dat));
			  reg_rd_addr<=x"43";
			when 163  =>
			  SCX<=to_integer(unsigned(reg_rd_dat));
			  reg_rd_addr<=x"4A";
			when 164  =>
			  WY<=to_integer(unsigned(reg_rd_dat));
			  reg_rd_addr<=x"4B";
			when 165  =>
			  WX<=to_integer(unsigned(reg_rd_dat));
			  reg_rd_addr<=x"47";
			when 166 =>
			   BBP <= reg_rd_dat;
			   reg_rd_addr <= x"48";
			when 167 =>
			  OBP0 <= reg_rd_dat;
			  reg_rd_addr <= x"49";
			when 168 => 
				OBP1 <= reg_rd_dat;
			
			when others => 
		end case;
		if xpos > 160 and xpos < 201 and LCDC(1) = '1' then
		  if (ypos_in+1)/3-yoffset >= unsigned(oam_rd_dat(7 downto 0))-16 and
		     (ypos_in+1)/3-yoffset < unsigned(oam_rd_dat(7 downto 0))-16+sprite_size then
		    sprite_lst(sprite_counter) <= oam_rd_dat;
			 sprite_counter := sprite_counter+1;
		  end if;
		  oam_rd_addr <= std_logic_vector(to_unsigned(xpos-160, 6));
		end if;
	 end if;
  end if;
end process;
end;