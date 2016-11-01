-- In this version scancode is read on the negative edge of kbclock.
-- It counts the number of negative edges. IF 11 edges are detected the byte is read and byte_read is '1'.

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.numeric_std.ALL;
ENTITY showspi IS
  PORT (
    reset       : in std_logic;
    dig0, dig1,
	 dig2, dig3,
    dig4, dig5  : OUT std_logic_vector(6 DOWNTO 0); -- show key pressed on display in Hex dig1 (upper 4 bits) dig0 (lower 4 bits)
	 raspi_ss0   : in  STD_LOGIC;
	 raspi_ss1   : in  STD_LOGIC;
    raspi_mosi  : in  STD_LOGIC;
	 raspi_miso  : out STD_LOGIC;
    raspi_sck   : in  STD_LOGIC;
	
	oam_wr_dat : out std_logic_vector (7 downto 0);
    oam_wr_addr : out std_logic_vector (7 downto 0); -- 256 bytes
    reg_wr_dat : out std_logic_vector (7 downto 0);
    reg_wr_addr : out std_logic_vector (7 downto 0); -- 256 bytes
    tdat_wr_dat : out std_logic_vector (7 downto 0);
    tdat_wr_addr : out std_logic_vector (12 downto 0); -- 2*256*16 8k bytes
	tmap_wr_dat : out std_logic_vector (7 downto 0);
    tmap_wr_addr : out std_logic_vector (10 downto 0) -- 2k
	);
END showspi;


ARCHITECTURE bhv OF showspi IS
  FUNCTION hex2display (n:std_logic_vector(3 DOWNTO 0)) RETURN std_logic_vector IS
    VARIABLE res : std_logic_vector(6 DOWNTO 0);
  BEGIN
    CASE n IS          --        gfedcba; low active
	    WHEN "0000" => RETURN NOT "0111111";
	    WHEN "0001" => RETURN NOT "0000110";
	    WHEN "0010" => RETURN NOT "1011011";
	    WHEN "0011" => RETURN NOT "1001111";
	    WHEN "0100" => RETURN NOT "1100110";
	    WHEN "0101" => RETURN NOT "1101101";
	    WHEN "0110" => RETURN NOT "1111101";
	    WHEN "0111" => RETURN NOT "0000111";
	    WHEN "1000" => RETURN NOT "1111111";
	    WHEN "1001" => RETURN NOT "1101111";
	    WHEN "1010" => RETURN NOT "1110111";
	    WHEN "1011" => RETURN NOT "1111100";
	    WHEN "1100" => RETURN NOT "0111001";
	    WHEN "1101" => RETURN NOT "1011110";
	    WHEN "1110" => RETURN NOT "1111001";
	    WHEN OTHERS => RETURN NOT "1110001";			
    END CASE;
  END hex2display;
BEGIN
  process (raspi_sck, reset)
  	variable dword   : std_logic_vector (23 downto 0);
	variable spidx   : integer range dword'low to dword'high;
	variable part : std_logic_vector (15 downto 0); 
	variable temp : unsigned (15 downto 0);
	variable long_address : std_logic_vector (15 downto 0);
	begin
	   if reset = '0' or raspi_ss0 = '1' then
		  dword := (others=>'0');
		  spidx := 0;
		elsif rising_edge(raspi_sck) and raspi_ss0 = '0'then
	     dword := dword(dword'high-1 downto 0) & raspi_mosi;
		  if spidx = dword'high then
		    dig0 <= hex2display(dword(3 downto 0));
		    dig1 <= hex2display(dword(7 downto 4));
		    dig2 <= hex2display(dword(11 downto 8));
		    dig3 <= hex2display(dword(15 downto 12));
		    dig4 <= hex2display(dword(19 downto 16));
			 dig5 <= hex2display(dword(23 downto 20));
			 spidx := 0;
			 part := dword(23 downto 8);
			 temp := unsigned(part);
			 
			if temp >= x"FE00" and temp<= x"FE9F" then 
				oam_wr_dat <= dword (7 downto 0);
				--oam_wr_addr <=  std_logic_vector(to_unsigned(part), 8);
				long_address := std_logic_vector(temp- x"fe00");
				oam_wr_addr <= long_address(7 downto 0);
			elsif temp >= x"FF00" and temp <= x"FFFF" then 
				reg_wr_dat <= dword (7 downto 0) ;
				long_address := std_logic_vector(temp- x"ff00");
				reg_wr_addr <=  long_address(7 downto 0); 
			elsif temp >= x"8000" and temp <= x"97ff" then 
				tdat_wr_dat <= dword (7 downto 0);
				long_address := std_logic_vector(temp- x"8000");
				tdat_wr_addr <=  long_address(12 downto 0);
			elsif temp >= x"9800" and temp <= x"9FFf" then 
				tmap_wr_dat <= dword(7 downto 0) ; 
				long_address := std_logic_vector(temp- x"9800");
				tmap_wr_addr <=  long_address(10 downto 0);
			end if; 

		  else
	       spidx := spidx+1;
	     end if;
		 
		end if;
	end process;
END;
