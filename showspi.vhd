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
    raspi_sck   : in  STD_LOGIC);
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
	variable spidx   : integer range dword'high to dword'low;
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
		  else
	       spidx := spidx+1;
	     end if;
		end if;
	end process;
END;
