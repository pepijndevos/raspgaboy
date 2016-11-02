 library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity rom1 is
 port(CLOCK_50  : in std_logic;
      reset   : in std_logic;
		col : in  integer;
		row : in integer;
		redofpixel : out std_logic_vector(7 downto 0);
		greenofpixel : out std_logic_vector(7 downto 0);
		blueofpixel : out std_logic_vector(7 downto 0)
      );
end rom1;

architecture Behavioral of rom1 is

--COMPONENT romport1
--  PORT (
--    clka : IN STD_LOGIC;
--    addra : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
--    douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
--  );
--END COMPONENT;

signal clk25 : std_logic;  

signal clka  :  std_logic;
signal addra :  std_logic_vector (7 DOWNTO 0); 
signal douta : std_logic_vector (7 downto 0); 


signal addr_rom : STD_LOGIC_VECTOR(3 DOWNTO 0) := (others => '0');
signal data_rom : STD_LOGIC_VECTOR(31 DOWNTO 0) := (others => '0');
--signal row,col : integer := 0;


begin


image_rom : Entity work.romport1(SYN) port map(addra,clka,douta);





process(clk25)
variable counter,donepixel,nextcol : integer := 0 ;
variable redofpixel, greenofpixel, blueofpixel : std_logic_vector(7 downto 0);
variable temp : std_logic_vector (3 downto 0);
begin
    if(rising_edge(clk25)) then
       
		  if col /= 201 then 
			if counter = 0 then 
				temp := addr_rom + "0001" ;
				redofpixel := "0000" & temp ;
				--redofpixel := addr_rom + "0001";   		--red value
				addr_rom <= addr_rom;
				counter := counter +1;
			elsif counter = counter + 1 then
				temp := addr_rom + "0001" ;
				greenofpixel := "0000" & temp ;		--green value
				addr_rom <= addr_rom;
				counter := counter + 1;
			elsif counter = counter + 2 then 			--blue value
			   temp := addr_rom + "0001" ;
				greenofpixel := "0000" & temp ;
				blueofpixel := addr_rom + "0001"; 
				addr_rom <= addr_rom;
				counter := 0; 
				donepixel := 1; 
			end if; 
			nextcol := col +1;
			
--			if donepixel =1 then 
--				if nextcol /= 201 then
--					col <= col +1;
--				else 
--					col <= 0; 
--					row <= row +1;
--				end if;
--			end if;
				
        elsif reset = '0' then
        
             
            addr_rom <= "0000"; 
           
        end if; 
    end if; 
	 
    
end process;    

end Behavioral;

