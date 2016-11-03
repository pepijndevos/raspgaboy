 library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity rom1 is
 port(clk25  : in std_logic;
      reset   : in std_logic;
		col : in  integer;
		row : in integer;
		redofpixel : out std_logic_vector(7 downto 0);
		greenofpixel : out std_logic_vector(7 downto 0);
		blueofpixel : out std_logic_vector(7 downto 0)
      );
end rom1;

architecture Behavioral of rom1 is

COMPONENT romport1
 PORT (
    addra		: IN STD_LOGIC_VECTOR (13 DOWNTO 0);
	clka		: IN STD_LOGIC  := '1';
	douta	: OUT STD_LOGIC_VECTOR (23 DOWNTO 0)
  );
END COMPONENT;

--signal clk25 : std_logic;  

--signal clka  :  std_logic;
--signal addra :  std_logic_vector (13 DOWNTO 0); 
--signal douta : std_logic_vector (23 downto 0); 


signal addr_rom : STD_LOGIC_VECTOR(13 DOWNTO 0) := (others => '0');
signal data_rom : STD_LOGIC_VECTOR(23 DOWNTO 0) := (others => '0');

--signal row,col : integer := 0;


begin


--image_rom : Entity work.romport1(SYN) port map(addr_rom,clk25,data_rom);


process(clk25,addr_rom)
--variable counter,donepixel,nextcol : integer := 0 ;
variable temp : unsigned(13 downto 0);
begin
    if rising_edge(clk25) then
			
		if col <= 201 and row> 420 then 
			redofpixel <= data_rom(7 downto 0);
			greenofpixel <= data_rom (15 downto 8);
			blueofpixel <= data_rom (23 downto 16);
			temp := unsigned((addr_rom) + "00000000000001");
			addr_rom <= std_logic_vector (temp) ;
		end if;
		if row >480 then 
			addr_rom <= "11111111111111";
		end if;
--			redofpixel <= "00000000";
--			greenofpixel <= "00000000";
--			blueofpixel <= "11111111";
	
       
		  -- if col <= 201 then 
			-- if counter = 0 then               --red value
				-- temp := addr_rom ;
				-- redofpixel <= "0000" & temp ; 		
				-- addr_rom <= temp;
				-- counter := counter +1;
			-- elsif counter = 1 then    --green value
				-- temp := addr_rom + "0001" ;
				-- greenofpixel <= "0000" & temp ;		
				-- addr_rom <= temp;
				-- counter := counter + 1;
			-- elsif counter = 2 then 		--blue value	
			    -- temp := addr_rom + "0001" ;
				-- blueofpixel <= "0000" & temp ; 
				-- addr_rom <= temp;
				-- counter := 0; 
				-- donepixel := 1; 
			-- end if; 
			
			-- nextcol := col +1;
			
--			if donepixel =1 then 
--				if nextcol /= 201 then
--					col <= col +1;
--				else 
--					col <= 0; 
--					row <= row +1;
--				end if;
--			end if;
				
--			else
	          --addr_rom <= "00000000000000"; 
           
      end if; 
  --  end if; 
end process;  	

romport1_inst : romport1 PORT MAP (
		addra	=>	addr_rom,           --prota mpainei to onoma apo to file romport1
		clka	=>  clk25,
		douta	=>  data_rom
);	
    
  

end Behavioral;

