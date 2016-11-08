library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;


entity rom1 is
 port(clk25  : in std_logic;
      reset   : in std_logic;
		col : in  integer;
		row : in integer;
		redofpixel : out std_logic_vector(7 downto 0);
		greenofpixel : out std_logic_vector(7 downto 0);
		blueofpixel : out std_logic_vector(7 downto 0);
		blackofpixel : out std_logic;
		buttons : in std_logic_vector (7 downto 0)
      );
end rom1;

architecture Behavioral of rom1 is

COMPONENT romport1
 PORT (
    addra		: IN STD_LOGIC_VECTOR (13 DOWNTO 0);
	clka		: IN STD_LOGIC  := '1';
	douta	: OUT STD_LOGIC
  );
END COMPONENT;

 


signal addr_rom : STD_LOGIC_VECTOR(13 DOWNTO 0) := (others => '0');
--signal data_rom : STD_LOGIC := (others => '0');
signal data_rom : STD_LOGIC := '0';

type lut_t is array (integer range 0 to 16) of signed(7 downto 0);
constant lut : lut_t := (x"00", x"0c", x"19", x"25", x"31", x"3c", x"47", x"51", x"5a", x"62", x"6a", x"70", x"75", x"7a", x"7d", x"7e", x"7f");

--FUNCTION flash (counteroffunction: integer) RETURN std_logic_vector IS
--		variable bluepixeloffunction : std_logic_vector(7 downto 0);
--  BEGIN
--
--    if counteroffunction > 10000 and counteroffunction < 20000  then 
--		bluepixeloffunction := "00000000";
--	 else 
--		bluepixeloffunction := "11111111";
--	 end if;
--	return bluepixeloffunction;
--  END flash;



begin


--image_rom : Entity work.romport1(SYN) port map(addr_rom,clk25,data_rom);


process(clk25,addr_rom)
--variable counter,donepixel,nextcol : integer := 0 ;
variable temp : unsigned(13 downto 0);
variable idx : integer range 0 to 16;
variable counter : unsigned (16 downto 0);
variable counteroffunction : integer := 0;
 
begin
    if rising_edge(clk25) then
			--counteroffunction := counteroffunction + 1 ;
			if col > 220 and col <=  420 and  row> 432  then 
				-- blueofpixel <= data_rom(7 downto 0);
				-- greenofpixel <= data_rom (15 downto 8);
				-- redofpixel <= data_rom (23 downto 16);
				--blackofpixel <= data_rom ;
				if data_rom = '1' then 
					redofpixel <= "00000000";
					greenofpixel <= "00000000";
					blueofpixel <= "11111111";
				else 
					idx := col/8+to_integer(counter(16 downto 12));
					case (idx mod 64)/16 is
					when 0 => redofpixel <= std_logic_vector(127+lut(idx mod 16));
			         greenofpixel <= std_logic_vector(127+lut(idx mod 16));
					when 1 => redofpixel <= std_logic_vector(127+lut(16-(idx mod 16)));
			         greenofpixel <= std_logic_vector(127+lut(16-(idx mod 16)));
					when 2 => redofpixel <= std_logic_vector(127-lut(idx mod 16));
						greenofpixel <= std_logic_vector(127-lut(idx mod 16));
					when 3 => redofpixel <= std_logic_vector(127-lut(16-(idx mod 16)));
			         greenofpixel <= std_logic_vector(127-lut(16-(idx mod 16)));
					when others =>
					end case;
					blueofpixel   <= "11111111";
				end if; 
			
				temp := unsigned(addr_rom) + 1;
				addr_rom <= std_logic_vector (temp) ;
			
			
			
                              
--							         redofpixel <= "00000000";
--										blueofpixel <= "11111111";
--										greenofpixel <= "00000000";


--										if counteroffunction = 150000000 then 
--												counteroffunction := 0;
--										end if;

--			       if (sel = "1110" and choosebtn = "10" and row>450 and row<460 and col>110 and col<120) or 
--						(sel = "1011" and choosebtn = "10" and row>440 and row < 450 and col> 100 and col< 110) or   --up button
--						(sel = "1101" and choosebtn = "10" and row>450 and row < 460 and col > 90 and col < 100) or    -- left button
--						(sel = "0111" and choosebtn = "10" and row>460 and row<470 and col>100 and col<110) or       -- down button	
--						(sel = "1101" and choosebtn = "01" and row>450 and row < 460 and col> 440 and col< 450) or  -- b button
--						(sel = "1110" and choosebtn = "01" and row>440 and row < 450 and col> 450 and col< 460) or --  a button
--						(sel = "1011" and choosebtn = "01" and row>468 and row<474 and col>127 and col<145) or  -- select
--						(sel = "0111" and choosebtn = "01" and row>475 and row<480 and col>150 and col<168)     --  start
--				      then 
--                              redofpixel <= "11111111";
--										blueofpixel <= "00000000";
--										greenofpixel <= "00000000";
--										
--					 end if;
		
      elsif (buttons(3) = '0' and row>450 and row<460 and col>110 and col<120) then      -- right 
		                     redofpixel <= "11111111";
									blueofpixel <= "00000000";
									greenofpixel <= "00000000";
			elsif		(buttons(1) = '0' and row>440 and row < 450 and col> 100 and col< 110) then  --up button
				        redofpixel <= "11111111";
									blueofpixel <= "00000000";
									greenofpixel <= "00000000";
			elsif  (buttons(0) = '0'  and row>450 and row < 460 and col > 90 and col < 100) then    --left
						  redofpixel <= "11111111";
									blueofpixel <= "00000000";
									greenofpixel <= "00000000";
				
				elsif		(buttons(2) = '0'  and row>460 and row<470 and col>100 and col<110) then       -- down button
					redofpixel <= "11111111";
									blueofpixel <= "00000000";
									greenofpixel <= "00000000";
									
				elsif		(buttons(4) = '0'  and row>450 and row < 460 and col> 180 and col< 190) then  -- b button
						redofpixel <= "11111111";
									blueofpixel <= "00000000";
									greenofpixel <= "00000000";
				elsif		(buttons(7) = '0'  and row>440 and row < 450 and col> 192 and col< 202) then --  a button
					redofpixel <= "11111111";
									blueofpixel <= "00000000";
									greenofpixel <= "00000000";
				elsif		(buttons(5) = '0' and row>472 and row<479 and col>127 and col<145) then  -- select
					redofpixel <= "11111111";
									blueofpixel <= "00000000";
									greenofpixel <= "00000000";
					elsif	(buttons(6) = '0'  and row>472 and row<479 and col>150 and col<168)  then   --  start
						redofpixel <= "11111111";
									blueofpixel <= "00000000";
									greenofpixel <= "00000000";
--				     elsif 
--					               redofpixel <= "00000000";
--										blueofpixel <= "11111111";
--										greenofpixel <= "00000000";
                              
										
					 --end if;
							
			elsif 
			(row>440 and row < 450 and col> 100 and col< 110) or   --up button
			(row>450 and row < 460 and col > 90 and col < 100) or    -- left button
			(row>460 and row<470 and col>100 and col<110) or       -- down button
			(row>450 and row<460 and col>110 and col<120)    -- right button
			or (row>450 and row < 460 and col> 180 and col< 190) or  -- b button
			(row>440 and row < 450 and col> 192 and col< 202) or --  a button
			( row>472 and row<479  and col>127 and col<145) or  -- select
			(row>472 and row<479 and col>150 and col<168)     --  start
			then 
						            redofpixel <= "00000000";
										blueofpixel <= "11111111";
										greenofpixel <= "00000000";				
							
		  elsif row > 432  then
					idx := col/8+to_integer(counter(16 downto 12));
					case (idx mod 64)/16 is
					when 0 => redofpixel <= std_logic_vector(127+lut(idx mod 16));
			         greenofpixel <= std_logic_vector(127+lut(idx mod 16));
					when 1 => redofpixel <= std_logic_vector(127+lut(16-(idx mod 16)));
			         greenofpixel <= std_logic_vector(127+lut(16-(idx mod 16)));
					when 2 => redofpixel <= std_logic_vector(127-lut(idx mod 16));
						greenofpixel <= std_logic_vector(127-lut(idx mod 16));
					when 3 => redofpixel <= std_logic_vector(127-lut(16-(idx mod 16)));
			         greenofpixel <= std_logic_vector(127-lut(16-(idx mod 16)));
					when others =>
					end case;
					blueofpixel   <= "11111111";
		  
		  
	     end if;
		
			if row = 0 then 
				addr_rom <= "00000000000000";
				counter := counter + 1;
			end if;
           
      end if; 
end process;  	

romport1_inst : romport1 PORT MAP (
		addra	=>	addr_rom,           --prota mpainei to onoma apo to file romport1
		clka	=>  clk25,
		douta	=>  data_rom
);	
    
  

end Behavioral;

