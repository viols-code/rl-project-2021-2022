
-------------------------------------------------------------------------------
-- 
-- Final Project for the Digital Circuit Design Course
-- Prof Palermo
--
-- Developed by : Viola Renne (10681612/932160)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Convolutional code
-------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;

entity FSM is
    port (
        u       : in std_logic;     -- the input
        clk     : in std_logic;   
        rst     : in std_logic;
        stop    : in std_logic;     -- stop signal
        p1      : out std_logic;    -- first output
        p2      : out std_logic     -- second output
    );
end FSM;


architecture behavioural of FSM is
    -- Custom type for FSM state
    type state_type is (S0, S1, S2, S3);
    
    signal next_state, current_state: state_type := S0;

begin
    state_reg: process(clk, rst)
    begin
        if rst='1' then
            current_state <= S0;
        elsif rising_edge(clk) then
            current_state <= next_state;
        end if;
    end process;

    -- delta function
    delta0: process(current_state, u, stop)
    begin
        next_state <= current_state;
        case current_state is
            when S0 =>
                if stop = '0' then
                    if u = '0' then
                        next_state <= S0;
                    else
                        next_state <= S2;
                    end if;
                end if;
            when S1 =>
                if stop = '0' then
                    if u = '0' then
                        next_state <= S0;
                    else
                        next_state <= S2;
                    end if;
                end if;
            when S2 =>
                if stop = '0' then
                    if u = '0' then
                        next_state <= S1;
                    else
                        next_state <= S3;
                    end if;
                end if;
            when S3 =>
                if stop = '0' then
                    if u = '0' then
                        next_state <= S1;
                    else
                        next_state <= S3;
                    end if;
                end if;
        end case;
    end process;

    --lambda function
    lambda0: process(current_state, u)
    begin
        case current_state is
            when S0 =>
                if u = '0' then
                    p1 <= '0';
                    p2 <= '0';
                else
                    p1 <= '1';
                    p2 <= '1';
                end if;
            when S1 =>
                if u = '0' then
                    p1 <= '1';
                    p2 <= '1';
                else
                    p1 <= '0';
                    p2 <= '0';
                end if;
            when S2 =>
                if u = '0' then
                    p1 <= '0';
                    p2 <= '1';
                else
                    p1 <= '1';
                    p2 <= '0';
                    end if;
            when S3 =>
                if u = '0' then
                    p1 <= '1';
                    p2 <= '0';
                else
                    p1 <= '0';
                    p2 <= '1';
                end if;
        end case;
    end process;
end behavioural;

-------------------------------------------------------------------------------
-- Datapath
-------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.std_logic_unsigned.ALL;

entity datapath is
    port(
        i_clk           : in std_logic;
        i_rst           : in std_logic;
        i_data          : in std_logic_vector(7 downto 0);
        o_data          : out std_logic_vector(7 downto 0);
        i_word_load     : in std_logic;
        stop            : in std_logic;
        o_load          : in std_logic;
        r_sel           : in std_logic;
        o_word_load     : in std_logic;
        d_sel           : in std_logic;
        end_addr_load   : in std_logic;
        read_addr_load  : in std_logic;
        i_sel           : in std_logic_vector(2 downto 0);
        re_sel          : in std_logic;
        wr_sel          : in std_logic;
        write_addr_load : in std_logic;
        oa_sel          : in std_logic;
        reset           : in std_logic;
        o_end           : out std_logic;
        o_addr          : out std_logic_vector(15 downto 0)
    );
end datapath;

architecture behavioural of datapath is
    signal i_word           : std_logic_vector(7 downto 0);         -- word read in the RAM
    signal o_word           : std_logic_vector(15 downto 0);        -- words to be written in the RAM
    signal mux_o_word       : std_logic_vector(15 downto 0);        -- result of the multiplexer for the o_word
    signal end_addr         : std_logic_vector(15 downto 0);        -- the number of word to be read and the last address 
    signal read_addr        : std_logic_vector(15 downto 0);        -- address used for reading
    signal mux_read_addr    : std_logic_vector(15 downto 0);        -- result of the multiplexer for the read_addr
    signal write_addr       : std_logic_vector(15 downto 0);        -- address used for writing
    signal mux_write_addr   : std_logic_vector(15 downto 0);        -- result of the multiplexer for the write_addr         
    signal output           : std_logic_vector(1 downto 0);         -- concatenation of output_1 and output_2
    signal input            : std_logic;                            -- input for the convolutional code
    signal output_1         : std_logic;                            -- first output of the convolutional code
    signal output_2         : std_logic;                            -- second output of the convolutional code

    component FSM is
        port (
            u       : in std_logic;
            clk     : in std_logic;
            rst     : in std_logic;
            stop    : in std_logic;
            p1      : out std_logic;
            p2      : out std_logic
        );
    end component;

begin
    CONVOLUTIONAL_CODE: FSM
        port map(
            u       => input,
            clk     => i_clk,
            rst     => reset,
            stop    => stop,
            p1      => output_1,
            p2      => output_2
        );

    -- process for i_word
    process(i_clk, i_rst)
    begin
        if(i_rst = '1') then
            i_word <= "00000000";   -- reset the word read
        elsif rising_edge(i_clk) then
            if(i_word_load = '1') then
                i_word <= i_data;   -- update the word read
            end if;
        end if;
    end process;

    -- the input of the convolutional code
    input <= i_word(to_integer(unsigned(i_sel)));

    -- process for the output
    process(i_clk, i_rst)
    begin
        if(i_rst = '1') then
            output <= "00"; -- reset the concatenation of output
        elsif rising_edge(i_clk) then
            if(o_load = '1') then
                output <= output_1 & output_2; -- concatenation of the two output from the convolutional code
            end if;
        end if;
    end process;

    -- multiplexer selecting the right o_word value
    with r_sel select
        mux_o_word <=   (others => '0') when '0',               -- for resetting the value of o_word
                        o_word(13 downto 0) & output when '1',  -- compute the value of o_word
                        "XXXXXXXXXXXXXXXX" when others;

    -- process for o_word
    process(i_clk, i_rst)
    begin
        if(i_rst = '1') then
            o_word <= (others => '0');  -- reset o_word value
        elsif rising_edge(i_clk) then
            if(o_word_load = '1') then
                o_word <= mux_o_word;   -- set the o_word value to the mux_o_word value
            end if;
        end if;
    end process;

    -- multiplexer for o_data
    with d_sel select
        o_data <=   o_word(7 downto 0) when '0',    -- selecting the 0..7 bit
                    o_word(15 downto 8) when '1',   -- selecting the 8..15 bit
                    "XXXXXXXX" when others;

    -- process for end_addr
    process(i_clk, i_rst)
    begin
        if(i_rst = '1') then
            end_addr <= (others => '0');    -- reset end_addr value
        elsif rising_edge(i_clk) then
            if(end_addr_load = '1') then
                end_addr <= "00000000" & i_data;    -- set end_addr value
            end if;
        end if;
    end process;

    -- multiplexer for mux_read_addr
    with re_sel select
        mux_read_addr <=    (others => '0') when '0',                   -- reset the read_addr
                            read_addr + "0000000000000001" when '1',    -- increment the read_addr by one
                            "XXXXXXXXXXXXXXXX" when others;          
    
    -- process for read_addr
    process(i_clk, i_rst)
    begin
        if(i_rst = '1') then
            read_addr <= (others => '0');   -- reset the read_addr value
        elsif rising_edge(i_clk) then
            if(read_addr_load = '1') then
                read_addr <= mux_read_addr; -- set the read_addr value
            end if;
        end if;
    end process;
    
    -- multiplexer for o_addr
    with oa_sel select
        o_addr <=   read_addr when '0',             -- read_addr for reading operation
                    write_addr when '1',            -- write_addr for writing operation
                    "XXXXXXXXXXXXXXXX" when others;

    -- multiplexer for mux_write_addr
    with wr_sel select
        mux_write_addr <=   "0000001111101000" when '0',                -- reset write_addr value
                            write_addr + "0000000000000001" when '1',   -- increment write_adddr by one
                            "XXXXXXXXXXXXXXXX" when others;

    -- process for write_addr
    process(i_clk, i_rst)
    begin
        if(i_rst = '1') then
            write_addr <= "0000001111101000";
        elsif rising_edge(i_clk) then
            if(write_addr_load = '1') then
                write_addr <= mux_write_addr;
            end if;
        end if;
    end process;
    
    -- compute o_end
    o_end <= '1' when (read_addr = end_addr) else '0';

end behavioural;


-------------------------------------------------------------------------------
-- The top-level module
-------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

-- entity
entity project_reti_logiche is
    port (
        i_clk       : in std_logic;
        i_rst       : in std_logic;
        i_start     : in std_logic;
        i_data      : in std_logic_vector(7 downto 0);
        o_address   : out std_logic_vector(15 downto 0);
        o_done      : out std_logic;
        o_en        : out std_logic;
        o_we        : out std_logic;
        o_data      : out std_logic_vector (7 downto 0)
    );
end project_reti_logiche;

architecture behavioural of project_reti_logiche is
    type fsm_state is(
        IDLING,             -- Idingli, waiting for start signal
        SET_READ_ADDR,      -- Setting address
        WAITING_ADDR,       -- Waiting for RAM to load the end_addr
        READ_ADDR,          -- Save the value in end_addr
        SET_READ_DATA,      -- Setting address to read the word 
        WAITING_DATA,       -- Waiting dor the RAM to load the word
        READ_DATA,          -- Read the word
        GENERATE_DATA,      -- Generating the two words with the convolutional code
        END_GENERATION,     -- Ending of the generation
        WRITE_FIRST_WORD,   -- Setting address and enables to write the first word in RAM
        WRITE_SECOND_WORD,  -- Setting address and enables to write the second word in RAM
        END_WRITE           -- Waiting for the start signal
    );

    signal curr_state, next_state   : fsm_state := IDLING;
    signal i_word_load              : std_logic;
    signal i_sel, i_sel_next        : std_logic_vector(2 downto 0);
    signal stop                     : std_logic;
    signal o_load                   : std_logic;
    signal r_sel                    : std_logic;
    signal o_word_load              : std_logic;
    signal d_sel                    : std_logic;
    signal end_addr_load            : std_logic;
    signal read_addr_load           : std_logic;
    signal re_sel                   : std_logic;
    signal wr_sel                   : std_logic;
    signal write_addr_load          : std_logic;
    signal oa_sel                   : std_logic;
    signal o_end                    : std_logic;
    signal reset                    : std_logic;
    
    signal o_we_next                : std_logic;
    signal o_en_next                : std_logic;
    
    component datapath is
        port(
            i_clk           : in std_logic;
            i_rst           : in std_logic;
            i_data          : in std_logic_vector(7 downto 0);
            o_data          : out std_logic_vector(7 downto 0);
            i_word_load     : in std_logic;
            i_sel           : in std_logic_vector(2 downto 0);
            stop            : in std_logic;
            o_load          : in std_logic;
            r_sel           : in std_logic;
            o_word_load     : in std_logic;
            d_sel           : in std_logic;
            end_addr_load   : in std_logic;
            read_addr_load  : in std_logic;
            re_sel          : in std_logic;
            wr_sel          : in std_logic;
            write_addr_load : in std_logic;
            oa_sel          : in std_logic;
            reset           : in std_logic;
            o_end           : out std_logic;
            o_addr          : out std_logic_vector(15 downto 0)
        );
    end component;

begin
    DATAPATH0: datapath
        port map(
            i_clk           => i_clk,
            i_rst           => i_rst,
            i_data          => i_data,
            o_data          => o_data,
            i_word_load     => i_word_load,
            i_sel           => i_sel,
            stop            => stop,
            o_load          => o_load,
            r_sel           => r_sel,
            o_word_load     => o_word_load,
            d_sel           => d_sel,
            end_addr_load   => end_addr_load,
            read_addr_load  => read_addr_load,
            re_sel          => re_sel,
            wr_sel          => wr_sel,
            write_addr_load => write_addr_load,
            oa_sel          => oa_sel,
            reset           => reset,
            o_end           => o_end,
            o_addr          => o_address
        );

    STATE_OUTPUT: process(i_clk, i_rst, curr_state)
    begin
        if(i_rst = '1') then
            -- Asynchronously reset the machine
            curr_state <= IDLING;
        elsif rising_edge(i_clk) then
            curr_state <= next_state;
            i_sel <= i_sel_next;
            o_we <= o_we_next;
            o_en <= o_en_next;
            oa_sel <= o_we_next;
        end if;
    end process;

    DELTA1: process (o_end, curr_state, i_start, i_sel)
    -- The combinatory process which compute the next state
    begin
        next_state <= curr_state;

        case curr_state is
            when IDLING =>
                if(i_start = '1') then
                    next_state <= SET_READ_ADDR;
                end if;

            when SET_READ_ADDR =>
                next_state <= WAITING_ADDR;

            when WAITING_ADDR =>
                next_state <= READ_ADDR;

            when READ_ADDR =>
                next_state <= SET_READ_DATA;

            when SET_READ_DATA =>
                if (o_end = '0') then
                    next_state <= WAITING_DATA;
                else
                    next_state <= END_WRITE;
                end if;

            when WAITING_DATA =>
                next_state <= READ_DATA;

            when READ_DATA =>
                next_state <= GENERATE_DATA;

            when GENERATE_DATA =>
                if(i_sel = "000") then
                    -- if i_sel = "000" then the input given to the convolutional code is the last
                    next_state <= END_GENERATION;
                else
                    next_state <= GENERATE_DATA;
                end if;
                
            when END_GENERATION =>
                next_state <= WRITE_FIRST_WORD;
                
            when WRITE_FIRST_WORD =>
                next_state <= WRITE_SECOND_WORD;

            when WRITE_SECOND_WORD =>
                next_state <= SET_READ_DATA;

            when END_WRITE =>
                if i_start = '0' then
                    next_state <= END_WRITE;
                else
                    next_state <= IDLING;
                end if;
        end case;
    end process;
    
    
    LAMBDA1: process (curr_state, o_end, i_clk, i_sel)
    begin
        -- Signal assignment to avoid inferred latch
        o_en_next <= '0';
        o_done <= '0';
        o_we_next <= '0';
        i_word_load <= '0';
        i_sel_next <= "111";
        stop <= '1';
        o_load <= '0';
        r_sel <= '0';
        o_word_load <= '0';
        d_sel <= '0';
        end_addr_load <= '0';
        read_addr_load <= '0';
        re_sel <= '0';
        wr_sel <= '0';
        write_addr_load <= '0';
        reset <= '0';

        case curr_state is
            when IDLING =>
                reset <= '1';

            when SET_READ_ADDR =>
                read_addr_load <= '1';
                write_addr_load <= '1';
                o_word_load <= '1';            
                o_en_next <= '1';
                o_we_next <= '0';
       
            when WAITING_ADDR =>

            when READ_ADDR =>
                end_addr_load <= '1';

            when SET_READ_DATA =>
                re_sel <= '1';
                read_addr_load <= '1';
                if (o_end = '0') then
                    o_we_next <= '0';
                    o_en_next <= '1';
                else
                    o_en_next <= '0';
                end if;

            when WAITING_DATA =>

            when READ_DATA =>
                i_word_load <= '1';

            when GENERATE_DATA =>
                i_sel_next <= std_logic_vector(unsigned(i_sel) - 1);
                o_load <= '1';
                r_sel <= '1';
                o_word_load <= '1';
                stop <= '0';
                
            when END_GENERATION => 
                o_load <= '1';
                r_sel <= '1';
                o_word_load <= '1';
                o_en_next <= '1';
                o_we_next <= '1';
                
            when WRITE_FIRST_WORD =>
                o_en_next <= '1';
                o_we_next <= '1';
                d_sel <= '1';
                write_addr_load <= '1';
                wr_sel <= '1';

            when WRITE_SECOND_WORD =>
                write_addr_load <= '1';
                wr_sel <= '1';

            when END_WRITE =>
                o_done <= '1';
        end case;
    end process;

end behavioural;