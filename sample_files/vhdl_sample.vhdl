-- ==============================================================================
-- Comprehensive VHDL Sample - Syntax Highlighting Demonstration
-- ==============================================================================

-- This file demonstrates all major VHDL language features
-- for syntax highlighting purposes.

-- ==============================================================================
-- Comments
-- ==============================================================================

-- Single line comment

-- Multiple
-- line
-- comments

-- ==============================================================================
-- Library and Use Clauses
-- ==============================================================================

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;

library WORK;
use WORK.ALL;

-- Custom package
library MY_LIB;
use MY_LIB.MY_PACKAGE.ALL;

-- ==============================================================================
-- Package Declaration
-- ==============================================================================

package TYPES_PKG is
    -- Constants
    constant DATA_WIDTH : integer := 32;
    constant ADDR_WIDTH : integer := 16;
    constant CLK_FREQ   : real := 100.0e6;

    -- Type declarations
    type STATE_TYPE is (IDLE, START, PROCESS, DONE, ERROR);
    type BYTE_ARRAY is array (natural range <>) of std_logic_vector(7 downto 0);
    type MEMORY_ARRAY is array (0 to 1023) of std_logic_vector(DATA_WIDTH-1 downto 0);

    -- Subtypes
    subtype WORD is std_logic_vector(31 downto 0);
    subtype NIBBLE is std_logic_vector(3 downto 0);
    subtype ADDR_RANGE is natural range 0 to 2**ADDR_WIDTH - 1;

    -- Record type
    type AXI_STREAM is record
        tdata  : std_logic_vector(DATA_WIDTH-1 downto 0);
        tvalid : std_logic;
        tready : std_logic;
        tlast  : std_logic;
        tkeep  : std_logic_vector(DATA_WIDTH/8-1 downto 0);
    end record;

    -- Enumeration with encoding
    type ALU_OP is (OP_ADD, OP_SUB, OP_AND, OP_OR, OP_XOR, OP_SHL, OP_SHR);

    -- Function declarations
    function log2(n : natural) return natural;
    function reverse_bits(v : std_logic_vector) return std_logic_vector;
    function to_bcd(bin : unsigned) return std_logic_vector;

    -- Procedure declarations
    procedure wait_cycles(signal clk : in std_logic; n : natural);
    procedure reset_signals(signal rst : out std_logic; cycles : natural);

    -- Component declaration
    component FIFO is
        generic (
            DEPTH : natural := 16;
            WIDTH : natural := 8
        );
        port (
            clk     : in  std_logic;
            rst     : in  std_logic;
            wr_en   : in  std_logic;
            rd_en   : in  std_logic;
            din     : in  std_logic_vector(WIDTH-1 downto 0);
            dout    : out std_logic_vector(WIDTH-1 downto 0);
            full    : out std_logic;
            empty   : out std_logic
        );
    end component;

end package TYPES_PKG;

-- ==============================================================================
-- Package Body
-- ==============================================================================

package body TYPES_PKG is

    -- Function implementation
    function log2(n : natural) return natural is
        variable temp : natural := n;
        variable result : natural := 0;
    begin
        while temp > 1 loop
            temp := temp / 2;
            result := result + 1;
        end loop;
        return result;
    end function log2;

    function reverse_bits(v : std_logic_vector) return std_logic_vector is
        variable result : std_logic_vector(v'range);
    begin
        for i in v'range loop
            result(v'high - i + v'low) := v(i);
        end loop;
        return result;
    end function reverse_bits;

    function to_bcd(bin : unsigned) return std_logic_vector is
        variable result : std_logic_vector(11 downto 0);
        variable temp   : unsigned(bin'range);
    begin
        result := (others => '0');
        temp := bin;
        for i in 0 to bin'length-1 loop
            -- Add 3 to columns >= 5
            if unsigned(result(3 downto 0)) >= 5 then
                result(3 downto 0) := std_logic_vector(unsigned(result(3 downto 0)) + 3);
            end if;
            if unsigned(result(7 downto 4)) >= 5 then
                result(7 downto 4) := std_logic_vector(unsigned(result(7 downto 4)) + 3);
            end if;
            if unsigned(result(11 downto 8)) >= 5 then
                result(11 downto 8) := std_logic_vector(unsigned(result(11 downto 8)) + 3);
            end if;
            -- Shift left
            result := result(10 downto 0) & temp(temp'high);
            temp := temp(temp'high-1 downto 0) & '0';
        end loop;
        return result;
    end function to_bcd;

    -- Procedure implementation
    procedure wait_cycles(signal clk : in std_logic; n : natural) is
    begin
        for i in 1 to n loop
            wait until rising_edge(clk);
        end loop;
    end procedure wait_cycles;

    procedure reset_signals(signal rst : out std_logic; cycles : natural) is
    begin
        rst <= '1';
        wait for cycles * 10 ns;
        rst <= '0';
    end procedure reset_signals;

end package body TYPES_PKG;

-- ==============================================================================
-- Entity Declaration
-- ==============================================================================

entity ALU is
    generic (
        DATA_WIDTH : natural := 32;
        USE_DSP    : boolean := true;
        PIPELINE   : natural range 0 to 3 := 1
    );
    port (
        -- Clock and reset
        clk     : in  std_logic;
        rst_n   : in  std_logic;  -- Active low reset

        -- Control
        op      : in  std_logic_vector(2 downto 0);
        valid   : in  std_logic;

        -- Data inputs
        a       : in  std_logic_vector(DATA_WIDTH-1 downto 0);
        b       : in  std_logic_vector(DATA_WIDTH-1 downto 0);

        -- Data outputs
        result  : out std_logic_vector(DATA_WIDTH-1 downto 0);
        carry   : out std_logic;
        zero    : out std_logic;
        ready   : out std_logic
    );
end entity ALU;

-- ==============================================================================
-- Architecture
-- ==============================================================================

architecture RTL of ALU is

    -- Signals
    signal a_reg, b_reg : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal result_int   : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal sum_ext      : std_logic_vector(DATA_WIDTH downto 0);
    signal op_reg       : std_logic_vector(2 downto 0);
    signal valid_d      : std_logic_vector(PIPELINE downto 0);

    -- Constants
    constant OP_ADD : std_logic_vector(2 downto 0) := "000";
    constant OP_SUB : std_logic_vector(2 downto 0) := "001";
    constant OP_AND : std_logic_vector(2 downto 0) := "010";
    constant OP_OR  : std_logic_vector(2 downto 0) := "011";
    constant OP_XOR : std_logic_vector(2 downto 0) := "100";
    constant OP_SHL : std_logic_vector(2 downto 0) := "101";
    constant OP_SHR : std_logic_vector(2 downto 0) := "110";
    constant OP_NOP : std_logic_vector(2 downto 0) := "111";

    -- Aliases
    alias op_code is op_reg;

    -- Attributes
    attribute DONT_TOUCH : string;
    attribute DONT_TOUCH of result_int : signal is "TRUE";

    attribute USE_DSP48 : string;
    attribute USE_DSP48 of sum_ext : signal is "YES";

begin

    -- ===========================================================================
    -- Input Register Stage
    -- ===========================================================================

    input_reg: process(clk, rst_n)
    begin
        if rst_n = '0' then
            a_reg    <= (others => '0');
            b_reg    <= (others => '0');
            op_reg   <= (others => '0');
            valid_d  <= (others => '0');
        elsif rising_edge(clk) then
            a_reg <= a;
            b_reg <= b;
            op_reg <= op;
            valid_d <= valid_d(PIPELINE-1 downto 0) & valid;
        end if;
    end process input_reg;

    -- ===========================================================================
    -- ALU Operations
    -- ===========================================================================

    alu_proc: process(clk)
        variable temp_result : unsigned(DATA_WIDTH downto 0);
    begin
        if rising_edge(clk) then
            case op_reg is
                when OP_ADD =>
                    temp_result := ('0' & unsigned(a_reg)) + ('0' & unsigned(b_reg));
                    result_int <= std_logic_vector(temp_result(DATA_WIDTH-1 downto 0));
                    carry <= temp_result(DATA_WIDTH);

                when OP_SUB =>
                    temp_result := ('0' & unsigned(a_reg)) - ('0' & unsigned(b_reg));
                    result_int <= std_logic_vector(temp_result(DATA_WIDTH-1 downto 0));
                    carry <= temp_result(DATA_WIDTH);

                when OP_AND =>
                    result_int <= a_reg and b_reg;
                    carry <= '0';

                when OP_OR =>
                    result_int <= a_reg or b_reg;
                    carry <= '0';

                when OP_XOR =>
                    result_int <= a_reg xor b_reg;
                    carry <= '0';

                when OP_SHL =>
                    result_int <= std_logic_vector(
                        shift_left(unsigned(a_reg),
                        to_integer(unsigned(b_reg(4 downto 0))))
                    );
                    carry <= '0';

                when OP_SHR =>
                    result_int <= std_logic_vector(
                        shift_right(unsigned(a_reg),
                        to_integer(unsigned(b_reg(4 downto 0))))
                    );
                    carry <= '0';

                when others =>
                    result_int <= (others => '0');
                    carry <= '0';
            end case;
        end if;
    end process alu_proc;

    -- ===========================================================================
    -- Output Assignments
    -- ===========================================================================

    result <= result_int;
    zero   <= '1' when result_int = (result_int'range => '0') else '0';
    ready  <= valid_d(PIPELINE);

end architecture RTL;

-- ==============================================================================
-- State Machine Entity
-- ==============================================================================

entity UART_TX is
    generic (
        CLK_FREQ  : natural := 100_000_000;
        BAUD_RATE : natural := 115200
    );
    port (
        clk     : in  std_logic;
        rst     : in  std_logic;
        data    : in  std_logic_vector(7 downto 0);
        start   : in  std_logic;
        tx      : out std_logic;
        busy    : out std_logic
    );
end entity UART_TX;

architecture BEHAVIORAL of UART_TX is

    -- State machine type
    type TX_STATE is (S_IDLE, S_START, S_DATA, S_STOP);
    signal state : TX_STATE := S_IDLE;

    -- Baud rate generation
    constant CLKS_PER_BIT : natural := CLK_FREQ / BAUD_RATE;
    signal baud_cnt : natural range 0 to CLKS_PER_BIT - 1 := 0;
    signal baud_tick : std_logic := '0';

    -- Data registers
    signal shift_reg : std_logic_vector(7 downto 0);
    signal bit_cnt : natural range 0 to 7 := 0;

begin

    -- Baud rate generator
    baud_gen: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' or state = S_IDLE then
                baud_cnt <= 0;
                baud_tick <= '0';
            elsif baud_cnt = CLKS_PER_BIT - 1 then
                baud_cnt <= 0;
                baud_tick <= '1';
            else
                baud_cnt <= baud_cnt + 1;
                baud_tick <= '0';
            end if;
        end if;
    end process baud_gen;

    -- State machine
    fsm: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                state <= S_IDLE;
                shift_reg <= (others => '0');
                bit_cnt <= 0;
                tx <= '1';
                busy <= '0';
            else
                case state is
                    when S_IDLE =>
                        tx <= '1';
                        busy <= '0';
                        if start = '1' then
                            shift_reg <= data;
                            state <= S_START;
                            busy <= '1';
                        end if;

                    when S_START =>
                        tx <= '0';  -- Start bit
                        if baud_tick = '1' then
                            state <= S_DATA;
                            bit_cnt <= 0;
                        end if;

                    when S_DATA =>
                        tx <= shift_reg(0);
                        if baud_tick = '1' then
                            shift_reg <= '0' & shift_reg(7 downto 1);
                            if bit_cnt = 7 then
                                state <= S_STOP;
                            else
                                bit_cnt <= bit_cnt + 1;
                            end if;
                        end if;

                    when S_STOP =>
                        tx <= '1';  -- Stop bit
                        if baud_tick = '1' then
                            state <= S_IDLE;
                        end if;

                    when others =>
                        state <= S_IDLE;
                end case;
            end if;
        end if;
    end process fsm;

end architecture BEHAVIORAL;

-- ==============================================================================
-- Generate Statements
-- ==============================================================================

entity REGISTER_FILE is
    generic (
        NUM_REGS  : natural := 32;
        REG_WIDTH : natural := 32
    );
    port (
        clk      : in  std_logic;
        we       : in  std_logic;
        addr_wr  : in  std_logic_vector(4 downto 0);
        addr_rd1 : in  std_logic_vector(4 downto 0);
        addr_rd2 : in  std_logic_vector(4 downto 0);
        data_in  : in  std_logic_vector(REG_WIDTH-1 downto 0);
        data_out1: out std_logic_vector(REG_WIDTH-1 downto 0);
        data_out2: out std_logic_vector(REG_WIDTH-1 downto 0)
    );
end entity REGISTER_FILE;

architecture STRUCTURAL of REGISTER_FILE is

    type REG_ARRAY is array (0 to NUM_REGS-1) of std_logic_vector(REG_WIDTH-1 downto 0);
    signal registers : REG_ARRAY := (others => (others => '0'));

begin

    -- Generate registers with for-generate
    reg_gen: for i in 0 to NUM_REGS-1 generate

        -- Conditional generate for register 0 (hardwired to zero)
        reg0_gen: if i = 0 generate
            registers(0) <= (others => '0');
        end generate reg0_gen;

        -- Other registers
        other_reg_gen: if i /= 0 generate
            process(clk)
            begin
                if rising_edge(clk) then
                    if we = '1' and to_integer(unsigned(addr_wr)) = i then
                        registers(i) <= data_in;
                    end if;
                end if;
            end process;
        end generate other_reg_gen;

    end generate reg_gen;

    -- Read ports (combinational)
    data_out1 <= registers(to_integer(unsigned(addr_rd1)));
    data_out2 <= registers(to_integer(unsigned(addr_rd2)));

end architecture STRUCTURAL;

-- ==============================================================================
-- Block Statement
-- ==============================================================================

architecture BLOCK_DEMO of ALU is
begin

    -- Block with guard
    protected_block: block (valid = '1')
    begin
        result <= guarded a when op = "00" else
                  guarded b when op = "01" else
                  guarded (others => '0');
    end block protected_block;

    -- Block with local declarations
    computation: block
        signal internal : std_logic_vector(DATA_WIDTH-1 downto 0);
        constant MASK : std_logic_vector(DATA_WIDTH-1 downto 0) := x"FFFF0000";
    begin
        internal <= a and MASK;
        result <= internal xor b;
    end block computation;

end architecture BLOCK_DEMO;

-- ==============================================================================
-- Assertions and PSL
-- ==============================================================================

architecture VERIFIED of UART_TX is
begin

    -- Concurrent assertions
    assert_no_x: assert not is_x(data)
        report "Data contains X values" severity warning;

    -- PSL assertions (if supported)
    -- psl default clock is rising_edge(clk);
    -- psl assert always (start -> eventually! busy);
    -- psl cover {start; [*]; not busy};

    -- Check for valid state transitions
    check_fsm: process(clk)
    begin
        if rising_edge(clk) then
            assert not (state = S_IDLE and busy = '1')
                report "Invalid state: IDLE with busy"
                severity error;
        end if;
    end process check_fsm;

end architecture VERIFIED;

-- ==============================================================================
-- Configuration
-- ==============================================================================

configuration CFG_ALU of ALU is
    for RTL
        -- Configure components here
    end for;
end configuration CFG_ALU;

-- ==============================================================================
-- Testbench
-- ==============================================================================

entity TB_ALU is
    -- Empty entity for testbench
end entity TB_ALU;

architecture SIM of TB_ALU is

    -- Constants
    constant CLK_PERIOD : time := 10 ns;
    constant DATA_WIDTH : natural := 32;

    -- Signals
    signal clk     : std_logic := '0';
    signal rst_n   : std_logic := '0';
    signal op      : std_logic_vector(2 downto 0) := (others => '0');
    signal valid   : std_logic := '0';
    signal a, b    : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');
    signal result  : std_logic_vector(DATA_WIDTH-1 downto 0);
    signal carry   : std_logic;
    signal zero    : std_logic;
    signal ready   : std_logic;

    -- Shared variable for coverage
    shared variable test_count : natural := 0;

begin

    -- DUT instantiation
    dut: entity work.ALU
        generic map (
            DATA_WIDTH => DATA_WIDTH,
            USE_DSP    => true,
            PIPELINE   => 1
        )
        port map (
            clk    => clk,
            rst_n  => rst_n,
            op     => op,
            valid  => valid,
            a      => a,
            b      => b,
            result => result,
            carry  => carry,
            zero   => zero,
            ready  => ready
        );

    -- Clock generation
    clk <= not clk after CLK_PERIOD / 2;

    -- Stimulus process
    stim: process
        -- Procedure for applying test
        procedure apply_test(
            test_a  : std_logic_vector(DATA_WIDTH-1 downto 0);
            test_b  : std_logic_vector(DATA_WIDTH-1 downto 0);
            test_op : std_logic_vector(2 downto 0)
        ) is
        begin
            a <= test_a;
            b <= test_b;
            op <= test_op;
            valid <= '1';
            wait until rising_edge(clk);
            valid <= '0';
            wait until ready = '1';
            wait until rising_edge(clk);
            test_count := test_count + 1;
        end procedure;

    begin
        -- Reset
        rst_n <= '0';
        wait for 100 ns;
        rst_n <= '1';
        wait for 20 ns;

        -- Test ADD
        apply_test(x"00000005", x"00000003", "000");
        assert result = x"00000008"
            report "ADD test failed" severity error;

        -- Test SUB
        apply_test(x"0000000A", x"00000003", "001");
        assert result = x"00000007"
            report "SUB test failed" severity error;

        -- Test AND
        apply_test(x"0F0F0F0F", x"00FF00FF", "010");
        assert result = x"000F000F"
            report "AND test failed" severity error;

        -- Report
        report "All tests passed! Count: " & integer'image(test_count);

        -- End simulation
        wait for 100 ns;
        report "Simulation complete" severity note;
        std.env.stop;
        wait;
    end process stim;

end architecture SIM;

-- ==============================================================================
-- VHDL-2008 Features
-- ==============================================================================

-- Conditional signal assignment (VHDL-2008)
-- result <= a + b when op = "00" else
--           a - b when op = "01" else
--           a and b when op = "10" else
--           (others => '0');

-- Selected signal assignment (VHDL-2008)
-- with op select result <=
--     a + b       when "00",
--     a - b       when "01",
--     a and b     when "10",
--     (others => '0') when others;

-- Process with sensitivity list all (VHDL-2008)
-- comb_proc: process(all)
-- begin
--     result <= a + b;
-- end process;

-- External names (VHDL-2008)
-- alias tb_result is <<signal .tb_alu.dut.result_int : std_logic_vector>>;
