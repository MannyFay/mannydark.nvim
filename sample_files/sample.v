// ==============================================================================
// Comprehensive Verilog Sample - Syntax Highlighting Demonstration
// ==============================================================================

// This file demonstrates all major Verilog (IEEE 1364) language features
// for syntax highlighting purposes.

// ==============================================================================
// Comments
// ==============================================================================

// Single line comment

/* Multi-line
   block comment */

/* Nested /* comments */ are NOT supported in standard Verilog */

// ==============================================================================
// Compiler Directives
// ==============================================================================

`timescale 1ns / 1ps
`default_nettype none

`define DATA_WIDTH 32
`define ADDR_WIDTH 16
`define CLK_PERIOD 10

`ifdef SIMULATION
    `define DEBUG 1
`else
    `define DEBUG 0
`endif

`ifndef SYNTHESIS
    `define ASSERT_ON
`endif

// ==============================================================================
// Module Declaration
// ==============================================================================

module alu #(
    parameter DATA_WIDTH = 32,
    parameter USE_DSP = 1,
    parameter PIPELINE = 1
) (
    // Clock and reset
    input  wire                     clk,
    input  wire                     rst_n,

    // Control
    input  wire [2:0]               op,
    input  wire                     valid,

    // Data inputs
    input  wire [DATA_WIDTH-1:0]    a,
    input  wire [DATA_WIDTH-1:0]    b,

    // Data outputs
    output reg  [DATA_WIDTH-1:0]    result,
    output reg                      carry,
    output wire                     zero,
    output reg                      ready
);

// ==============================================================================
// Local Parameters and Constants
// ==============================================================================

localparam OP_ADD = 3'b000;
localparam OP_SUB = 3'b001;
localparam OP_AND = 3'b010;
localparam OP_OR  = 3'b011;
localparam OP_XOR = 3'b100;
localparam OP_SHL = 3'b101;
localparam OP_SHR = 3'b110;
localparam OP_NOP = 3'b111;

// ==============================================================================
// Wire and Reg Declarations
// ==============================================================================

// Registers
reg [DATA_WIDTH-1:0] a_reg, b_reg;
reg [2:0] op_reg;
reg [PIPELINE:0] valid_d;

// Wires
wire [DATA_WIDTH:0] sum_ext;
wire [DATA_WIDTH-1:0] result_int;

// Integer for loops
integer i, j;
genvar g;

// Real for calculations
real clock_freq = 100.0e6;
realtime delay;

// ==============================================================================
// Input Register Stage
// ==============================================================================

always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        a_reg   <= {DATA_WIDTH{1'b0}};
        b_reg   <= {DATA_WIDTH{1'b0}};
        op_reg  <= 3'b0;
        valid_d <= {(PIPELINE+1){1'b0}};
    end else begin
        a_reg   <= a;
        b_reg   <= b;
        op_reg  <= op;
        valid_d <= {valid_d[PIPELINE-1:0], valid};
    end
end

// ==============================================================================
// ALU Operations
// ==============================================================================

always @(posedge clk) begin
    case (op_reg)
        OP_ADD: begin
            {carry, result} <= a_reg + b_reg;
        end

        OP_SUB: begin
            {carry, result} <= a_reg - b_reg;
        end

        OP_AND: begin
            result <= a_reg & b_reg;
            carry  <= 1'b0;
        end

        OP_OR: begin
            result <= a_reg | b_reg;
            carry  <= 1'b0;
        end

        OP_XOR: begin
            result <= a_reg ^ b_reg;
            carry  <= 1'b0;
        end

        OP_SHL: begin
            result <= a_reg << b_reg[4:0];
            carry  <= 1'b0;
        end

        OP_SHR: begin
            result <= a_reg >> b_reg[4:0];
            carry  <= 1'b0;
        end

        default: begin
            result <= {DATA_WIDTH{1'b0}};
            carry  <= 1'b0;
        end
    endcase
end

// ==============================================================================
// Combinational Logic
// ==============================================================================

// Continuous assignment
assign zero = (result == {DATA_WIDTH{1'b0}});
assign sum_ext = {1'b0, a_reg} + {1'b0, b_reg};

// Conditional operator
assign result_int = valid ? (a_reg + b_reg) : {DATA_WIDTH{1'b0}};

// Always block for ready signal
always @(posedge clk or negedge rst_n) begin
    if (!rst_n)
        ready <= 1'b0;
    else
        ready <= valid_d[PIPELINE];
end

endmodule

// ==============================================================================
// UART Transmitter Module
// ==============================================================================

module uart_tx #(
    parameter CLK_FREQ  = 100_000_000,
    parameter BAUD_RATE = 115200
) (
    input  wire       clk,
    input  wire       rst,
    input  wire [7:0] data,
    input  wire       start,
    output reg        tx,
    output reg        busy
);

// State encoding
localparam [1:0] S_IDLE  = 2'b00;
localparam [1:0] S_START = 2'b01;
localparam [1:0] S_DATA  = 2'b10;
localparam [1:0] S_STOP  = 2'b11;

// Calculated parameters
localparam CLKS_PER_BIT = CLK_FREQ / BAUD_RATE;
localparam CNT_WIDTH = $clog2(CLKS_PER_BIT);

// Registers
reg [1:0] state, next_state;
reg [CNT_WIDTH-1:0] baud_cnt;
reg [7:0] shift_reg;
reg [2:0] bit_cnt;
reg baud_tick;

// Baud rate generator
always @(posedge clk) begin
    if (rst || state == S_IDLE) begin
        baud_cnt  <= 0;
        baud_tick <= 1'b0;
    end else if (baud_cnt == CLKS_PER_BIT - 1) begin
        baud_cnt  <= 0;
        baud_tick <= 1'b1;
    end else begin
        baud_cnt  <= baud_cnt + 1;
        baud_tick <= 1'b0;
    end
end

// State register
always @(posedge clk) begin
    if (rst)
        state <= S_IDLE;
    else
        state <= next_state;
end

// Next state logic
always @(*) begin
    next_state = state;
    case (state)
        S_IDLE:  if (start)                 next_state = S_START;
        S_START: if (baud_tick)             next_state = S_DATA;
        S_DATA:  if (baud_tick && bit_cnt == 7) next_state = S_STOP;
        S_STOP:  if (baud_tick)             next_state = S_IDLE;
        default: next_state = S_IDLE;
    endcase
end

// Output logic
always @(posedge clk) begin
    if (rst) begin
        tx        <= 1'b1;
        busy      <= 1'b0;
        shift_reg <= 8'b0;
        bit_cnt   <= 3'b0;
    end else begin
        case (state)
            S_IDLE: begin
                tx   <= 1'b1;
                busy <= 1'b0;
                if (start) begin
                    shift_reg <= data;
                    busy      <= 1'b1;
                end
            end

            S_START: begin
                tx <= 1'b0;
            end

            S_DATA: begin
                tx <= shift_reg[0];
                if (baud_tick) begin
                    shift_reg <= {1'b0, shift_reg[7:1]};
                    bit_cnt   <= bit_cnt + 1;
                end
            end

            S_STOP: begin
                tx      <= 1'b1;
                bit_cnt <= 3'b0;
            end
        endcase
    end
end

endmodule

// ==============================================================================
// Memory Module with Generate
// ==============================================================================

module memory #(
    parameter DEPTH = 1024,
    parameter WIDTH = 32,
    parameter INIT_FILE = ""
) (
    input  wire                    clk,
    input  wire                    we,
    input  wire [$clog2(DEPTH)-1:0] addr,
    input  wire [WIDTH-1:0]        din,
    output reg  [WIDTH-1:0]        dout
);

// Memory array
reg [WIDTH-1:0] mem [0:DEPTH-1];

// Initialize memory
initial begin
    if (INIT_FILE != "") begin
        $readmemh(INIT_FILE, mem);
    end else begin
        for (integer k = 0; k < DEPTH; k = k + 1) begin
            mem[k] = {WIDTH{1'b0}};
        end
    end
end

// Write logic
always @(posedge clk) begin
    if (we)
        mem[addr] <= din;
end

// Read logic
always @(posedge clk) begin
    dout <= mem[addr];
end

endmodule

// ==============================================================================
// Generate Constructs
// ==============================================================================

module shift_register #(
    parameter WIDTH = 8,
    parameter DEPTH = 4
) (
    input  wire             clk,
    input  wire             rst,
    input  wire             en,
    input  wire [WIDTH-1:0] din,
    output wire [WIDTH-1:0] dout
);

wire [WIDTH-1:0] stage [0:DEPTH];

assign stage[0] = din;
assign dout = stage[DEPTH];

// Generate shift stages
generate
    genvar i;
    for (i = 0; i < DEPTH; i = i + 1) begin : shift_stage
        reg [WIDTH-1:0] stage_reg;

        always @(posedge clk) begin
            if (rst)
                stage_reg <= {WIDTH{1'b0}};
            else if (en)
                stage_reg <= stage[i];
        end

        assign stage[i+1] = stage_reg;
    end
endgenerate

// Conditional generate
generate
    if (DEPTH > 8) begin : deep_pipeline
        // Extra buffering for deep pipelines
        reg [WIDTH-1:0] extra_reg;
        always @(posedge clk) begin
            if (rst)
                extra_reg <= {WIDTH{1'b0}};
            else
                extra_reg <= dout;
        end
    end
endgenerate

endmodule

// ==============================================================================
// Functions and Tasks
// ==============================================================================

module functions_demo (
    input  wire [31:0] a,
    input  wire [31:0] b,
    output reg  [31:0] result
);

// Function definition
function [31:0] add;
    input [31:0] x;
    input [31:0] y;
    begin
        add = x + y;
    end
endfunction

function automatic [31:0] factorial;
    input [31:0] n;
    begin
        if (n <= 1)
            factorial = 1;
        else
            factorial = n * factorial(n - 1);
    end
endfunction

function integer log2;
    input integer value;
    begin
        log2 = 0;
        while (value > 1) begin
            value = value >> 1;
            log2 = log2 + 1;
        end
    end
endfunction

// Task definition
task automatic delay_cycles;
    input integer n;
    begin
        repeat (n) @(posedge clk);
    end
endtask

task display_result;
    input [31:0] val;
    begin
        $display("Result = %d (0x%h)", val, val);
    end
endtask

// Using functions
always @(*) begin
    result = add(a, b);
end

endmodule

// ==============================================================================
// Primitives and UDP
// ==============================================================================

// Built-in primitives
module primitives_demo (
    input  wire a, b, c,
    output wire y1, y2, y3
);

and  u1 (y1, a, b);
or   u2 (y2, a, b, c);
not  u3 (y3, a);

// Tri-state buffer
wire enable = 1'b1;
bufif1 u4 (y1, a, enable);

endmodule

// User-defined primitive (UDP)
primitive mux2to1 (
    output y,
    input  a, b, sel
);

table
    // a  b  sel : y
       0  ?  0   : 0;
       1  ?  0   : 1;
       ?  0  1   : 0;
       ?  1  1   : 1;
       0  0  x   : 0;
       1  1  x   : 1;
endtable

endprimitive

// ==============================================================================
// Testbench Features
// ==============================================================================

module tb_alu;

// Parameters
parameter DATA_WIDTH = 32;
parameter CLK_PERIOD = 10;

// Signals
reg                     clk;
reg                     rst_n;
reg  [2:0]              op;
reg                     valid;
reg  [DATA_WIDTH-1:0]   a, b;
wire [DATA_WIDTH-1:0]   result;
wire                    carry;
wire                    zero;
wire                    ready;

// Events
event test_done;
event error_detected;

// Clock generation
initial begin
    clk = 0;
    forever #(CLK_PERIOD/2) clk = ~clk;
end

// DUT instantiation
alu #(
    .DATA_WIDTH(DATA_WIDTH),
    .USE_DSP(1),
    .PIPELINE(1)
) dut (
    .clk(clk),
    .rst_n(rst_n),
    .op(op),
    .valid(valid),
    .a(a),
    .b(b),
    .result(result),
    .carry(carry),
    .zero(zero),
    .ready(ready)
);

// Stimulus
initial begin
    // Initialize
    rst_n = 0;
    op = 0;
    valid = 0;
    a = 0;
    b = 0;

    // Reset
    #100;
    rst_n = 1;
    #20;

    // Test ADD
    @(posedge clk);
    a = 32'h00000005;
    b = 32'h00000003;
    op = 3'b000;
    valid = 1;
    @(posedge clk);
    valid = 0;

    // Wait for result
    @(posedge ready);
    @(posedge clk);

    // Check result
    if (result !== 32'h00000008) begin
        $display("ERROR: ADD test failed! Expected 8, got %d", result);
        -> error_detected;
    end else begin
        $display("PASS: ADD test");
    end

    // More tests...
    repeat (10) @(posedge clk);

    -> test_done;
end

// Timeout watchdog
initial begin
    #10000;
    $display("ERROR: Timeout!");
    $finish;
end

// Test completion
initial begin
    @(test_done);
    $display("All tests completed!");
    $finish;
end

// Waveform dump
initial begin
    $dumpfile("tb_alu.vcd");
    $dumpvars(0, tb_alu);
end

// Monitor
initial begin
    $monitor("Time=%0t a=%h b=%h op=%b result=%h",
             $time, a, b, op, result);
end

// Assertions (Verilog-2001 style)
always @(posedge clk) begin
    if (rst_n && valid) begin
        if ($isunknown(a) || $isunknown(b)) begin
            $display("WARNING: Unknown values on inputs");
        end
    end
end

// Coverage (simulation only)
`ifdef COVERAGE
covergroup cg_alu @(posedge clk iff valid);
    op_cp: coverpoint op {
        bins add = {3'b000};
        bins sub = {3'b001};
        bins logical = {[3'b010:3'b100]};
        bins shift = {[3'b101:3'b110]};
    }
endgroup

cg_alu cg_inst = new();
`endif

endmodule

// ==============================================================================
// System Tasks and Functions
// ==============================================================================

module system_tasks_demo;

reg [31:0] value;
reg [7:0] mem [0:255];
integer file_handle;
reg [8*80:1] line;

initial begin
    // Display tasks
    $display("Simple display");
    $display("Formatted: %d %h %b %s", 42, 42, 42, "hello");
    $write("No newline");
    $strobe("Strobe at end of time step");
    $monitor("Monitoring value: %d", value);

    // File I/O
    file_handle = $fopen("output.txt", "w");
    $fdisplay(file_handle, "Writing to file");
    $fclose(file_handle);

    file_handle = $fopen("input.txt", "r");
    while (!$feof(file_handle)) begin
        $fgets(line, file_handle);
    end
    $fclose(file_handle);

    // Memory operations
    $readmemh("data.hex", mem);
    $readmemb("data.bin", mem);
    $writememh("out.hex", mem);

    // Random numbers
    value = $random;
    value = $urandom;
    value = $urandom_range(100, 0);

    // Timing
    #10;
    $display("Time = %0t", $time);
    $display("Realtime = %0t", $realtime);

    // Simulation control
    $stop;
    $finish;
end

endmodule

// ==============================================================================
// Attributes
// ==============================================================================

(* dont_touch = "true" *)
module attributed_module (
    (* mark_debug = "true" *) input wire clk,
    (* keep = "true" *) input wire data,
    output wire result
);

(* full_case, parallel_case *)
reg [1:0] state;

(* use_dsp = "yes" *)
wire [31:0] product;

assign result = data;

endmodule

`default_nettype wire
