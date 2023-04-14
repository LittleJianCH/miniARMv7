`timescale 10ns/10ns
`include "../../gen/ALU.v"

module simulation;
  reg CP, reset;
  reg [31:0] a, b;
  reg [3:0] op;
  reg cin;
  reg [2:0] shift_op;
  reg [7:0] shift_num;
  wire [31:0] out;
  wire nout, zout, cout, vout;
  ALU alu(CP, reset,
          a, b, op, cin,
          shift_op, shift_num,
          out, nout, zout, cout, vout);

  always #20 CP = !CP;

  initial begin
    $dumpfile("wave.vcd");
    $dumpvars(0, simulation);

    CP = 0; reset = 1; #30 reset = 0;

    op = 12; a = 440694180; b = 1622033726; cin = 0; shift_op = 6; shift_num = 21; #30

    op = 10; a = 507760139; b = 4074572357; cin = 1; shift_op = 4; shift_num = 30; #30

    op = 2; a = 0; b = 1; cin = 0; shift_op = 0; shift_num = 0; #30

    op = 4; a = 4294967295; b = 1; cin = 0; shift_op = 0; shift_num = 0; #30

    op = 2; a = 18446744073249410691; b = 18446744073249410691; cin = 0; shift_op = 1; shift_num = 25; #30

    op = 7; a = 781146134; b = 18446744071946142749; cin = 0; shift_op = 7; shift_num = 8; #30

    op = 5; a = 1183074884; b = 1940992646; cin = 0; shift_op = 7; shift_num = 23; #30

    op = 3; a = 18446744073188910380; b = 336014124; cin = 0; shift_op = 1; shift_num = 26; #30

    op = 2; a = 566033422; b = 344196171; cin = 0; shift_op = 3; shift_num = 10; #30

    op = 3; a = 18446744073158875515; b = 410476580; cin = 1; shift_op = 1; shift_num = 20; #30

    op = 5; a = 18446744072914510396; b = 18446744071784364227; cin = 1; shift_op = 2; shift_num = 1; #30

    op = 10; a = 2075950654; b = 18446744072043747641; cin = 0; shift_op = 2; shift_num = 2; #30

    op = 4; a = 18446744073465807193; b = 18446744073140934943; cin = 0; shift_op = 1; shift_num = 22; #30

    $finish;
  end
endmodule
