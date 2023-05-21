`timescale 10ns/10ns
`include "../../gen/CPU.v"

module simulation;
  reg CP, reset;
  wire writePC, writeIR, writeReg;
  wire [31:0] A, B, C, F, IR;
  wire [7:0] PC;
  wire [3:0] nzcv;
  wire done;

  CPU cpu(CP, reset, writePC, writeIR, writeReg, A, B, C, F, IR, PC, nzcv, done);

  always #10 CP = !CP;

  initial begin
    $dumpfile("wave.vcd");
    $dumpvars(0, simulation);

    CP = 0; reset = 1; #30 reset = 0;

    #30000 $finish;
  end
endmodule
