`timescale 10ns/10ns
`include "../../gen/RegisterFile.v"

module simulation;
  reg CP, reset;
  reg [4:0] mode;
  reg [31:0] dataW, dataPC;
  wire [31:0] dataA, dataB, dataC;
  reg [3:0] addrA, addrB, addrC, addrW;
  reg regW, regPC;
  wire err;
  RegisterFile RF(CP, reset, mode,
                  addrA, addrB, addrC, addrW,
                  dataW, regW, dataPC, regPC, err, dataA, dataB, dataC);
  
  always #5 CP = !CP;

  initial begin
    $dumpfile("wave.vcd");
    $dumpvars(0, simulation);

    CP = 0; 
    reset = 1; #10 reset = 0; #10

    mode = 5'b10000; 

    addrA = 0; addrB = 8; addrC = 15;

    dataPC = 22; regPC = 1; dataW = 123; regW = 1; addrW = 0; #30

    dataPC = 23; regPC = 1; dataW = 456; regW = 1; addrW = 8; #30

    regW = 0; mode = 5'b10001; #30

    dataPC = 24; regPC = 1; dataW = 789; regW = 1; addrW = 8; #30

    dataPC = 25; regPC = 1; dataW = 333; regW = 1; addrW = 15; #30

    mode = 5'b11010;

    dataW = 123; regW = 1; addrW = 14; #30

    $finish;
  end
endmodule
