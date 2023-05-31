`timescale 10ns/10ns
`include "../../gen/RAM.v"

module simulation;
  reg clock, reset, wEN, rEN;
  reg [31:0] addrR, addrW, dataW;
  wire [31:0] dataR;

  RAM ram(
    .clock(clock),
    .reset(reset),
    .io_wEN(wEN),
    .io_rEN(rEN),
    .io_addrR(addrR),
    .io_addrW(addrW),
    .io_dataR(dataR),
    .io_dataW(dataW)
  );

  always #5 clock = !clock;

  initial begin
    $dumpfile("wave.vcd");
    $dumpvars(0, simulation);

    clock = 0; reset = 1; rEN = 0; wEN = 0; #10; reset = 0;

    addrW = 0; dataW = 123; wEN = 1; #20 wEN = 0;

    addrR = 0; rEN = 1; #20 rEN = 0;

    addrW = 1; dataW = 456; wEN = 1; #20 wEN = 0;

    addrR = 1; rEN = 1; #20 rEN = 0;

    addrW = 2; dataW = 789; wEN = 1; addrR = 2; rEN = 1; #20 wEN = 0; rEN = 0;

    $finish;
  end
endmodule
