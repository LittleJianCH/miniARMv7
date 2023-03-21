`timescale 10ns/10ns
`include "../../gen/BarrelShifter.v"

module simulation;
  reg CP, reset;
  reg [2:0] io_Shift_OP;
  reg [31:0] io_Shift_Data;
  reg [7:0] io_Shift_Num;
  reg io_Carry_Flag;
  wire [31:0] io_Shift_Out;
  wire io_Shift_Carry_Out;
  BarrelShifter BS(CP, reset, 
                   io_Shift_OP, io_Shift_Data, 
                   io_Shift_Num, io_Carry_Flag, 
                   io_Shift_Out, io_Shift_Carry_Out);

  always #20 CP = !CP;

  initial begin
    $dumpfile("wave.vcd");
    $dumpvars(0, simulation);

    CP = 0; reset = 1; #30 reset = 0;

    // lsl
    io_Shift_OP = 3'b001;
    io_Shift_Data = 32'h12345678;
    io_Shift_Num = 0;
    io_Carry_Flag = 1'b0;
    #10

    io_Shift_Num = 4;
    #10

    io_Shift_Num = 100;
    #10

    io_Shift_Num = 0;
    io_Shift_OP = 3'b000;
    #10

    // lsr
    io_Shift_OP = 3'b011;
    io_Shift_Data = 32'h12345678;
    io_Shift_Num = 0;
    io_Carry_Flag = 1'b0;
    #10

    io_Shift_Num = 4;
    #10

    io_Shift_Num = 100;
    #10

    io_Shift_OP = 3'b010;
    io_Shift_Num = 0;
    #10

    // asr
    io_Shift_OP = 3'b101;
    io_Shift_Data = 32'h12345678;
    io_Shift_Num = 0;
    io_Carry_Flag = 1'b0;
    #10
    
    io_Shift_Num = 8;
    #10

    io_Shift_Data = 32'h80000000;
    io_Shift_Num = 4;
    #10

    io_Shift_Num = 100;
    #10

    // ror
    io_Shift_OP = 3'b111;
    io_Shift_Data = 32'h12345678;
    io_Shift_Num = 0;
    io_Carry_Flag = 1'b0;
    #10

    io_Shift_Num = 4;
    #10

    io_Shift_Num = 100;
    #10

    io_Shift_Num = 0;
    io_Shift_Data = 32'h00000001;
    io_Shift_OP = 3'b110;
    io_Carry_Flag = 1'b1;
    #10

    $finish;
  end

endmodule
