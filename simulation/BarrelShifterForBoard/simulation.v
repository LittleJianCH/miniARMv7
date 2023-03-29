`timescale 10ns/10ns
`include "../../gen/BarrelShifterForBoard.v"

module simulation;
  reg CP, reset;
  reg OP_loader, Data_loader, Num_loader, Carry_Flag_loader, Output_Chooser;
  reg [31:0] data;
  wire [31:0] out;
  wire [7:0] display_seg;
  wire display_en;
  wire [2:0] display_ctrl; 
  BarrelShifterForBoard BS(CP, reset, 
                           OP_loader, Data_loader, Num_loader, Carry_Flag_loader, 
                           Output_Chooser, 
                           data, out, 
                           display_seg, display_en, display_ctrl);
  
  always #2 CP = !CP;

  initial begin
    $dumpfile("wave.vcd");
    $dumpvars(0, simulation);

    CP = 0; reset = 1; #30 reset = 0; #30

    Output_Chooser = 0; #30
    Data_loader = 1; data = 32'h70020402; #30 Data_loader = 0; #30
    Num_loader = 1; data = 32'h00000000; #30 Num_loader = 0; #30
    OP_loader = 1; data = 32'h00000001; #30 OP_loader = 0; #30
    
    $finish;
  end

endmodule
