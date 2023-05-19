package Decoder

import chisel3._
import scala.annotation.varargs

class Decoder extends Module {
  val io = IO(new Bundle {
    // For FetchUnit
    val IR = Input(UInt(32.W))

    // For Register File
    val rDataA = Input(UInt(32.W))
    val rDataB = Input(UInt(32.W))
    val rDataC = Input(UInt(32.W))
    val rAddrA = Output(UInt(4.W))
    val rAddrB = Output(UInt(4.W))
    val rAddrC = Output(UInt(4.W))
    val wAddr = Output(UInt(4.W))
    val wData = Output(UInt(32.W))

    // For ALU
    val aluA = Output(UInt(32.W))
    val aluB = Output(UInt(32.W))
    val aluOp = Output(UInt(4.W))
    val aluShiftOp = Output(UInt(3.W))
    val aluShiftNum = Output(UInt(8.W))
    val aluOut = Input(UInt(32.W))

    // For NZCV
    val nzcvEN = Output(Bool())

    val err = Output(Bool())
  })

  val I = io.IR
  io.nzcvEN := I(20)
  io.err := false.B

  // some default values to pass the compilation
  // our code won't run depending the default values (not like io.err)
  io.rAddrA := 0.U
  io.rAddrB := 0.U
  io.rAddrC := 0.U
  io.aluA := 0.U
  io.aluB := 0.U
  io.aluOp := 0.U
  io.aluShiftOp := 0.U
  io.aluShiftNum := 0.U
  io.wData := 0.U
  io.wAddr := 0.U

  @varargs def matchE(handles: (Bool, () => Any)*): Unit = {
    handles.foldLeft(when(false.B) {}) { (whens, handle) =>
      whens.elsewhen(handle._1)(handle._2())
    }.otherwise {
      io.err := true.B
    }
  }

  matchE(
    (I(27, 26) === "b00".U) -> (()=> matchE(
      // Data-processing and miscellaneous instructions
      (!(I(24, 23) === "b10".U && I(20) === 0.B)) -> {()=>
        // Data-processing
        io.rAddrA := I(19, 16)
        io.aluA := io.rDataA
        io.aluOp := I(24, 21)

        io.wAddr := I(15, 12)
        io.wData := io.aluOut

        matchE(
          (I(25) === 0.B) -> {()=>
            io.rAddrB := I(3, 0)
            io.aluB := io.rDataB
            io.aluShiftOp := I(6, 4)
            matchE(
              (I(4) === 0.B) -> {()=>
                // Data-processing (register)
                io.aluShiftNum := I(11, 7)
              },
              (I(7) === 0.B && I(4) === 1.B) -> { () =>
                // Data-processing (register-shifted register)
                io.aluShiftNum := I(11, 8)
              })
          },
          (I(25) === 1.B) -> {()=>
            // Data-processing (immediate)
            io.aluB := I(7, 0)
            io.aluShiftOp := "b111".U
            io.aluShiftNum := I(11, 8)
          }
        )
      }
    ))
  )
}