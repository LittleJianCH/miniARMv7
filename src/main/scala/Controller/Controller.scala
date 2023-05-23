package Controller

import chisel3._
import chisel3.util._

class Controller extends Module {
  // Controller 负责处理所有与 IR, state 具体值相关的信号
  val io = IO(new Bundle {
    // state
    val state = Input(UInt(4.W))
    val done = Output(Bool())

    // For FetchUnit
    val IR = Input(UInt(32.W))
    val writeIR = Output(Bool())
    val cond = Input(Bool())

    // For Register File
    val rDataA = Input(UInt(32.W))
    val rDataB = Input(UInt(32.W))
    val rDataC = Input(UInt(32.W))
    val rAddrA = Output(UInt(4.W))
    val rAddrB = Output(UInt(4.W))
    val rAddrC = Output(UInt(4.W))
    val wAddr = Output(UInt(4.W))
    val writeR = Output(Bool())
    val writePC = Output(Bool())

    // For ALU
    val aluA = Output(UInt(32.W))
    val aluB = Output(UInt(32.W))
    val aluOp = Output(UInt(4.W))
    val aluShiftOp = Output(UInt(3.W))
    val aluShiftNum = Output(UInt(8.W))

    // For NZCV
    val writeNZCV = Output(Bool())

    val err = Output(Bool())
  })

  io.done := false.B
  io.writeIR := false.B
  io.rAddrA := 0.U
  io.rAddrB := 0.U
  io.rAddrC := 0.U
  io.wAddr := 0.U
  io.writeR := false.B
  io.writePC := false.B
  io.aluA := 0.U
  io.aluB := 0.U
  io.aluOp := 0.U
  io.aluShiftOp := 0.U
  io.aluShiftNum := 0.U
  io.writeNZCV := false.B
  io.err := false.B

  def CM(str: String, I: UInt): Bool = {
    // 判断 I 是否符合 str 模式串（x 为通配符）
    // 由于 instr_map 中会大量使用此函数，所以使用了较不可读的短函数名，意为 CheckMatch
    val n = str.length
    VecInit((0 until n).map { i =>
      (str(i) == 'x').B || ((str(i) == '1').B === I(n - i - 1))
    }).asUInt.andR
  }

  val instr_map: List[(UInt => Bool, (UInt, UInt) => Unit)] = List(
    ( // Data-processing (register)
      I => CM("000", I(27, 25)) &&
           (!CM("10xx0", I(24, 20))) &&
           CM("xxx0", I(7, 4)),
      (I, state) => {
        switch (state) {
          is (1.U) {
            io.rAddrA := I(19, 16)
            io.aluA := io.rDataA
            io.aluOp := I(24, 21)
            io.rAddrB := I(3, 0)
            io.aluB := io.rDataB
            io.aluShiftOp := I(6, 4)
            io.aluShiftNum := I(11, 7)

            io.wAddr := I(15, 12)
            io.writeR := true.B
            io.writePC := true.B

            io.done := true.B
          }
        }
      }
    ),
    ( // Data-processing (register-shifted register)
      I => CM("000", I(27, 25)) &&
           (!CM("10xx0", I(24, 20))) &&
           CM("0xx1", I(7, 4)),
      (I, state) => {
        switch(state) {
          is(1.U) {
            io.rAddrA := I(19, 16)
            io.aluA := io.rDataA
            io.aluOp := I(24, 21)
            io.rAddrB := I(3, 0)
            io.aluB := io.rDataB
            io.aluShiftOp := I(6, 4)
            io.aluShiftNum := I(11, 8)

            io.wAddr := I(15, 12)
            io.writeR := true.B
            io.writePC := true.B

            io.done := true.B
          }
        }
      }
    ),
    ( // Data-processing (immediate)
      I => CM("001", I(27, 25)) &&
           (!CM("10xx0", I(24, 20))),
      (I, state) => {
        switch (state) {
          is (1.U) {
            io.rAddrA := I(19, 16)
            io.aluA := io.rDataA
            io.aluOp := I(24, 21)
            io.aluB := I(7, 0)
            io.aluShiftOp := "b111".U
            io.aluShiftNum := I(11, 8)

            io.wAddr := I(15, 12)
            io.writeR := true.B
            io.writePC := true.B

            io.done := true.B
          }
        }
      }
    )
  )

  when (io.state === 0.U) {
    when (io.cond) {
      io.writeIR := true.B
    } .otherwise {
      io.done := true.B
    }
  } .otherwise {
    for ((check, handle) <- instr_map) {
      when (check(io.IR)) {
        handle(io.IR, io.state)
      }
    }
  }
}