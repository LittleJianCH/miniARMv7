package Controller

import chisel3._
import chisel3.util._

class Controller(realARM: Boolean = false) extends Module {
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
    val writeN = Output(Bool())
    val writeZ = Output(Bool())
    val writeC = Output(Bool())
    val writeV = Output(Bool())

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
  io.writeN := false.B
  io.writeZ := false.B
  io.writeC := false.B
  io.writeV := false.B
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
           CM("xxx0", I(7, 4)) &&
           (!CM("1111", I(15, 12))),
      (I, state) => {
        switch (state) {
          is (1.U) {
            io.rAddrA := I(19, 16)
            io.aluA := io.rDataA
            io.rAddrB := I(3, 0)
            io.aluB := io.rDataB
            io.aluShiftOp := I(6, 4)
            io.aluShiftNum := I(11, 7)

            io.aluOp := MuxLookup(I(24, 21), I(24, 21), Seq(
              "b1000".U -> "b0000".U, // TST
              "b1001".U -> "b0001".U, // TEQ
              "b1010".U -> "b0010".U, // CMP
              "b1011".U -> "b0011".U, // CMN
            ))

            io.wAddr := I(15, 12)
            io.writeR := !VecInit(Seq(
              "b1000", "b1001", "b1010", "b1011"
            ).map(_.U)).contains(I(24, 21))
            io.writeN := I(20)
            io.writeZ := I(20)
            io.writeC := I(20)
            io.writeV := I(20) && VecInit(Seq(
              "b0010", "b0011", "b0100", "b0101",
              "b0110", "b0111", "b1010", "b1011"
            ).map(_.U)).contains(I(24, 21))

            io.done := true.B
          }
        }
      }
    ),
    ( // Data-processing (register-shifted register)
      I => CM("000", I(27, 25)) &&
           (!CM("10xx0", I(24, 20))) &&
           CM("0xx1", I(7, 4)) &&
           (!CM("1111", I(15, 12))),
      (I, state) => {
        switch(state) {
          is (1.U) {
            io.rAddrA := I(19, 16)
            io.aluA := io.rDataA
            io.rAddrB := I(3, 0)
            io.aluB := io.rDataB
            io.aluShiftOp := I(6, 4)
            io.aluShiftNum := I(11, 8)

            io.aluOp := MuxLookup(I(24, 21), I(24, 21), Seq(
              "b1000".U -> "b0000".U, // TST
              "b1001".U -> "b0001".U, // TEQ
              "b1010".U -> "b0010".U, // CMP
              "b1011".U -> "b0011".U, // CMN
            ))

            io.wAddr := I(15, 12)
            io.writeR := !VecInit(Seq(
              "b1000", "b1001", "b1010", "b1011"
            ).map(_.U)).contains(I(24, 21))
            io.writeR := true.B
            io.writeN := I(20)
            io.writeZ := I(20)
            io.writeC := I(20)
            io.writeV := I(20) && VecInit(Seq(
              "b0010", "b0011", "b0100", "b0101",
              "b0110", "b0111", "b1010", "b1011"
            ).map(_.U)).contains(I(24, 21))

            io.done := true.B
          }
        }
      }
    ),
    ( // Data-processing (immediate)
      I => CM("001", I(27, 25)) &&
           (!CM("10xx0", I(24, 20))) &&
           (!CM("1111", I(15, 12))),
      (I, state) => {
        switch (state) {
          is (1.U) {
            io.rAddrA := I(19, 16)
            io.aluA := io.rDataA
            io.aluB := I(7, 0)
            io.aluShiftOp := "b111".U
            io.aluShiftNum := I(11, 8)

            io.aluOp := MuxLookup(I(24, 21), I(24, 21), Seq(
              "b1000".U -> "b0000".U, // TST
              "b1001".U -> "b0001".U, // TEQ
              "b1010".U -> "b0010".U, // CMP
              "b1011".U -> "b0011".U, // CMN
            ))

            io.wAddr := I(15, 12)
            io.writeR := !VecInit(Seq(
              "b1000", "b1001", "b1010", "b1011"
            ).map(_.U)).contains(I(24, 21))
            io.writeN := I(20)
            io.writeZ := I(20)
            io.writeC := I(20)
            io.writeV := I(20) && VecInit(Seq(
              "b0010", "b0011", "b0100", "b0101",
              "b0110", "b0111", "b1010", "b1011"
            ).map(_.U)).contains(I(24, 21))

            io.done := true.B
          }
        }
      }
    ),
    ( // Branch and Exchange
      I => CM("00010010", I(27, 20)) &&
           CM("11111111111", I(19, 8)) &&
           CM("0001", I(7, 4)),
      (I, state) => {
        switch (state) {
          is (1.U) {
            io.rAddrA := I(3, 0)
            io.aluA := io.rDataA
            io.aluOp := "b1000".U

            io.wAddr := 15.U
            io.writeR := true.B

            io.done := true.B
          }
        }
      }
    ),
    ( // Branch
      I => CM("1010", I(27, 24)),
      (I, state) => {
        switch (state) {
          is (1.U) {
            io.rAddrA := 15.U
            io.aluA := io.rDataA
            io.aluB := Cat(Fill(6, I(23)), I(23, 0), Fill(2, 0.U))
            io.aluOp := "b0100".U

            io.wAddr := 15.U
            io.writeR := true.B

            io.done := true.B
          }
        }
      }
    ),
    ( // Branch with Link
      I => CM("1011", I(27, 24)),
      (I, state) => {
        switch (state) {
          is (1.U) {
            io.rAddrA := 15.U
            io.aluA := io.rDataA

            if (!realARM) {
              io.aluOp := "b1000".U
            } else {
              // 由于模拟了 PC + 2 的行为，在 BL 时存入的值应为 (PC + 2) - 1
              io.aluOp := "b0010".U
              io.aluB := 1.U
            }

            io.wAddr := 14.U
            io.writeR := true.B
          }
          is (2.U) {
            io.rAddrA := 15.U
            io.aluA := io.rDataA
            io.aluB := Cat(Fill(6, I(23)), I(23, 0), Fill(2, 0.U))
            io.aluOp := "b0100".U

            io.wAddr := 15.U
            io.writeR := true.B

            io.done := true.B
          }
        }
      }
    )
  )

  when (io.state === 0.U) {
    when (io.cond) {
      io.writeIR := true.B
      io.writePC := true.B
    } .otherwise {
      io.done := true.B
      io.writePC := true.B
    }
  } .otherwise {
    val matched = Wire(Bool())
    matched := false.B
    for ((check, handle) <- instr_map) {
      when (check(io.IR)) {
        handle(io.IR, io.state)
        matched := true.B
      }
    }

    when (!matched) {
      io.err := true.B
    }
  }
}