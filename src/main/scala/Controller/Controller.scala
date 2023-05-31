package Controller

import chisel3._
import chisel3.util._

class Cond extends Module {
  val io = IO(new Bundle {
    val IR = Input(UInt(32.W))
    val cond = Output(Bool())
  })


}

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
    val wData = Output(UInt(32.W))
    val writeR = Output(Bool())
    val writePC = Output(Bool())

    // For ALU
    val aluA = Output(UInt(32.W))
    val aluB = Output(UInt(32.W))
    val aluC = Output(UInt(32.W))
    val aluMulOp = Output(UInt(2.W))
    val aluOp = Output(UInt(4.W))
    val aluShiftOp = Output(UInt(3.W))
    val aluShiftNum = Output(UInt(8.W))
    val aluOut = Input(UInt(32.W))
    val aluCout = Input(Bool())

    // For RAM
    val ramREN = Output(Bool())
    val ramWEN = Output(Bool())
    val ramAddrR = Output(UInt(32.W))
    val ramAddrW = Output(UInt(32.W))
    val ramDataR = Input(UInt(32.W))
    val ramDataW = Output(UInt(32.W))

    // For ROM
    val romAddr = Output(UInt(32.W))
    val romData = Input(UInt(32.W))

    // For NZCV
    val writeN = Output(Bool())
    val writeZ = Output(Bool())
    val writeC = Output(Bool())
    val writeV = Output(Bool())

    // For regs
    val regsR = Input(Vec(4, UInt(32.W)))
    val regsW = Output(Vec(4, UInt(32.W)))
    val regsWE = Output(Vec(4, Bool()))

    val err = Output(Bool())
  })

  io.done := false.B
  io.writeIR := false.B
  io.rAddrA := 0.U
  io.rAddrB := 0.U
  io.rAddrC := 0.U
  io.wAddr := 0.U
  io.wData := 0.U
  io.writeR := false.B
  io.writePC := false.B
  io.aluA := 0.U
  io.aluB := 0.U
  io.aluC := 0.U
  io.aluMulOp := 0.U
  io.aluOp := 0.U
  io.aluShiftOp := 0.U
  io.aluShiftNum := 0.U
  io.writeN := false.B
  io.writeZ := false.B
  io.writeC := false.B
  io.writeV := false.B
  io.err := false.B
  io.regsW := VecInit(Seq.fill(4)(0.U))
  io.regsWE := VecInit(Seq.fill(4)(false.B))
  io.ramAddrR := 0.U
  io.ramAddrW := 0.U
  io.ramREN := false.B
  io.ramWEN := false.B
  io.ramDataW := 0.U
  io.romAddr := 0.U

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
            io.wData := io.aluOut
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
            io.wData := io.aluOut
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
            io.wData := io.aluOut
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
            io.wAddr := 15.U
            io.wData := io.rDataA
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
            io.wData := io.aluOut
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
            io.wData := io.aluOut
            io.writeR := true.B
          }
          is (2.U) {
            io.rAddrA := 15.U
            io.aluA := io.rDataA
            io.aluB := Cat(Fill(6, I(23)), I(23, 0), Fill(2, 0.U))
            io.aluOp := "b0100".U

            io.wAddr := 15.U
            io.wData := io.aluOut
            io.writeR := true.B

            io.done := true.B
          }
        }
      }
    ),
    ( // SWP
      I => CM("00010000", I(27, 20)) &&
           CM("0000", I(11, 8)) &&
           CM("1001", I(7, 4)),
      (I, state) => {
        switch (state) {
          is (1.U) {
            io.rAddrA := I(19, 16)

            when (!io.rDataA(31)) {
              // we can't write in ROM
              io.err := true.B
            }

            io.ramAddrR := io.rDataA
            io.ramREN := true.B
          }
          is (2.U) {
            io.rAddrA := I(19, 16)
            io.rAddrB := I(3, 0)

            io.ramAddrR := io.rDataA
            io.wAddr := I(15, 12)
            io.wData := io.ramDataR
            io.writeR := true.B

            io.ramAddrW := io.rDataA
            io.ramDataW := io.rDataB
            io.ramWEN := true.B

            io.done := true.B
          }
        }
      }
    ),
    ( // LDR
      I => CM("01xxx0x1", I(27, 20)),
      (I, state) => {
        io.rAddrA := I(19, 16)
        io.rAddrC := I(3, 0)
        io.aluA := io.rDataA
        io.aluB := Mux(I(25), io.rDataC, I(11, 0))
        io.aluOp := Mux(I(23), "b0100".U, "b0010".U)
        when(I(25)) {
          io.aluShiftOp := I(6, 4)
          io.aluShiftNum := I(11, 7)
        }

        when (state === 1.U) {
          val addr = Mux(I(24), io.aluOut, io.rDataA)
          when (addr(31)) {
            // read in RAM
            io.ramAddrR := addr
            io.ramREN := true.B
            io.regsW(0) := io.ramAddrR
            io.regsWE(0) := true.B
            io.regsW(1) := 1.U
            io.regsWE(1) := true.B
          } .otherwise {
            // read in ROM
            io.romAddr := addr
            io.regsW(1) := 2.U
            io.regsWE(1) := true.B

            io.wAddr := I(15, 12)
            io.wData := io.romData
            io.writeR := true.B
            io.done := !(I(21) || (!I(24)))
          }
        } .elsewhen ((io.regsR(1) === 1.U) && (state === 2.U)) {
          io.ramAddrR := io.regsR(0)
        } .elsewhen ((io.regsR(1) === 1.U) && (state === 3.U)) {
          io.ramAddrR := io.regsR(0)
          io.wAddr := I(15, 12)
          io.wData := io.ramDataR
          io.writeR := true.B
          io.done := !(I(21) || (!I(24)))
          io.regsW(1) := 2.U
          io.regsWE(1) := true.B
        } .elsewhen (io.regsR(1) === 2.U) {
          // I(21) || (!I(24))
          io.wAddr := I(19, 16)
          io.wData := io.aluOut
          io.writeR := true.B
          io.done := true.B
        }
      }
    ),
    ( // STR
      I => CM("01xxx0x0", I(27, 20)),
      (I, state) => {
        io.rAddrA := I(19, 16)
        io.rAddrB := I(15, 12)
        io.rAddrC := I(3, 0)
        io.aluA := io.rDataA
        io.aluB := Mux(I(25), io.rDataC, I(11, 0))
        io.aluOp := Mux(I(23), "b0100".U, "b0010".U)
        when(I(25)) {
          io.aluShiftOp := I(6, 4)
          io.aluShiftNum := I(11, 7)
        }

        switch (state) {
          is (1.U) {
            when(!io.aluOut(31)) {
              // we can't write in ROM
              io.err := true.B
            }

            io.ramAddrW := io.aluOut
            io.ramDataW := io.rDataB
            io.ramWEN := true.B

            io.done := !(I(21) || (!I(24)))
          }
          is (2.U) {
            // I(21) || (!I(24))
            io.wAddr := I(19, 16)
            io.wData := io.aluOut
            io.writeR := true.B
            io.done := true.B
          }
        }
      }
    ),
    ( // Halfword multiply and multiply accumulate
      I => CM("00010xx0", I(27, 20)) &&
           CM("1xx0", I(7, 4)),
      (I, state) => {
        when (I(22, 21) === 0.U || I(22, 21) === 3.U) {
          // Signed 16-bit multiply, 32-bit accumulate & Signed 16-bit multiply, 32-bit result
          switch (state) {
            is (1.U) {
              io.rAddrA := I(11, 8)
              io.rAddrB := I(15, 12)
              io.rAddrC := I(3, 0)

              io.aluA := Mux(I(6), io.rDataA(31, 16), io.rDataA(15, 0))
              io.aluB := Mux(I(22, 21) === 0.U, io.rDataB, 0.U)
              io.aluC := Mux(I(5), io.rDataC(31, 16), io.rDataC(15, 0))
              io.aluOp := "b0100".U
              io.aluMulOp := "b11".U

              io.wAddr := I(19, 16)
              io.wData := io.aluOut
              io.writeR := true.B

              io.done := true.B
            }
          }
        } .elsewhen (I(22, 21) === 1.U) {
          // Signed 16-bit × 32-bit multiply, 32-bit accumulate &&
          // Signed 16-bit × 32-bit multiply, 32-bit result
          switch (state) {
            is (1.U) {
              io.rAddrA := I(11, 8)
              io.rAddrC := I(3, 0)

              io.aluA := Mux(I(6), io.rDataA(31, 16), io.rDataA(15, 0))
              io.aluC := io.rDataC(15, 0)
              io.aluOp := "b1000".U
              io.aluMulOp := "b01".U

              io.regsW(0) := io.aluOut
              io.regsWE(0) := true.B
              io.regsW(1) := io.rDataC(31)
              io.regsWE(1) := true.B
            }
            is (2.U) {
              io.rAddrA := I(11, 8)
              io.rAddrC := I(3, 0)

              io.aluA := Mux(I(6), io.rDataA(31, 16), io.rDataA(15, 0))
              io.aluB := io.regsR(0)
              io.aluC := io.rDataC(31, 16)
              io.aluOp := Mux(io.regsR(1) === 1.U, "b0010".U, "b0100".U)
              io.aluMulOp := "b11".U
              io.aluShiftOp := "b010".U
              io.aluShiftNum := 16.U

              when (I(5)) {
                io.wAddr := I(19, 16)
                io.wData := io.aluOut
                io.writeR := true.B
                io.done := true.B
              } .otherwise {
                io.regsW(0) := io.aluOut
                io.regsWE(0) := true.B
              }
            }
            is (3.U) {
              io.rAddrA := I(15, 12)

              io.aluA := io.rDataA
              io.aluB := io.regsR(0)
              io.aluOp := "b0100".U

              io.wAddr := I(19, 16)
              io.wData := io.aluOut
              io.writeR := true.B

              io.done := true.B
            }
          }
        } .elsewhen (I(22, 21) === 2.U) {
          // Signed 16-bit multiply, 64-bit accumulate
          switch (state) {
            is (1.U) {
              io.rAddrA := I(11, 8)
              io.rAddrB := I(15, 12)
              io.rAddrC := I(3, 0)

              io.aluA := Mux(I(6), io.rDataA(31, 16), io.rDataA(15, 0))
              io.aluB := io.rDataB
              io.aluC := Mux(I(5), io.rDataC(31, 16), io.rDataC(15, 0))
              io.aluOp := "b0100".U
              io.aluMulOp := "b11".U

              io.regsW(0) := io.aluOut
              io.regsWE(0) := true.B
              io.regsW(1) := io.aluCout
              io.regsWE(1) := true.B
            }
            is (2.U) {
              io.rAddrA := I(19, 16)

              io.aluA := io.rDataA
              io.aluB := io.regsR(1)
              io.aluOp := "b0100".U

              io.wAddr := I(19, 16)
              io.wData := io.aluOut
              io.writeR := true.B
            }
            is (3.U) {
              io.aluA := io.regsR(0)
              io.aluOp := "b1000".U

              io.wAddr := I(15, 12)
              io.wData := io.aluOut
              io.writeR := true.B

              io.done := true.B
            }
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