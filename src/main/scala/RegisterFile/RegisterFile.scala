package RegisterFile

import chisel3._
import chisel3.util._

import ALU.Inc

class AddressTransfer extends Module {
  val io = IO(new Bundle {
    val mode = Input(UInt(5.W))
    val addr = Input(UInt(4.W))
    val out = Output(UInt(6.W))
  })

  val errR = 35 // if the register doesn't appear in the mode, return 35

  io.out := MuxLookup(io.mode, errR.U, Seq(
    "b10000".U -> io.addr,
    "b10001".U -> VecInit(((0 to 7) ++ (16 to 22) ++ Seq(15)).map(_.U))(io.addr),
    "b10010".U -> VecInit(((0 to 12) ++ Seq(23, 24, 15)).map(_.U))(io.addr),
    "b10011".U -> VecInit(((0 to 12) ++ Seq(25, 26, 15)).map(_.U))(io.addr),
    "b10110".U -> VecInit(((0 to 12) ++ Seq(27, 28, 15)).map(_.U))(io.addr),
    "b10111".U -> VecInit(((0 to 12) ++ Seq(29, 30, 15)).map(_.U))(io.addr),
    "b11010".U -> VecInit(((0 to 12) ++ Seq(31, errR, 15)).map(_.U))(io.addr),
    "b11011".U -> VecInit(((0 to 12) ++ Seq(32, 33, 15)).map(_.U))(io.addr),
    "b11111".U -> io.addr,
  ))
}

class RegisterFile(realARM: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val mode = Input(UInt(5.W))
    val rAddrA = Input(UInt(4.W))
    val rAddrB = Input(UInt(4.W))
    val rAddrC = Input(UInt(4.W))
    val wAddr = Input(UInt(4.W))
    val wData = Input(UInt(32.W))
    val wReg = Input(UInt(1.W))
    val wPC = Input(UInt(32.W))
    val wPCReg = Input(UInt(1.W))
    val error = Output(UInt(1.W))
    val rDataA = Output(UInt(32.W))
    val rDataB = Output(UInt(32.W))
    val rDataC = Output(UInt(32.W))
    val rPC = Output(UInt(32.W))

    val regs = Output(Vec(34, UInt(6.W)))
  })

  // we will write to the register file on the falling edge of the clock
  val negClock = (~clock.asUInt).asBool.asClock

  // R0 ~ R14, PC(R15), R8_fiq ~ R14_fiq, R13_irq ~ R14_irq, R13_svc ~ R14_svc,
  // R13_mon ~ R14_mon, R13_abt ~ R14_abt, R13_hyp, R13_und ~ R14_und
  val regsCount = 15 + 1 + 7 + 2 + 2 + 2 + 2 + 1 + 2 // 34
  val regs = withClock(negClock)(RegInit(VecInit(Seq.fill(regsCount)(0.U(32.W)))))
  io.regs := regs

  val addrTransferA = Module(new AddressTransfer)
  val addrTransferB = Module(new AddressTransfer)
  val addrTransferC = Module(new AddressTransfer)
  val addrTransferW = Module(new AddressTransfer)

  addrTransferA.io.mode := io.mode
  addrTransferA.io.addr := io.rAddrA

  addrTransferB.io.mode := io.mode
  addrTransferB.io.addr := io.rAddrB

  addrTransferC.io.mode := io.mode
  addrTransferC.io.addr := io.rAddrC

  addrTransferW.io.mode := io.mode
  addrTransferW.io.addr := io.wAddr

  io.error := (addrTransferA.io.out === 35.U) ||
              (addrTransferB.io.out === 35.U) ||
              (addrTransferC.io.out === 35.U) ||
              (addrTransferW.io.out === 35.U)

  val addrA = addrTransferA.io.out
  val addrB = addrTransferB.io.out
  val addrC = addrTransferC.io.out

  if (realARM) {
    val incA = Module(new Inc(30))
    val incB = Module(new Inc(30))
    val incC = Module(new Inc(30))

    incA.io.in := regs(addrA)(31, 2)
    incB.io.in := regs(addrB)(31, 2)
    incC.io.in := regs(addrC)(31, 2)

    io.rDataA := Mux((addrA === 15.U),
      Cat(incA.io.out, regs(addrA)(1, 0)),
      regs(addrA)
    )
    io.rDataB := Mux(realARM.B && (addrB === 15.U),
      Cat(incB.io.out, regs(addrB)(1, 0)),
      regs(addrB)
    )
    io.rDataC := Mux(realARM.B && (addrC === 15.U),
      Cat(incC.io.out, regs(addrC)(1, 0)),
      regs(addrC)
    )
  } else {
    io.rDataA := regs(addrA)
    io.rDataB := regs(addrB)
    io.rDataC := regs(addrC)
  }


  when (io.wReg === 1.U) {
    regs(addrTransferW.io.out) := io.wData
  }

  when (io.wPCReg === 1.U) {
    regs(15) := io.wPC
  }

  io.rPC := regs(15)
}

object RegisterFileGen extends App {
  chisel3.emitVerilog(new RegisterFile, Array("--target-dir", "gen"))
}
