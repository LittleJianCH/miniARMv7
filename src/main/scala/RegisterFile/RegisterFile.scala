package RegisterFile

import chisel3._
import chisel3.util._

class AddressTransfer extends Module {
  val io = IO(new Bundle {
    val mode = Input(UInt(5.W))
    val addr = Input(UInt(4.W))
    val out = Output(UInt(6.W))
  })

  io.out := MuxCase(io.addr, Seq(
    (VecInit((8 to 14).map(_.U)).contains(io.addr) && io.mode === "b10001".U) ->
      VecInit((Seq.fill(8)(0) ++ (17 to 22)).map(_.U))(io.addr),
    VecInit(Seq(13.U, 14.U)).contains(io.addr) ->
      MuxLookup(io.mode, io.addr, Seq(
          "b10011".U -> VecInit(Seq.fill(13)(0.U) ++ Seq(25.U, 26.U))(io.addr),
          "b10110".U -> VecInit(Seq.fill(13)(0.U) ++ Seq(27.U, 28.U))(io.addr),
          "b10111".U -> VecInit(Seq.fill(13)(0.U) ++ Seq(29.U, 30.U))(io.addr),
          "b11010".U -> VecInit(Seq.fill(13)(0.U) ++ Seq(31.U, 32.U))(io.addr),
          "b11011".U -> VecInit(Seq.fill(13)(0.U) ++ Seq(33.U, 34.U))(io.addr),
      ))
  ))
}

class RegisterFile extends Module {
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
    val rDataA = Output(UInt(32.W))
    val rDataB = Output(UInt(32.W))
    val rDataC = Output(UInt(32.W))
  })

  // we will write to the register file on the falling edge of the clock
  val negClock = (~clock.asUInt).asBool.asClock

  // R0 ~ R14, PC(R15), R8_fiq ~ R14_fiq, R13_irq ~ R14_irq, R13_svc ~ R14_svc,
  // R13_mon ~ R14_mon, R13_abt ~ R14_abt, R13_hyp ~ R14_hyp, R13_und ~ R14_und
  val regsCount = 15 + 1 + 7 + 2 + 2 + 2 + 2 + 2 + 2 // 35
  val regs = withClock(negClock)(Reg(Vec(regsCount, UInt(32.W))))

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

  io.rDataA := regs(addrTransferA.io.out)
  io.rDataB := regs(addrTransferB.io.out)
  io.rDataC := regs(addrTransferC.io.out)

  when (io.wReg === 1.U) {
    regs(addrTransferW.io.out) := io.wData
  }

  when (io.wPCReg === 1.U) {
    regs(15) := io.wPC
  }
}

object RegisterFileGen extends App {
  chisel3.emitVerilog(new RegisterFile, Array("--target-dir", "gen"))
}