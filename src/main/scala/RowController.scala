import chisel3._
import chisel3.util._

class RowController extends Module {
  val io = IO(new Bundle {
    val nextRowIndex = Input(UInt(2.W)) // 0, 1 or 2 = A, B or C

    val writeColumn = Input(UInt(5.W)) // 0 to 19
    val writeEnable = Input(Bool())
    val writeData = Input(UInt(1.W))

    val prevRow = Output(Vec(20, UInt(3.W)))
    val row = Output(Vec(20, UInt(3.W)))
    val nextRow = Output(Vec(20, UInt(3.W)))
  })

  val rowA = RegInit(VecInit(Seq.fill(20)(0.U(3.W)))) // Use 2.W when we add a cache bit
  val rowB = RegInit(VecInit(Seq.fill(20)(0.U(3.W))))
  val rowC = RegInit(VecInit(Seq.fill(20)(0.U(3.W))))

  when(io.nextRowIndex === 0.U) {
    io.prevRow := rowB
    io.row := rowC
    io.nextRow := rowA
  } .elsewhen(io.nextRowIndex === 1.U) {
    io.prevRow := rowC
    io.row := rowA
    io.nextRow := rowB
  } .otherwise {
    io.prevRow := rowA
    io.row := rowB
    io.nextRow := rowC
  }

  when(io.writeEnable && io.nextRowIndex === 0.U) {
    rowA(io.writeColumn) := io.writeData
  } .elsewhen(io.writeEnable && io.nextRowIndex === 1.U) {
    rowB(io.writeColumn) := io.writeData
  } .elsewhen(io.writeEnable) {
    rowC(io.writeColumn) := io.writeData
  }
}
