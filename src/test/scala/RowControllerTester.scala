import chisel3._
import chisel3.iotesters
import chisel3.iotesters.PeekPokeTester

class RowControllerTester(dut: RowController) extends PeekPokeTester(dut) {
  poke(dut.io.nextRowIndex, 0)
  poke(dut.io.writeColumn, 0)
  poke(dut.io.writeEnable, 1)
  poke(dut.io.writeData, 1)
  step(1)

  poke(dut.io.writeColumn, 1)
  step(1)

  poke(dut.io.nextRowIndex, 1)
  poke(dut.io.writeColumn, 2)
  step(1)

  poke(dut.io.writeColumn, 3)
  step(1)

  poke(dut.io.nextRowIndex, 2)
  poke(dut.io.writeColumn, 4)
  step(1)

  poke(dut.io.writeColumn, 5)
  step(1)

  poke(dut.io.writeEnable, 0)
  step(5)

  expect(peek(dut.io.prevRow(0)) == 1, "Expected prevRow(0) to be 1, got " + peek(dut.io.prevRow(0)))
  expect(peek(dut.io.prevRow(1)) == 1, "Expected prevRow(1) to be 1, got " + peek(dut.io.prevRow(1)))
  expect(peek(dut.io.row(2)) == 1, "Expected row(2) to be 1, got " + peek(dut.io.row(2)))
  expect(peek(dut.io.row(3)) == 1, "Expected row(3) to be 1, got " + peek(dut.io.row(3)))
  expect(peek(dut.io.nextRow(4)) == 1, "Expected nextRow(4) to be 1, got " + peek(dut.io.nextRow(4)))
  expect(peek(dut.io.nextRow(5)) == 1, "Expected nextRow(5) to be 1, got " + peek(dut.io.nextRow(5)))

  step(5)
}

object RowControllerTester {
  def main(args: Array[String]): Unit = {
    println("Running the RowController tester")
    iotesters.Driver.execute(
      Array("--generate-vcd-output", "on",
        "--target-dir", "generated",
        "--top-name", "RowController"),
      () => new RowController()) {
      c => new RowControllerTester(c)
    }
  }
}
