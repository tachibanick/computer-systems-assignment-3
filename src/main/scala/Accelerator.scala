import chisel3._
import chisel3.util._

class Accelerator extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt(16.W))
    val dataRead = Input(UInt(32.W))
    val writeEnable = Output(Bool())
    val dataWrite = Output(UInt(32.W))

  })

  // Row controlling
  val prevRow = RegInit(VecInit(Seq.fill(20)(0.U(3.W))))
  val row = RegInit(VecInit(Seq.fill(20)(0.U(3.W))))
  val nextRow = RegInit(VecInit(Seq.fill(20)(0.U(3.W))))

  //States
  val x = RegInit(0.U(5.W))
  val y = RegInit(0.U(5.W))

  val incrementY = () => {
    y := y + 1.U
    x := 0.U
  }

  val incrementX = () => {
    when(x < 19.U) {
      x := x + 1.U
    }.otherwise {
      incrementY()
    }
  }

  // Run in first cell of a new row
  val clearNextRow = () => {
    for (i <- 0 until 20) {
      nextRow(i) := 0.U
    }
  }

  val getReadAddress = (x: UInt, y: UInt) => 20.U * y + x
  val getWriteAddress = (x: UInt, y: UInt) => getReadAddress(x, y) + 400.U

  val getPixelCache = (pixelX: UInt, pixelY: UInt) => MuxCase(0.U(3.W), Array(
    (pixelY === y - 1.U) -> prevRow(pixelX),
    (pixelY === y) -> row(pixelX),
    (pixelY === y + 1.U) -> nextRow(pixelX)
  ))

  val getValue = (x: UInt, y: UInt) => getPixelCache(x, y)(0)
  val isRead = (x: UInt, y: UInt) => getPixelCache(x, y)(1)

  val readPixel = (pixelX: UInt, pixelY: UInt) => {
    io.address := getReadAddress(pixelX, pixelY)
    val value = io.dataRead === 255.U

    when(pixelY === y - 1.U) {
      prevRow(pixelX) := value + 2.U + (prevRow(pixelX) & 4.U)
    }.elsewhen(pixelY === y) {
      row(pixelX) := value + 2.U + (row(pixelX) & 4.U)
    }.elsewhen(pixelY === y + 1.U) {
      nextRow(pixelX) := value + 2.U + (nextRow(pixelX) & 4.U)
    }
  }

  val getErode = (x: UInt, y: UInt) => getPixelCache(x, y)(2)
  val setErode = (pixelX: UInt, pixelY: UInt) => {
    io.address := getReadAddress(pixelX, pixelY)

    when(pixelY === y - 1.U) {
      prevRow(pixelX) := prevRow(pixelX)(1, 0) + 4.U
    }.elsewhen(pixelY === y) {
      row(pixelX) := row(pixelX)(1, 0) + 4.U
    }.elsewhen(pixelY === y + 1.U) {
      nextRow(pixelX) := nextRow(pixelX)(1, 0) + 4.U
    }
  }

  val writePixel = (x: UInt, y: UInt, value: UInt) => {
    io.address := getWriteAddress(x, y)
    io.dataWrite := value
    io.writeEnable := true.B
  }

  val writeBlack = (x: UInt, y: UInt) => writePixel(x, y, 0.U)
  val writeWhite = (x: UInt, y: UInt) => writePixel(x, y, 255.U)

  //Default values
  io.writeEnable := false.B
  io.address := 0.U
  io.dataWrite := 0.U
  io.done := false.B

  val start = RegInit(false.B)

  when(io.start) {
    start := true.B
  }

  //FSMD
  when(start) {
    when(y === 20.U) {
      io.done := true.B
    }.otherwise {
      when(x === 0.U || x === 19.U || y === 0.U || y === 19.U) {
        when(x === 0.U) {
          // Shift rows
          prevRow := row
          row := nextRow
          clearNextRow()
        }

        writeBlack(x, y)
        incrementX()
      }.otherwise {
        when(getErode(x, y)) {
          writeBlack(x, y)
          incrementX()
        }.elsewhen(!isRead(x, y)) {
          readPixel(x, y)
        }.elsewhen(!getValue(x, y)) {
          // Pixel is black. Set erode right and down and write black
          setErode(x + 1.U, y)
          setErode(x, y + 1.U)
          writeBlack(x, y)
          incrementX()
        }.otherwise {
          // Pixel is white

          // Check left neighbour
          when(!isRead(x - 1.U, y)) {
            readPixel(x - 1.U, y)
          }.elsewhen(!getValue(x - 1.U, y)) {
            writeBlack(x - 1.U, y)
            incrementX()
          }.otherwise {
            // Left neighbour is white

            // Check up neighbour
            when(!isRead(x, y - 1.U)) {
              readPixel(x, y - 1.U)
            }.elsewhen(!getValue(x, y - 1.U)) {
              writeBlack(x, y - 1.U)
              incrementX()
            }.otherwise {
              // Up neighbour is white

              // Check right neighbour
              when(!isRead(x + 1.U, y)) {
                readPixel(x + 1.U, y)
              }.elsewhen(!getValue(x + 1.U, y)) {
                writeBlack(x + 1.U, y)
                incrementX()
              }.otherwise {
                // Right neighbour is white

                // Check down neighbour
                when(!isRead(x, y + 1.U)) {
                  readPixel(x, y + 1.U)
                }.elsewhen(!getValue(x, y + 1.U)) {
                  writeBlack(x, y + 1.U)
                  incrementX()
                }.otherwise {
                  // Down neighbour is white

                  // Write white pixel
                  writeWhite(x, y)
                  incrementX()
                }
              }
            }
          }
        }
      }
    }
  }
}
