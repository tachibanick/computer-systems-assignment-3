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
  //  val prevRow = Wire(Vec(20, UInt(3.W)))
  //  val row = Wire(Vec(20, UInt(3.W)))
  //  val nextRow = Wire(Vec(20, UInt(3.W)))
  //  val nextRowIndex = RegInit(2.U(2.W))

  //  // TODO: Maybe use wires instead of registers for these
  //  when(nextRowIndex === 0.U) {
  //    prevRow := rowB
  //    row := rowC
  //    nextRow := rowA
  //  }.elsewhen(nextRowIndex === 1.U) {
  //    prevRow := rowC
  //    row := rowA
  //    nextRow := rowB
  //  }.otherwise {
  //    prevRow := rowA
  //    row := rowB
  //    nextRow := rowC
  //  }


  //States
  val x = RegInit(0.U(5.W))
  val y = RegInit(0.U(5.W))

  // TODO: Increment after last row
  val incrementY = () => {
    y := y + 1.U
    x := 0.U
    //    when(nextRowIndex === 2.U) {
    //      nextRowIndex := 0.U
    //    }.otherwise {
    //      nextRowIndex := nextRowIndex + 1.U
    //    }
  }

  val incrementX = () => {
    when (x < 19.U) {
      x := x + 1.U
    }.otherwise {
      incrementY()
    }
  }

  // Run in first cell of a new row
  val clearNextRow = () => {
    nextRow(0) := 0.U
    nextRow(1) := 0.U
    nextRow(2) := 0.U
    nextRow(3) := 0.U
    nextRow(4) := 0.U
    nextRow(5) := 0.U
    nextRow(6) := 0.U
    nextRow(7) := 0.U
    nextRow(8) := 0.U
    nextRow(9) := 0.U
    nextRow(10) := 0.U
    nextRow(11) := 0.U
    nextRow(12) := 0.U
    nextRow(13) := 0.U
    nextRow(14) := 0.U
    nextRow(15) := 0.U
    nextRow(16) := 0.U
    nextRow(17) := 0.U
    nextRow(18) := 0.U
    nextRow(19) := 0.U
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
      prevRow(pixelX) := value + 2.U + prevRow(pixelX)(2)
    } .elsewhen(pixelY === y) {
      row(pixelX) := value + 2.U + row(pixelX)(2)
    } .elsewhen(pixelY === y + 1.U) {
      nextRow(pixelX) := value + 2.U + nextRow(pixelX)(2)
    }
  }

  val getErode = (x: UInt, y: UInt) => getPixelCache(x, y)(2)
  val setErode = (pixelX: UInt, pixelY: UInt) => {
    io.address := getReadAddress(pixelX, pixelY)

    when(pixelY === y - 1.U) {
      prevRow(pixelX) := prevRow(pixelX)(1,0) + 4.U
    } .elsewhen(pixelY === y) {
      row(pixelX) := row(pixelX)(1,0) + 4.U
    } .elsewhen(pixelY === y + 1.U) {
      nextRow(pixelX) := nextRow(pixelX)(1,0) + 4.U
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
  //  val writeEnable = RegInit(false.B)
  //  io.writeEnable := writeEnable
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
    // TODO: Split up somehow to increment and clear separately
    when(y === 20.U) {
      io.done := true.B
    }.otherwise {
      when(x === 0.U && RegNext(x) =/= 0.U) {
        prevRow := row
        row := nextRow
        clearNextRow()
      }.elsewhen(x === 0.U || x === 19.U || y === 0.U || y === 19.U) {
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

  //    io.address := getReadAddress(x, y)
  //
  //    val value = io.dataRead(0) // clock cycle
  //    val pixel = row(x)
  //    pixel := value + 2.U
  //
  //
  //    when(value) { // white
  //      when(row(x - 1.U)(1)) { // left neighbor is read
  //        when(row(x - 1.U)(0)) { // left neighbor white)
  //          when(prevRow(x)(1)) { //upstairs neighbor is read
  //            when(prevRow(x)(0)) { //upstairs neighbor is white
  //              when(row(x + 1.U)(1)) { // right neighbor is read
  //                when(row(x + 1.U)(0)) { //right neighbor white
  //                  when(nextRow(x)(1)) { //downstairs neighbor is read
  //                    when(nextRow(x)(0)) { // downstairs neighbor white
  //                      // stateReg := write_white
  //                    }.otherwise { //downstairs is black
  //                      // stateReg := write_black
  //                    }
  //                  }.otherwise { //downstairs unread
  //                    // stateReg := down
  //                  }
  //                }.otherwise { //right neighbor black
  //                  // stateReg := write_black
  //                }
  //              }.otherwise { //right neighbour unread
  //                // stateReg := right
  //              }
  //            }.otherwise { //upstairs black
  //              // stateReg := write_black
  //            }
  //          }.otherwise { //upstairs neighbor unread
  //            // stateReg := top
  //          }
  //        }.otherwise { // left neighbor black
  //          // stateReg := write_black
  //        }
  //      }.otherwise { //left neighbor unread
  //        // stateReg := left
  //      }
  //
  //    }.otherwise { // value is black
  //      row(x + 1.U) := 4.U + row(x + 1.U)
  //      nextRow(x) := 4.U + nextRow(x)
  //
  //      // stateReg := write_black
  //    }
  //}
  //    is (left) {
  //      writeEnable := false.B
  //      val value = io.dataRead(getAddress(x-1.U,y))(0)
  //      row(x-1.U) := value + 2.U
  //      when (row(x-1.U)(0)){ // left is white
  //        when(prevRow(x)(1)) { //upstairs neighbor is read
  //          when (prevRow(x)(0)) { //upstairs neighbor is white
  //            when (row(x+1.U)(1)) { // right neighbor is read
  //              when (row(x+1.U)(0)) { //right neighbor white
  //                when (nextRow(x)(1)) { //downstairs neighbor is read
  //                  when (nextRow(x)(0)) { // downstairs neighbor white
  //                    stateReg := write_white
  //                  } . otherwise { //downstairs is black
  //                    stateReg := write_black
  //                  }
  //                } . otherwise { //downstairs unread
  //                  stateReg := down
  //                }
  //              } .otherwise{ //right neighbor black
  //                stateReg := write_black
  //              }
  //            } .otherwise { //right neighbour unread
  //              stateReg := right
  //            }
  //          } . otherwise { //upstairs black
  //            stateReg := write_black
  //          }
  //        } . otherwise { //upstairs neighbor unread
  //          stateReg := top
  //        }
  //      } . otherwise {
  //        stateReg := write_black
  //      }
  //    }
  //    is (top) {
  //      writeEnable := false.B
  //      val value = io.dataRead(getAddress(x,y-1.U))(0)
  //      prevRow(x) := value + 2.U
  //      when (prevRow(x)(0)) { //upstiars white
  //
  //        when (row(x+1.U)(1)) { // right neighbor is read
  //          when (row(x+1.U)(0)) { //right neighbor white
  //            when (nextRow(x)(1)) { //downstairs neighbor is read
  //              when (nextRow(x)(0)) { // downstairs neighbor white
  //                stateReg := write_white
  //              } . otherwise { //downstairs is black
  //                stateReg := write_black
  //              }
  //            } . otherwise { //downstairs unread
  //              stateReg := down
  //            }
  //          } .otherwise{ //right neighbor black
  //            stateReg := write_black
  //          }
  //        } .otherwise { //right neighbour unread
  //          stateReg := right
  //        }
  //
  //      } .otherwise{ //upstairs blacker
  //        stateReg := write_black
  //      }
  //    }
  //
  //    is (right) {
  //      writeEnable := false.B
  //      val value = io.dataRead(getAddress(x+1.U,y))(0)
  //      row(x+1.U) := value + 2.U
  //      when (row(x+1.U)(0)){
  //
  //        when (nextRow(x)(1)) { //downstairs neighbor is read
  //          when (nextRow(x)(0)) { // downstairs neighbor white
  //            stateReg := write_white
  //          } . otherwise { //downstairs is black
  //            stateReg := write_black
  //          }
  //        } . otherwise { //downstairs unread
  //          stateReg := down
  //        }
  //      }.otherwise{ //right is black
  //        stateReg := write_black
  //      }
  //    }
  //    is (down) {
  //      writeEnable := false.B
  //      val value = io.dataRead(getAddress(x,y-1.U))(0)
  //      nextRow(x) := value + 2.U
  //      when (nextRow(x)(0)) {
  //        stateReg := write_white
  //      }.otherwise{
  //        stateReg := write_black
  //      }
  //    }
  //
  //    is (write_white){
  //      io.dataWrite := 255.U
  //      io.address := getAddress(x,y)
  //      writeEnable := true.B
  //      x := x+1.U
  //      when (x>19.U){
  //        y := y+1.U
  //        x := 0.U
  //        //rotate rows
  //        when (nextRowIndex === 2.U){
  //          nextRowIndex := 0.U
  //        } .otherwise{
  //          nextRowIndex := nextRowIndex + 1.U
  //        }
  //        when (y > 19.U){
  //          //TODO clear everything?
  //          io.done := true.B
  //        }
  //      }
  //      when (x === 0.U || x === 19.U || y === 0.U || y === 19.U){
  //        stateReg := write_black
  //      }.otherwise{
  //        when(row(x)(2)){ //if eroded
  //          stateReg := write_black
  //        }.elsewhen(row(x)(1)){
  //          when (row(x)(0)) { //is white
  //            when (row(x-1.U)(1)) { // left neighbor is read
  //              when (row(x-1.U)(0)) { // left neighbor white)
  //                when(prevRow(x)(1)) { //upstairs neighbor is read
  //                  when (prevRow(x)(0)) { //upstairs neighbor is white
  //                    when (row(x+1.U)(1)) { // right neighbor is read
  //                      when (row(x+1.U)(0)) { //right neighbor white
  //                        when (nextRow(x)(1)) { //downstairs neighbor is read
  //                          when (nextRow(x)(0)) { // downstairs neighbor white
  //                            stateReg := write_white
  //                          } . otherwise { //downstairs is black
  //                            stateReg := write_black
  //                          }
  //                        } . otherwise { //downstairs unread
  //                          stateReg := down
  //                        }
  //                      } .otherwise{ //right neighbor black
  //                        stateReg := write_black
  //                      }
  //                    } .otherwise { //right neighbour unread
  //                      stateReg := right
  //                    }
  //                  } . otherwise { //upstairs black
  //                    stateReg := write_black
  //                  }
  //                } . otherwise { //upstairs neighbor unread
  //                  stateReg := top
  //                }
  //              } . otherwise { // left neighbor black
  //                stateReg := write_black
  //              }
  //            } . otherwise { //left neighbor unread
  //              stateReg := left
  //            }
  //          }.otherwise{
  //            row(x+1.U) := row(x+1.U) + 4.U
  //            nextRow(x) := nextRow(x) + 4.U
  //            stateReg := write_black
  //          }
  //
  //        }.otherwise{
  //          stateReg := center
  //        }
  //      }
  //
  //    }
  //    is (write_black) {
  //      io.dataWrite := 0.U
  //      io.address := getAddress(x,y)
  //      writeEnable := true.B
  //      x := x+1.U
  //      when (x>19.U){
  //        y := y+1.U
  //        x := 0.U
  //        when (nextRowIndex === 2.U){
  //          nextRowIndex := 0.U
  //        } .otherwise{
  //          nextRowIndex := nextRowIndex + 1.U
  //        }
  //        when (y > 19.U){
  //          //TODO clear bottom row
  //          io.done := true.B
  //        }
  //      }
  //      when (x === 0.U || x === 19.U || y === 0.U || y === 19.U){
  //        stateReg := write_black
  //      }.otherwise{
  //        when(row(x)(2)){ //if eroded
  //          stateReg := write_black
  //        }.elsewhen(row(x)(1)){
  //          when (row(x)(0)) { //is white
  //            when (row(x-1.U)(1)) { // left neighbor is read
  //              when (row(x-1.U)(0)) { // left neighbor white)
  //                when(prevRow(x)(1)) { //upstairs neighbor is read
  //                  when (prevRow(x)(0)) { //upstairs neighbor is white
  //                    when (row(x+1.U)(1)) { // right neighbor is read
  //                      when (row(x+1.U)(0)) { //right neighbor white
  //                        when (nextRow(x)(1)) { //downstairs neighbor is read
  //                          when (nextRow(x)(0)) { // downstairs neighbor white
  //                            stateReg := write_white
  //                          } . otherwise { //downstairs is black
  //                            stateReg := write_black
  //                          }
  //                        } . otherwise { //downstairs unread
  //                          stateReg := down
  //                        }
  //                      } .otherwise{ //right neighbor black
  //                        stateReg := write_black
  //                      }
  //                    } .otherwise { //right neighbour unread
  //                      stateReg := right
  //                    }
  //                  } . otherwise { //upstairs black
  //                    stateReg := write_black
  //                  }
  //                } . otherwise { //upstairs neighbor unread
  //                  stateReg := top
  //                }
  //              } . otherwise { // left neighbor black
  //                stateReg := write_black
  //              }
  //            } . otherwise { //left neighbor unread
  //              stateReg := left
  //            }
  //          }.otherwise{
  //            row(x+1.U) := row(x+1.U) + 4.U
  //            nextRow(x) := nextRow(x) + 4.U
  //            stateReg := write_black
  //          }
  //
  //        }.otherwise{
  //          stateReg := center
  //        }
  //      }
  //
  //    }
  //  }

}
