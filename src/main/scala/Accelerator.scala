import chisel3._
import chisel3.util._

class Accelerator extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt (16.W))
    val dataRead = Input(UInt (32.W))
    val writeEnable = Output(Bool ())
    val dataWrite = Output(UInt (32.W))

  })

  // Row controlling
  val rowA = RegInit(VecInit(Seq.fill(20)(0.U(3.W))))
  val rowB = RegInit(VecInit(Seq.fill(20)(0.U(3.W))))
  val rowC = RegInit(VecInit(Seq.fill(20)(0.U(3.W))))
  val prevRow = RegInit(VecInit(Seq.fill(20)(0.U(3.W)))) 
  val row = RegInit(VecInit(Seq.fill(20)(0.U(3.W))))
  val nextRow = RegInit(VecInit(Seq.fill(20)(0.U(3.W))))
  val nextRowIndex = RegInit(2.U(2.W))

  when(nextRowIndex === 0.U) {
    prevRow := rowB
    row := rowC
    nextRow := rowA
  } .elsewhen(nextRowIndex === 1.U) {
    prevRow := rowC
    row := rowA
    nextRow := rowB
  } .otherwise {
    prevRow := rowA
    row := rowB
    nextRow := rowC
  }

  //States
  val init :: write_white :: write_black :: center :: left :: top :: right ::  down :: Nil = Enum (8)
  val stateReg = RegInit(center)
  val x = RegInit(0.U(5.W))
  val y = RegInit(0.U(5.W))

  val getAddress = (x: UInt, y: UInt) => Mux(writeEnable, 400.U+y*20.U+x, 20.U*y+x)

  

  //Default values
  val writeEnable = RegInit(false.B)
  io.writeEnable := writeEnable
  io.address := 0.U
  io.dataWrite := 0.U(32.W)
  io.done := false.B


  //FSMD
  switch(stateReg) {
    is(center) {
      writeEnable := false.B
      io.address := getAddress(x,y)

      val value = io.dataRead(0) // clock cycle
      val pixel = row(x)
      pixel := value + 2.U

      when (value) { // white
        when (row(x-1.U)(1)) { // left neighbor is read
          when (row(x-1.U)(0)) { // left neighbor white)
            when(prevRow(x)(1)) { //upstairs neighbor is read
              when (prevRow(x)(0)) { //upstairs neighbor is white
                when (row(x+1.U)(1)) { // right neighbor is read
                  when (row(x+1.U)(0)) { //right neighbor white
                    when (nextRow(x)(1)) { //downstairs neighbor is read
                      when (nextRow(x)(0)) { // downstairs neighbor white
                        stateReg := write_white
                      } . otherwise { //downstairs is black
                        stateReg := write_black
                      }
                    } . otherwise { //downstairs unread
                      stateReg := down
                    }
                  } .otherwise{ //right neighbor black
                    stateReg := write_black
                  }
                } .otherwise { //right neighbour unread
                  stateReg := right
                }
              } . otherwise { //upstairs black
                stateReg := write_black
              }
            } . otherwise { //upstairs neighbor unread
              stateReg := top
            }
          } . otherwise { // left neighbor black
            stateReg := write_black
          }
        } . otherwise { //left neighbor unread
          stateReg := left
        }

      } .otherwise{ // value is black
        row(x+1.U) := 4.U + row(x+1.U)
        nextRow(x) := 4.U + nextRow(x)

        stateReg := write_black
      }
    }
    is (left) {
      writeEnable := false.B
      val value = io.dataRead(getAddress(x-1.U,y))(0)
      value := value + 2.U
      row(x-1.U) := value
      when (row(x-1.U)(0)){ // left is white

        when(prevRow(x)(1)) { //upstairs neighbor is read
          when (prevRow(x)(0)) { //upstairs neighbor is white
            when (row(x+1.U)(1)) { // right neighbor is read
              when (row(x+1.U)(0)) { //right neighbor white
                when (nextRow(x)(1)) { //downstairs neighbor is read
                  when (nextRow(x)(0)) { // downstairs neighbor white
                    stateReg := write_white
                  } . otherwise { //downstairs is black
                    stateReg := write_black
                  }
                } . otherwise { //downstairs unread
                  stateReg := down
                }
              } .otherwise{ //right neighbor black
                stateReg := write_black
              }
            } .otherwise { //right neighbour unread
              stateReg := right
            }
          } . otherwise { //upstairs black
            stateReg := write_black
          }
        } . otherwise { //upstairs neighbor unread
          stateReg := top
        }
      } . otherwise {
        stateReg := write_black
      }
    }
    is (top) {
      writeEnable := false.B
      val value = io.dataRead(getAddress(x,y-1.U))(0)
      value := value + 2.U
      prevRow(x) := value
      when (prevRow(x)(0)) { //upstiars white

        when (row(x+1.U)(1)) { // right neighbor is read
          when (row(x+1.U)(0)) { //right neighbor white
            when (nextRow(x)(1)) { //downstairs neighbor is read
              when (nextRow(x)(0)) { // downstairs neighbor white
                stateReg := write_white
              } . otherwise { //downstairs is black
                stateReg := write_black
              }
            } . otherwise { //downstairs unread
              stateReg := down
            }
          } .otherwise{ //right neighbor black
            stateReg := write_black
          }
        } .otherwise { //right neighbour unread
          stateReg := right
        }

      } .otherwise{ //upstairs blacker
        stateReg := write_black
      }
    }

    is (right) {
      writeEnable := false.B
      val value = io.dataRead(getAddress(x+1.U,y))(0)
      value := value + 2.U
      row(x+1.U) := value
      when (row(x+1.U)(0)){

        when (nextRow(x)(1)) { //downstairs neighbor is read
          when (nextRow(x)(0)) { // downstairs neighbor white
            stateReg := write_white
          } . otherwise { //downstairs is black
            stateReg := write_black
          }
        } . otherwise { //downstairs unread
          stateReg := down
        }
      }.otherwise{ //right is black
        stateReg := write_black
      }
    }
    is (down) {
      writeEnable := false.B
      val value = io.dataRead(getAddress(x,y-1.U))(0)
      value := value + 2.U
      nextRow(x) := value
      when (nextRow(x)(0)) {
        stateReg := write_white
      }.otherwise{
        stateReg := write_black
      }
    }

    is (write_white){
      io.dataWrite := 255.U
      io.address := getAddress(x,y)
      writeEnable := true.B
      x := x+1.U
      when (x>19.U){
        y := y+1.U
        x := 0.U
        //rotate rows
        when (nextRowIndex === 2.U){
          nextRowIndex := 0.U
        } .otherwise{
          nextRowIndex := nextRowIndex + 1.U
        }
        when (y > 19.U){
          //TODO clear everything?
          io.done := true.B
        }
      }
      when (x === 0.U || x === 19.U || y === 0.U || y === 19.U){
        stateReg := write_black
      }.otherwise{
        when(row(x)(2)){ //if eroded
          stateReg := write_black
        }.elsewhen(row(x)(1)){
          when (row(x)(0)) { //is white
            when (row(x-1.U)(1)) { // left neighbor is read
              when (row(x-1.U)(0)) { // left neighbor white)
                when(prevRow(x)(1)) { //upstairs neighbor is read
                  when (prevRow(x)(0)) { //upstairs neighbor is white
                    when (row(x+1.U)(1)) { // right neighbor is read
                      when (row(x+1.U)(0)) { //right neighbor white
                        when (nextRow(x)(1)) { //downstairs neighbor is read
                          when (nextRow(x)(0)) { // downstairs neighbor white
                            stateReg := write_white
                          } . otherwise { //downstairs is black
                            stateReg := write_black
                          }
                        } . otherwise { //downstairs unread
                          stateReg := down
                        }
                      } .otherwise{ //right neighbor black
                        stateReg := write_black
                      }
                    } .otherwise { //right neighbour unread
                      stateReg := right
                    }
                  } . otherwise { //upstairs black
                    stateReg := write_black
                  }
                } . otherwise { //upstairs neighbor unread
                  stateReg := top
                }
              } . otherwise { // left neighbor black
                stateReg := write_black
              }
            } . otherwise { //left neighbor unread
              stateReg := left
            }
          }.otherwise{
            row(x+1.U) := row(x+1.U) + 4.U
            nextRow(x) := nextRow(x) + 4.U
            stateReg := write_black
          }

        }.otherwise{
          stateReg := center
        }
      }

    }
    is (write_black) {
      io.dataWrite := 0.U
      io.address := getAddress(x,y)
      writeEnable := true.B
      x := x+1.U
      when (x>19.U){
        y := y+1.U
        x := 0.U
        when (nextRowIndex === 2.U){
          nextRowIndex := 0.U
        } .otherwise{
          nextRowIndex := nextRowIndex + 1.U
        }
        when (y > 19.U){
          //TODO clear bottom row
          io.done := true.B
        }
      }
      when (x === 0.U || x === 19.U || y === 0.U || y === 19.U){
        stateReg := write_black
      }.otherwise{
        when(row(x)(2)){ //if eroded
          stateReg := write_black
        }.elsewhen(row(x)(1)){
          when (row(x)(0)) { //is white
            when (row(x-1.U)(1)) { // left neighbor is read
              when (row(x-1.U)(0)) { // left neighbor white)
                when(prevRow(x)(1)) { //upstairs neighbor is read
                  when (prevRow(x)(0)) { //upstairs neighbor is white
                    when (row(x+1.U)(1)) { // right neighbor is read
                      when (row(x+1.U)(0)) { //right neighbor white
                        when (nextRow(x)(1)) { //downstairs neighbor is read
                          when (nextRow(x)(0)) { // downstairs neighbor white
                            stateReg := write_white
                          } . otherwise { //downstairs is black
                            stateReg := write_black
                          }
                        } . otherwise { //downstairs unread
                          stateReg := down
                        }
                      } .otherwise{ //right neighbor black
                        stateReg := write_black
                      }
                    } .otherwise { //right neighbour unread
                      stateReg := right
                    }
                  } . otherwise { //upstairs black
                    stateReg := write_black
                  }
                } . otherwise { //upstairs neighbor unread
                  stateReg := top
                }
              } . otherwise { // left neighbor black
                stateReg := write_black
              }
            } . otherwise { //left neighbor unread
              stateReg := left
            }
          }.otherwise{
            row(x+1.U) := row(x+1.U) + 4.U
            nextRow(x) := nextRow(x) + 4.U
            stateReg := write_black
          }

        }.otherwise{
          stateReg := center
        }
      }

    }
  }

}
