import chisel3._
import chisel3.util._
import chisel3.stage.ChiselStage

class Seg7LEDBundle extends Bundle {
  val cathodes = UInt(7.W)
  val decimalPoint = UInt(1.W)
  val anodes = UInt(8.W)
}

class Seg7LED extends Module {
  val io = IO(new Bundle {
    val digits = Input(Vec(8, UInt(4.W)))
    val enable = Input(UInt(8.W))
    val seg7led = Output(new Seg7LEDBundle)
  })

  // Rotate every 1ms
  val (digitChangeCount, digitChange) = Counter(true.B, 100000)
  val (digitIndex, digitWrap) = Counter(digitChange, 8)
  val digitNum = io.digits(digitIndex)

  io.seg7led.cathodes := MuxCase("b1111_1111".U,
    Seq(
      (digitNum === "h0".U) -> "b100_0000".U,
      (digitNum === "h1".U) -> "b111_1001".U,
      (digitNum === "h2".U) -> "b010_0100".U,
      (digitNum === "h3".U) -> "b011_0000".U,
      (digitNum === "h4".U) -> "b001_1001".U,
      (digitNum === "h5".U) -> "b001_0010".U,
      (digitNum === "h6".U) -> "b000_0010".U,
      (digitNum === "h7".U) -> "b101_1000".U,
      (digitNum === "h8".U) -> "b000_0000".U,
      (digitNum === "h9".U) -> "b001_0000".U,
      (digitNum === "ha".U) -> "b000_1000".U,
      (digitNum === "hb".U) -> "b000_0011".U,
      (digitNum === "hc".U) -> "b100_0110".U,
      (digitNum === "hd".U) -> "b010_0001".U,
      (digitNum === "he".U) -> "b000_0110".U,
      (digitNum === "hf".U) -> "b000_1110".U))
  val anodes = RegInit("b1111_1110".U(8.W))
  when (digitChange) {
    anodes := Cat(anodes(6, 0), anodes(7))
  }
  io.seg7led.anodes := anodes | ~io.enable
  io.seg7led.decimalPoint := 1.U
}

class VgaBundle extends Bundle {
  val red = UInt(4.W)
  val green = UInt(4.W)
  val blue = UInt(4.W)
  val hSync = Bool()
  val vSync = Bool()
}

abstract class DisplayMode {
  val hMax: Int
  val hSyncPeriod: Int
  val hBackPorch: Int
  val hFrontPorch: Int

  val vMax: Int
  val vSyncPeriod: Int
  val vBackPorch: Int
  val vFrontPorch: Int

  def hDispMax = hMax - (hSyncPeriod + hBackPorch + hFrontPorch)
  def vDispMax = vMax - (vSyncPeriod + vBackPorch + vFrontPorch)
  def pxMax = hDispMax * vDispMax
}

object Svga extends DisplayMode {
  val hMax = 1056
  val hSyncPeriod = 128
  val hBackPorch = 88
  val hFrontPorch = 40

  val vMax = 628
  val vSyncPeriod = 4
  val vBackPorch = 23
  val vFrontPorch = 1
}

class DisplayClock extends BlackBox {
  val io = IO(new Bundle {
    val clk_system = Input(Clock())
    val clk_display = Output(Clock())
  })
}

class SnakeMapRam extends BlackBox {
  val io = IO(new Bundle {
    val clka = Input(Clock())
    val ena = Input(Bool())
    val wea = Input(Bool())
    val addra = Input(UInt(14.W))
    val dina = Input(UInt(1.W))
    val douta = Output(UInt(1.W))

    val clkb = Input(Clock())
    val enb = Input(Bool())
    val web = Input(Bool())
    val addrb = Input(UInt(14.W))
    val dinb = Input(UInt(1.W))
    val doutb = Output(UInt(1.W))
  })
}

class SnakeQueueRam extends BlackBox {
  val io = IO(new Bundle {
    val clka = Input(Clock())
    val ena = Input(Bool())
    val wea = Input(Bool())
    val addra = Input(UInt(14.W))
    val dina = Input(UInt(14.W))
    val douta = Output(UInt(14.W))
  })
}

class Debounce extends Module {
  val io = IO(new Bundle {
    val in = Input(Bool())
    val out = Output(Bool())
  })

  // Trigger every 10ms
  val (count, enable) = Counter(true.B, 1000000)
  val reg0 = RegEnable(io.in, false.B, enable)
  val reg1 = RegEnable(reg0, false.B, enable)
  io.out := reg0 && ~reg1 && enable
}

object Debounce {
  def apply(in: Bool): Bool = {
    val debounce = Module(new Debounce)
    debounce.io.in := in
    debounce.io.out
  }
}

object GameBoard {
  val pxWidth = 16
  val pxHeight = 16
  val boardWidth = Svga.hDispMax / pxWidth
  val boardHeight = Svga.vDispMax / pxHeight
}

class PositionBundle extends Bundle {
  val x = UInt(7.W)
  val y = UInt(7.W)
}

class SnakeMapDisplayBundle extends Bundle {
  val clk = Input(Clock())
  val en = Input(Bool())
  val we = Input(Bool())
  val addr = Input(UInt(14.W))
  val din = Input(UInt(1.W))
  val dout = Output(UInt(1.W))
}

class SnakeDisplay extends Module {
  val io = IO(new Bundle {
    val ready = Input(Bool())
    val food = Input(new PositionBundle)
    val mapA = Flipped(new SnakeMapDisplayBundle)
    val mapB = Flipped(new SnakeMapDisplayBundle)
    val nextMap = Input(UInt(1.W))
    val curMap = Output(UInt(1.W))
    val vga = Output(new VgaBundle)
  })

  val (hCount, hEn) = Counter(true.B, Svga.hMax)
  val (vCount, vEn) = Counter(hEn, Svga.vMax)
  val pxEnable = io.ready && hCount >= (Svga.hSyncPeriod + Svga.hBackPorch).U &&
    hCount < (Svga.hMax - Svga.hFrontPorch).U &&
    vCount >= (Svga.vSyncPeriod + Svga.vBackPorch).U &&
    vCount < (Svga.vMax - Svga.vFrontPorch).U

  val x = (hCount - (Svga.hSyncPeriod + Svga.hBackPorch).U) / GameBoard.pxWidth.U
  val y = (vCount - (Svga.vSyncPeriod + Svga.vBackPorch).U) / GameBoard.pxHeight.U
  val addr = Cat(y(6, 0), x(6, 0))
  val mapIndex = RegEnable(io.nextMap, 0.U, vEn)
  io.mapA.clk := clock
  io.mapA.en := pxEnable && mapIndex === 0.U
  io.mapA.we := false.B
  io.mapA.addr := addr
  io.mapA.din := 0.U
  io.mapB.clk := clock
  io.mapB.we := false.B
  io.mapB.en := pxEnable && mapIndex === 1.U
  io.mapB.addr := addr
  io.mapB.din := 0.U
  io.curMap := mapIndex

  // Update food position only after a map swap to ensure the state is consistent.
  val matched = RegNext(io.nextMap === mapIndex, false.B)
  val food = RegEnable(io.food, ~matched && io.nextMap === mapIndex)

  val pxData = MuxCase("h000".U(12.W), Array(
    (~RegNext(pxEnable, false.B)) -> "h000".U,
    (Mux(mapIndex === 0.U, io.mapA.dout, io.mapB.dout) === 1.U) -> "hfff".U,
    (Cat(food.y, food.x) === RegNext(addr)) -> "hf00".U,
    (RegNext(y, 0.U) % 8.U === 0.U || RegNext(x, 0.U) % 8.U === 0.U) -> "h020".U,
    (RegNext(y, 0.U) % 4.U === 0.U || RegNext(x, 0.U) % 4.U === 0.U) -> "h010".U))
  io.vga.red := pxData(11, 8)
  io.vga.green := pxData(7, 4)
  io.vga.blue := pxData(3, 0)
  io.vga.hSync := RegNext(!(hCount < Svga.hSyncPeriod.U), true.B)
  io.vga.vSync := RegNext(!(vCount < Svga.vSyncPeriod.U), true.B)
}

class RndGenerator extends Module {
  val io = IO(new Bundle {
    val init = Input(Bool())
    val seed = Input(UInt(128.W))
    val next = Input(Bool())
    val output = Output(UInt(64.W))
  })

  val state = RegInit("hf0f0f0f0f0f0f0f0f0f0f0f0f0f0f0f0".U)
  when (io.init) {
    state := io.seed
  }

  io.output := 0.U
  when (io.next) {
    val t = state(127, 64)
    val s = state(63, 0)
    val nextState = Cat(s,
      t ^
      Cat(t(40, 0), 0.U(23.W)) ^
      Cat(0.U(18.W), t(63, 18)) ^
      s ^
      Cat(0.U(5.W), s(63, 5)))
    state := nextState
    io.output := nextState(127, 64) + nextState(63, 0)
  }
}

class MapUpdater extends Module {
  val io = IO(new Bundle {
    val mapWrite = Output(Bool())
    val mapIn = Output(UInt(1.W))
    val mapOut = Input(UInt(1.W))
    val mapAddr = Output(UInt(14.W))
    val addPos = Input(new PositionBundle)
    val enableDelete = Input(Bool())
    val deletePos = Input(new PositionBundle)
    val valid = Input(Bool())
    val conflict = Output(Bool())
    val ready = Output(Bool())
  })

  val sIdle :: sDelete :: sCheckAdd :: sAdd :: Nil = Enum(4)
  val state = RegInit(sIdle)
  val conflict = RegInit(false.B)
  io.conflict := conflict
  io.ready := state === sIdle
  io.mapWrite := state === sDelete || state === sAdd
  io.mapIn := Mux(state === sAdd, 1.U, 0.U)
  io.mapAddr := MuxCase(0.U(14.W), Seq(
    (state === sAdd || state === sCheckAdd) -> Cat(io.addPos.y, io.addPos.x),
    (state === sDelete) -> Cat(io.deletePos.y, io.deletePos.x)))
  switch (state) {
    is (sIdle) {
      when (io.valid) {
        conflict := false.B
        when (io.enableDelete) {
          state := sDelete
        } .otherwise {
          state := sCheckAdd
        }
      }
    }
    is (sDelete) {
      state := sCheckAdd
    }
    is (sCheckAdd) {
      state := sAdd
    }
    is (sAdd) {
      when (io.mapOut === 1.U) {
        conflict := true.B
      }
      state := sIdle
    }
  }
}

object Direction {
  // Have each pair of opposite direction to be bitwise negate so that we can
  // use negate to check whether we are going back.
  // Ensure right is zero so that it's the default direction on reset.
  val right = "b00".U(2.W)
  val left  = "b11".U(2.W)
  val down  = "b01".U(2.W)
  val up    = "b10".U(2.W)
}

class GameLoop extends Module {
  val io = IO(new Bundle {
    val seed = Flipped(Decoupled(UInt(128.W)))
    val start = Input(Bool())
    val up = Input(Bool())
    val down = Input(Bool())
    val left = Input(Bool())
    val right = Input(Bool())
    val mapA = Flipped(new SnakeMapDisplayBundle)
    val mapB = Flipped(new SnakeMapDisplayBundle)
    val curMap = Input(UInt(1.W))
    val nextMap = Output(UInt(1.W))
    val food = Output(new PositionBundle)
    val valid = Output(Bool())
    val seg7led = Output(new Seg7LEDBundle)
    val led0 = Output(Bool())
    val led1 = Output(Bool())
  })

  val sReset :: sInitMap :: sInitSnake :: sInitFood :: sReady :: sIdle :: sAdvanceHead :: sUpdateFood :: sCheckFood :: sReadTail :: sAdvanceTail :: sUpdateMap0 :: sWaitMap0 :: sUpdateMap1 :: sWaitMap1 :: sGameOver :: Nil = Enum(16)
  val state = RegInit(sReset)
  io.valid := state >= sReady
  val started = RegInit(false.B)

  val foodGen = Module(new RndGenerator)
  val seed0 = RegNext(io.seed.valid, false.B)
  val seed1 = RegNext(seed0, false.B)
  foodGen.io.init := seed0 && ~seed1
  foodGen.io.seed := io.seed.bits
  io.seed.ready := true.B

  withReset (state === sReset) {
    val nextDirection = RegInit(Direction.right)
    val direction = RegEnable(nextDirection, Direction.right, state === sAdvanceHead)
    val prevDirection = RegEnable(direction, Direction.right, state === sAdvanceHead)
    val setDirection = (d: UInt) => {
      when (prevDirection === direction) {
        when (prevDirection =/= ~d) {
          direction := d
          nextDirection := d
        }
      } .elsewhen (direction === nextDirection) {
        when (direction =/= ~d) {
          nextDirection := d
        }
      }
    }
    when (io.up) {
      setDirection(Direction.up)
    }
    when (io.down) {
      setDirection(Direction.down)
    }
    when (io.left) {
      setDirection(Direction.left)
    }
    when (io.right) {
      setDirection(Direction.right)
    }

    val mapSize = 16384
    val (initMapAddr, _) = Counter(state === sInitMap, mapSize)
    val (tickCount, tick) = Counter(state >= sIdle && state < sGameOver, 10000000)

    val nextMap = RegInit(0.U(1.W))
    io.nextMap := nextMap

    val headX = RegInit((GameBoard.boardWidth / 2 - 2).U(7.W))
    val headY = RegInit((GameBoard.boardHeight / 2).U(7.W))
    val queueSize = 8192
    val (nextHeadAddr, headAddrWrap) = Counter(
      state === sInitSnake || state === sAdvanceHead, queueSize)
    val tailX = Reg(UInt(7.W))
    val tailY = Reg(UInt(7.W))
    val (tailAddr, tailAddrWrap) = Counter(state === sAdvanceTail, queueSize)

    val foodX = Reg(UInt(7.W))
    val foodY = Reg(UInt(7.W))
    io.food.x := foodX
    io.food.y := foodY
    val hitFood = headX === foodX && headY === foodY
    val updateFood = state === sUpdateFood || state === sInitFood
    foodGen.io.next := updateFood
    val nextFoodX = foodGen.io.output(63, 32) % GameBoard.boardWidth.U(7.W)
    val nextFoodY = foodGen.io.output(31, 0) % GameBoard.boardHeight.U(7.W)
    when (updateFood) {
      foodX := nextFoodX
      foodY := nextFoodY
    }

    val (score, _) = Counter(state === sAdvanceHead && hitFood, mapSize)
    val (distance, _) = Counter(state === sAdvanceHead, mapSize)
    val scoreLed = Module(new Seg7LED)
    scoreLed.io.seg7led <> io.seg7led
    scoreLed.io.enable := Cat(
      MuxCase("b1111".U, Array(
        (distance < 10.U) -> "b0001".U,
        (distance < 100.U) -> "b0011".U,
        (distance < 1000.U) -> "b0111".U)),
      MuxCase("b1111".U, Array(
        (score < 10.U) -> "b0001".U,
        (score < 100.U) -> "b0011".U,
        (score < 1000.U) -> "b0111".U)))
    scoreLed.io.digits := List(
      score % 10.U,
      (score / 10.U) % 10.U,
      (score / 100.U) % 10.U,
      (score / 1000.U) % 10.U,
      distance % 10.U,
      (distance / 10.U) % 10.U,
      (distance / 100.U) % 10.U,
      (distance / 1000.U) % 10.U)

    val mapUpdaterA = Module(new MapUpdater)
    val mapUpdaterB = Module(new MapUpdater)
    val connectMapUpdater = (map: SnakeMapDisplayBundle, updater: MapUpdater) => {
      map.clk := clock
      map.en := true.B
      map.we := Mux(state === sInitMap || state === sInitSnake, true.B, updater.io.mapWrite)
      map.din := MuxCase(updater.io.mapIn, Array(
        (state === sInitMap) -> 0.U,
        (state === sInitSnake) -> 1.U))
      map.addr := MuxCase(0.U(14.W), Seq(
        (state === sInitMap) -> initMapAddr,
        (state === sInitSnake) -> Cat(headY, headX),
        (state >= sUpdateMap0 && state <= sWaitMap1) -> updater.io.mapAddr,
        (state >= sUpdateFood && state <= sCheckFood) -> Cat(nextFoodY(6, 0), nextFoodX(6, 0))))
      updater.io.mapOut := map.dout
      updater.io.addPos.x := headX
      updater.io.addPos.y := headY
      updater.io.enableDelete := ~hitFood
      updater.io.deletePos.x := tailX
      updater.io.deletePos.y := tailY
    }
    val canUpdateMap = io.curMap === nextMap &&
      (state === sUpdateMap0 || state === sUpdateMap1)
    mapUpdaterA.io.valid := canUpdateMap && io.curMap === 1.U
    connectMapUpdater(io.mapA, mapUpdaterA)
    mapUpdaterB.io.valid := canUpdateMap && io.curMap === 0.U
    connectMapUpdater(io.mapB, mapUpdaterB)
    io.led0 := mapUpdaterA.io.conflict
    io.led1 := mapUpdaterB.io.conflict

    val queue = Module(new SnakeQueueRam)
    queue.io.clka := clock
    queue.io.ena := true.B
    queue.io.wea := state === sInitSnake || state === sAdvanceHead
    queue.io.dina := Cat(headY, headX)
    queue.io.addra := MuxCase(0.U(14.W), Seq(
      (state === sInitSnake || state === sAdvanceHead) -> nextHeadAddr,
      (state === sReadTail) -> tailAddr))

    switch (state) {
      is (sReset) {
        when (io.seed.valid) {
          state := sInitMap
        }
      }
      is (sInitMap) {
        when (initMapAddr === (mapSize - 1).U) {
          state := sInitSnake
        }
      }
      is (sInitSnake) {
        when (nextHeadAddr < 4.U) {
          headX := headX + 1.U
        } .otherwise {
          state := sInitFood
        }
      }
      is (sInitFood) {
        // Ensure that the food doesn't conflict with the snake
        when (nextFoodY =/= headY || nextFoodX <= headX - 5.U || nextFoodX > headX) {
          state := sReady
          // Force a swap map to let the display shows new food
          nextMap := ~nextMap
        }
      }
      is (sReady) {
        when (io.start || started) {
          state := sIdle
          started := true.B
        }
      }
      is (sIdle) {
        when (tick) {
          state := sAdvanceHead
          switch (direction) {
            is (Direction.up) {
              when (headY === 0.U) {
                headY := (GameBoard.boardHeight - 1).U
              } .otherwise {
                headY := headY - 1.U
              }
            }
            is (Direction.down) {
              when (headY === (GameBoard.boardHeight - 1).U) {
                headY := 0.U
              } .otherwise {
                headY := headY + 1.U
              }
            }
            is (Direction.left) {
              when (headX === 0.U) {
                headX := (GameBoard.boardWidth - 1).U
              } .otherwise {
                headX := headX - 1.U
              }
            }
            is (Direction.right) {
              when (headX === (GameBoard.boardWidth - 1).U) {
                headX := 0.U
              } .otherwise {
                headX := headX + 1.U
              }
            }
          }
        }
      }
      is (sAdvanceHead) {
        when (hitFood) {
          state := sUpdateFood
        } .otherwise {
          state := sReadTail
        }
      }
      is (sUpdateFood) {
        state := sCheckFood
      }
      is (sCheckFood) {
        when (io.mapA.dout === 1.U || hitFood) {
          state := sUpdateFood
        } .otherwise {
          state := sUpdateMap0
        }
      }
      is (sReadTail) {
        state := sAdvanceTail
      }
      is (sAdvanceTail) {
        tailX := queue.io.douta(6, 0)
        tailY := queue.io.douta(13, 7)
        state := sUpdateMap0
      }
      is (sUpdateMap0) {
        when (canUpdateMap) {
          state := sWaitMap0
        }
      }
      is (sWaitMap0) {
        when (mapUpdaterA.io.ready && mapUpdaterB.io.ready) {
          nextMap := ~nextMap
          state := sUpdateMap1
        }
      }
      is (sUpdateMap1) {
        when (canUpdateMap) {
          state := sWaitMap1
        }
      }
      is (sWaitMap1) {
        when (mapUpdaterA.io.ready && mapUpdaterB.io.ready) {
          when (mapUpdaterA.io.conflict) {
            state := sGameOver
          } .otherwise {
            state := sIdle
          }
        }
      }
      is (sGameOver) {
        when (io.start) {
          state := sReset
        }
      }
    }
  }
}

class MicBundle extends Bundle {
  val clk = Output(Bool())
  val data = Input(Bool())
  val sel = Output(Bool())
}

class MicSeeder(n: Int) extends Module {
  val io = IO(new Bundle {
    val mic = new MicBundle
    val out = Decoupled(UInt(n.W))
  })

  val done = RegInit(false.B)
  val (clockCount, tick) = Counter(~done, 50)
  io.mic.clk := clockCount >= 25.U
  io.mic.sel := false.B

  val (_, allBits) = Counter(tick, n)
  when (allBits) {
    done := true.B
  }
  io.out.valid := done

  val result = RegInit(0.U(n.W))
  when (tick && ~done) {
    result := Cat(io.mic.data, result(n - 1, 1))
  }
  io.out.bits := result
}

class Snake extends Module {
  val io = IO(new Bundle {
    val resetN = Input(Bool())
    val start = Input(Bool())
    val up = Input(Bool())
    val down = Input(Bool())
    val left = Input(Bool())
    val right = Input(Bool())
    val mic = new MicBundle
    val seg7led = Output(new Seg7LEDBundle)
    val vga = Output(new VgaBundle)
    val led0 = Output(Bool())
    val led1 = Output(Bool())
  })

  val myReset = ~io.resetN

  withReset (myReset) {
    val micSeeder = Module(new MicSeeder(128))
    micSeeder.io.mic <> io.mic

    val mapA = Module(new SnakeMapRam)
    val mapB = Module(new SnakeMapRam)

    val gameLoop = Module(new GameLoop)
    gameLoop.io.seed <> micSeeder.io.out
    gameLoop.io.start := Debounce(io.start)
    gameLoop.io.up := Debounce(io.up)
    gameLoop.io.down := Debounce(io.down)
    gameLoop.io.left := Debounce(io.left)
    gameLoop.io.right := Debounce(io.right)
    io.led0 := gameLoop.io.led0
    io.led1 := gameLoop.io.led1

    val displayClock = Module(new DisplayClock)
    displayClock.io.clk_system := clock

    withClock (displayClock.io.clk_display) {
      val display = Module(new SnakeDisplay)
      display.io.ready := gameLoop.io.valid
      display.io.food <> gameLoop.io.food
      display.io.nextMap := gameLoop.io.nextMap
      gameLoop.io.curMap := display.io.curMap
      display.io.nextMap := gameLoop.io.nextMap
      io.vga := display.io.vga
      io.seg7led <> gameLoop.io.seg7led

      def connectPortA(ram: SnakeMapRam, bundle: SnakeMapDisplayBundle) = {
        ram.io.clka := bundle.clk
        ram.io.ena := bundle.en
        ram.io.wea := bundle.we
        ram.io.addra := bundle.addr
        ram.io.dina := bundle.din
        bundle.dout := ram.io.douta
      }
      def connectPortB(ram: SnakeMapRam, bundle: SnakeMapDisplayBundle) = {
        ram.io.clkb := bundle.clk
        ram.io.enb := bundle.en
        ram.io.web := bundle.we
        ram.io.addrb := bundle.addr
        ram.io.dinb := bundle.din
        bundle.dout := ram.io.doutb
      }
      connectPortA(mapA, display.io.mapA)
      connectPortA(mapB, display.io.mapB)
      connectPortB(mapA, gameLoop.io.mapA)
      connectPortB(mapB, gameLoop.io.mapB)
    }
  }
}

object Snake extends App {
  (new ChiselStage).emitVerilog(new Snake, args)
}
