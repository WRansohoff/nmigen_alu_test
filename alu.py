from nmigen import *
from nmigen.back.pysim import *

# ALU definitions: [ bitcode, clock cycles, string ]
C_AND = [ 0b101000, 2, "&" ]
C_OR  = [ 0b101110, 2, "|" ]
C_XOR = [ 0b100110, 2, "^" ]
C_A   = [ 0b101010, 2, "=" ]

class ALU( Elaboratable ):
  def __init__( self ):
    # 'A' and 'B' data inputs.
    self.a = Signal( 32, reset = 0x00000000 )
    self.b = Signal( 32, reset = 0x00000000 )
    # 'F' function select input.
    self.f = Signal( 6,  reset = 0b000000 )
    # 'Y' data output.
    self.y = Signal( 32, reset = 0x00000000 )
    # 'Start' and 'Done' signalling bits.
    self.start = Signal()
    self.done  = Signal()

  def elaborate( self, platform ):
    # Core ALU module.
    m = Module()

    # 'Counter' which determines how many cycles have elapsed in
    # the current operation.
    cnt = Signal( 6 )
    # Latched input values.
    xa  = Signal( 32 )
    xb  = Signal( 32 )
    fn  = Signal( 6 )

    # Latch input / function values when count = 0 and start = 1.
    with m.If( ( cnt == 0 ) & ( self.start == 1 ) ):
      m.d.sync += [
        xa.eq( self.a ),
        xb.eq( self.b ),
        fn.eq( self.f ),
        cnt.eq( 1 ),
        self.done.eq( 0 )
      ]
    with m.Elif( cnt > 0 ):
      # TODO: Perform ALU computations.
      # TODO: Boolean unit (F = [...]):
      #  - 0b101000: Y = A AND B
      with m.If( fn == C_AND[ 0 ] ):
        with m.If( cnt == 1 ):
          m.d.sync += [
            self.y.eq( xa & xb ),
            cnt.eq( 0 ),
            self.start.eq( 0 ),
            self.done.eq( 1 )
          ]
      #  - 0b101110: Y = A  OR B
      #  - 0b100110: Y = A XOR B
      #  - 0b101010: Y = A
      # TODO: Arithmetic unit (F = [...]):
      #  - 0b01xxx0: Y = A + B
      #  - 0b01xxx1: Y = A - B
      # TODO: Comparison unit (F = [...]):
      #  - 0b00x011: Y = ( A == B )
      #  - 0b00x101: Y = ( A <  B )
      #  - 0b00x111: Y = ( A <= B )
      # TODO: Shift unit (F = [...]):
      #  - 0b11xx00: Y = A << B
      #  - 0b11xx01: Y = A >> B (no sign extend)
      #  - 0b11xx11: Y = A >> B (with sign extend)
      # Return 0 after one clock cycle for unrecognized commands.
      with m.Else():
        with m.If( cnt == 1 ):
          m.d.sync += [
            xa.eq( 0x00000000 ),
            xb.eq( 0x00000000 ),
            self.y.eq( 0x00000000 ),
            cnt.eq( 0 ),
            self.start.eq( 0 ),
            self.done.eq( 1 )
          ]

    # Dummy synchronous step for simulation
    m.d.sync += self.a.eq( self.b )

    # End of ALU module definition.
    return m

# Perform an individual ALU functional test.
def alu_ft( alu, a, b, fn, expected ):
  # Set A, B, F.
  yield alu.a.eq( a )
  yield alu.b.eq( b )
  yield alu.f.eq( fn[ 0 ] )
  # Set 'start'
  yield alu.start.eq( 1 )
  # Wait the appropriate number of ticks.
  for i in range( fn[ 1 ] ):
    yield Tick()
  # Done.
  yield Tick()
  # Check the result.
  yield Settle()
  act = yield alu.y
  if expected != act:
    print( "FAIL: 0x%08X %s 0x%08X = 0x%08X (got: 0x%08X)"
           %( a, fn[ 2 ], b, expected, act ) )
  else:
    print( "PASS: 0x%08X %s 0x%08X = 0x%08X"
           %( a, fn[ 2 ], b, expected ) )

# Top-level ALU test method.
def alu_test( alu ):
  # Let signals settle after reset.
  yield Settle()

  # Test the bitwise 'AND' operation.
  print( "AND (&) tests:" )
  yield from alu_ft( alu, 0xCCCCCCCC, 0xCCCC0000, C_AND, 0xCCCC0000 )
  yield from alu_ft( alu, 0x00000000, 0x00000000, C_AND, 0x00000000 )
  yield from alu_ft( alu, 0xFFFFFFFF, 0xFFFFFFFF, C_AND, 0xFFFFFFFF )
  yield from alu_ft( alu, 0x00000000, 0xFFFFFFFF, C_AND, 0x00000000 )
  yield from alu_ft( alu, 0xFFFFFFFF, 0x00000000, C_AND, 0x00000000 )

  # Test the bitwise 'OR' operation.
  print( "OR  (|) tests:" )
  yield from alu_ft( alu, 0xCCCCCCCC, 0xCCCC0000, C_OR, 0xCCCCCCCC )
  yield from alu_ft( alu, 0x00000000, 0x00000000, C_OR, 0x00000000 )
  yield from alu_ft( alu, 0xFFFFFFFF, 0xFFFFFFFF, C_OR, 0xFFFFFFFF )
  yield from alu_ft( alu, 0x00000000, 0xFFFFFFFF, C_OR, 0xFFFFFFFF )
  yield from alu_ft( alu, 0xFFFFFFFF, 0x00000000, C_OR, 0xFFFFFFFF )

  # Test the bitwise 'XOR' operation.
  print( "XOR (^) tests:" )
  yield from alu_ft( alu, 0xCCCCCCCC, 0xCCCC0000, C_XOR, 0x0000CCCC )
  yield from alu_ft( alu, 0x00000000, 0x00000000, C_XOR, 0x00000000 )
  yield from alu_ft( alu, 0xFFFFFFFF, 0xFFFFFFFF, C_XOR, 0x00000000 )
  yield from alu_ft( alu, 0x00000000, 0xFFFFFFFF, C_XOR, 0xFFFFFFFF )
  yield from alu_ft( alu, 0xFFFFFFFF, 0x00000000, C_XOR, 0xFFFFFFFF )

  # Test the 'Y = A' operation.
  print( "A   (=) tests:" )
  yield from alu_ft( alu, 0xCCCCCCCC, 0xCCCC0000, C_A, 0xCCCCCCCC )
  yield from alu_ft( alu, 0x00000000, 0x00000000, C_A, 0x00000000 )
  yield from alu_ft( alu, 0xFFFFFFFF, 0xFFFFFFFF, C_A, 0xFFFFFFFF )
  yield from alu_ft( alu, 0x00000000, 0xFFFFFFFF, C_A, 0x00000000 )
  yield from alu_ft( alu, 0xFFFFFFFF, 0x00000000, C_A, 0xFFFFFFFF )

  # Done.
  yield Tick()

# 'main' method to run a basic testbench.
if __name__ == "__main__":
  # Instantiate an ALU module.
  dut = ALU()

  # Run the tests.
  # (This is just a stub for now)
  with Simulator( dut, vcd_file = open( 'test.vcd', 'w' ) ) as sim:
    def proc():
      yield from alu_test( dut )
    sim.add_clock( 24e-6 )
    sim.add_sync_process( proc )
    sim.run()
