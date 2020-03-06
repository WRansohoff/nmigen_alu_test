from nmigen import *
from nmigen.back.pysim import *

# ALU definitions: [ bitcode, clock cycles, string ]
C_AND = [ 0b101000, 2, "&" ]
C_OR  = [ 0b101110, 2, "|" ]
C_XOR = [ 0b100110, 2, "^" ]
C_A   = [ 0b101010, 2, "=" ]
C_ADD = [ 0b010000, 2, "+" ]
C_SUB = [ 0b010001, 2, "-" ]
C_CEQ = [ 0b000011, 2, "==" ]
C_CLT = [ 0b000101, 2, "<" ]
C_CLE = [ 0b000111, 2, "<=" ]
C_SHL = [ 0b110000, 2, "<<" ]
C_SHR = [ 0b110001, 2, ">>" ]
C_SRA = [ 0b110011, 2, ">>" ]

class ALU( Elaboratable ):
  def __init__( self ):
    # 'A' and 'B' data inputs.
    self.a = Signal( 32, reset = 0x00000000 )
    self.b = Signal( 32, reset = 0x00000000 )
    # 'F' function select input.
    self.f = Signal( 6,  reset = 0b000000 )
    # 'Y' data output.
    self.y = Signal( 32, reset = 0x00000000 )
    # 'N' arithmetic flag (last result was negative)
    self.n = Signal()
    # 'Z' arithmetic flag (last result == zero)
    self.z = Signal()
    # 'V' arithmetic flag (last result overflowed)
    self.v = Signal()
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

    # Combinational N, Z, and V arithmetic flags.
    # 'N' flag is always equal to the most significant result bit.
    m.d.comb += self.n.eq( self.y.bit_select( 31, 1 ) )
    # 'Z' flag is true if the result is zero.
    m.d.comb += self.z.eq( self.y == 0 )
    # 'V' flag is true if an overflow occurred.
    with m.If( fn.matches( '01---0' ) ):
      # With addition, overflow occurred if the two input signs are
      # the same and those signs differ from the result's sign.
      m.d.comb += self.v.eq( ( xa[ 31 ] == xb[ 31 ] ) &
                             ( xa[ 31 ] != self.y[ 31 ] ) )
    with m.Elif( fn.matches( '01---1' ) ):
      # With subtraction, overflow occurred if A and -B have the same
      # sign, and that sign differs from the result's sign.
      m.d.comb += self.v.eq( ( xa[ 31 ] != xb[ 31 ] ) &
                             ( xa[ 31 ] != self.y[ 31 ] ) )
    with m.Else():
      # For non-arithmetic operations, set 'V' to 0.
      m.d.comb += self.v.eq( 0 )

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
      # Perform ALU computations based on the 'function' bits.
      # Boolean unit (F = [...]):
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
      with m.Elif( fn == C_OR[ 0 ] ):
        with m.If( cnt == 1 ):
          m.d.sync += [
            self.y.eq( xa | xb ),
            cnt.eq( 0 ),
            self.start.eq( 0 ),
            self.done.eq( 1 )
          ]
      #  - 0b100110: Y = A XOR B
      with m.Elif( fn == C_XOR[ 0 ] ):
        with m.If( cnt == 1 ):
          m.d.sync += [
            self.y.eq( xa ^ xb ),
            cnt.eq( 0 ),
            self.start.eq( 0 ),
            self.done.eq( 1 )
          ]
      #  - 0b101010: Y = A
      with m.Elif( fn == C_A[ 0 ] ):
        with m.If( cnt == 1 ):
          m.d.sync += [
            self.y.eq( xa ),
            cnt.eq( 0 ),
            self.start.eq( 0 ),
            self.done.eq( 1 )
          ]
      # Arithmetic unit (F = [...]):
      #  - 0b01xxx0: Y = A + B
      with m.Elif( fn.matches( '01---0' ) ):
        with m.If( cnt == 1 ):
          m.d.sync += [
            self.y.eq( xa + xb ),
            cnt.eq( 0 ),
            self.start.eq( 0 ),
            self.done.eq( 1 )
          ]
      #  - 0b01xxx1: Y = A - B
      with m.Elif( fn.matches( '01---1' ) ):
        with m.If( cnt == 1 ):
          m.d.sync += [
            self.y.eq( xa - xb ),
            cnt.eq( 0 ),
            self.start.eq( 0 ),
            self.done.eq( 1 )
          ]
      # Comparison unit (F = [...]):
      #  - 0b00x011: Y = ( A == B )
      with m.Elif( fn.matches( '00-011' ) ):
        with m.If( cnt == 1 ):
          m.d.sync += [
            self.y.eq( xa == xb ),
            cnt.eq( 0 ),
            self.start.eq( 0 ),
            self.done.eq( 1 )
          ]
      #  - 0b00x101: Y = ( A <  B )
      with m.Elif( fn.matches( '00-101' ) ):
        with m.If( cnt == 1 ):
          m.d.sync += [
            # Can't use 'xa < xb' because HW logic doesn't account
            # for negative numbers, i.e. 0xFFFFFFFF > 0x00000000.
            self.y.eq( ( ( xb - xa ) > 0 ) &
                       ( ( xb - xa )[ 31 ] == 0 ) ),
            cnt.eq( 0 ),
            self.start.eq( 0 ),
            self.done.eq( 1 )
          ]
      #  - 0b00x111: Y = ( A <= B )
      with m.Elif( fn.matches( '00-111' ) ):
        with m.If( cnt == 1 ):
          m.d.sync += [
            # Same as above; 'xa <= xb' hardware description
            # does not account for negative numbers.
            self.y.eq( ( ( xb - xa ) >= 0 ) &
                       ( ( xb - xa )[ 31 ] == 0 ) ),
            cnt.eq( 0 ),
            self.start.eq( 0 ),
            self.done.eq( 1 )
          ]
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
  # Done. Check the result after combinational logic settles.
  yield Settle()
  act = yield alu.y
  if expected != act:
    print( "FAIL: 0x%08X %s 0x%08X = 0x%08X (got: 0x%08X)"
           %( a, fn[ 2 ], b, expected, act ) )
  else:
    print( "PASS: 0x%08X %s 0x%08X = 0x%08X"
           %( a, fn[ 2 ], b, expected ) )

# Helper method to verify that 'N', 'Z', 'V' flags are set correctly.
def check_nzv( alu, n, z, v ):
  an = yield alu.n
  az = yield alu.z
  av = yield alu.v
  if ( an == n ) and ( az == z ) and ( av == v ):
    print( "  PASS: N, Z, V flags: %d, %d, %d"
           %( n, z, v ) )
  else:
    print( "  FAIL: N, Z, V flags: %d, %d, %d (got: %d, %d, %d)"
           %( n, z, v, an, az, av ) )

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

  # Test the addition operation.
  print( "ADD (+) tests:" )
  yield from alu_ft( alu, 0, 0, C_ADD, 0 )
  yield from check_nzv( alu, 0, 1, 0 )
  yield from alu_ft( alu, 0, 1, C_ADD, 1 )
  yield from check_nzv( alu, 0, 0, 0 )
  yield from alu_ft( alu, 1, 0, C_ADD, 1 )
  yield from check_nzv( alu, 0, 0, 0 )
  yield from alu_ft( alu, 0xFFFFFFFF, 1, C_ADD, 0 )
  yield from check_nzv( alu, 0, 1, 0 )
  yield from alu_ft( alu, 29, 71, C_ADD, 100 )
  yield from check_nzv( alu, 0, 0, 0 )
  yield from alu_ft( alu, 0x80000000, 0x80000000, C_ADD, 0 )
  yield from check_nzv( alu, 0, 1, 1 )
  yield from alu_ft( alu, 0x7FFFFFFF, 0x7FFFFFFF, C_ADD, 0xFFFFFFFE )
  yield from check_nzv( alu, 1, 0, 1 )

  # Test the subtraction operation.
  print( "SUB (-) tests:" )
  yield from alu_ft( alu, 0, 0, C_SUB, 0 )
  yield from check_nzv( alu, 0, 1, 0 )
  yield from alu_ft( alu, 0, 1, C_SUB, 0xFFFFFFFF )
  yield from check_nzv( alu, 1, 0, 0 )
  yield from alu_ft( alu, 1, 0, C_SUB, 1 )
  yield from check_nzv( alu, 0, 0, 0 )
  yield from alu_ft( alu, 0xFFFFFFFF, 1, C_SUB, 0xFFFFFFFE )
  yield from check_nzv( alu, 1, 0, 0 )
  yield from alu_ft( alu, 0xFFFFFFFF, 0xFFFFFFFF, C_SUB, 0 )
  yield from check_nzv( alu, 0, 1, 0 )
  yield from alu_ft( alu, 0x7FFFFFFF, 0x80000000, C_SUB, 0xFFFFFFFF )
  yield from check_nzv( alu, 1, 0, 1 )
  yield from alu_ft( alu, 0x80000000, 0x7FFFFFFF, C_SUB, 1 )
  yield from check_nzv( alu, 0, 0, 1 )

  # Test the '==' comparison operation.
  print( "CMP (==) tests:" )
  yield from alu_ft( alu, 0, 0, C_CEQ, 1 )
  yield from alu_ft( alu, 1, 0, C_CEQ, 0 )
  yield from alu_ft( alu, 0, 1, C_CEQ, 0 )
  yield from alu_ft( alu, 42, 42, C_CEQ, 1 )
  yield from alu_ft( alu, -42, -42, C_CEQ, 1 )
  yield from alu_ft( alu, 42, -42, C_CEQ, 0 )

  # Test the '<' comparison operation.
  print( "CMP (<) tests:" )
  yield from alu_ft( alu, 0, 0, C_CLT, 0 )
  yield from alu_ft( alu, 1, 0, C_CLT, 0 )
  yield from alu_ft( alu, 0, 1, C_CLT, 1 )
  yield from alu_ft( alu, -1, 0, C_CLT, 1 )
  yield from alu_ft( alu, -42, -10, C_CLT, 1 )
  yield from alu_ft( alu, -10, -42, C_CLT, 0 )

  # Test the '<=' comparison operation.
  print( "CMP (<=) tests:" )
  yield from alu_ft( alu, 0, 0, C_CLE, 1 )
  yield from alu_ft( alu, 1, 0, C_CLE, 0 )
  yield from alu_ft( alu, 0, 1, C_CLE, 1 )
  yield from alu_ft( alu, -1, 0, C_CLE, 1 )
  yield from alu_ft( alu, -42, -10, C_CLE, 1 )
  yield from alu_ft( alu, -10, -42, C_CLE, 0 )
  yield from alu_ft( alu, -42, -42, C_CLE, 1 )

  # Test the shift left operation.
  print ( "SHL (<<) tests:" )
  yield from alu_ft( alu, 0x00000001, 0, C_SHL, 0x00000001 )
  yield from alu_ft( alu, 0x00000001, 1, C_SHL, 0x00000002 )
  yield from alu_ft( alu, 0x00000001, 4, C_SHL, 0x00000010 )
  yield from alu_ft( alu, 0x00000010, 4, C_SHL, 0x00000100 )
  yield from alu_ft( alu, 0x80000000, 1, C_SHL, 0x00000000 )

  # Test the shift right operation.
  print ( "SHR (>>) tests:" )
  yield from alu_ft( alu, 0x00000001, 0, C_SHR, 0x00000001 )
  yield from alu_ft( alu, 0x00000001, 1, C_SHR, 0x00000000 )
  yield from alu_ft( alu, 0x00000011, 1, C_SHR, 0x00000001 )
  yield from alu_ft( alu, 0x00000010, 1, C_SHR, 0x00000001 )
  yield from alu_ft( alu, 0x80000000, 1, C_SHR, 0x40000000 )
  yield from alu_ft( alu, 0x80000000, 4, C_SHR, 0x08000000 )

  # Test the shift right with sign extension operation.
  print ( "SRA (>> + sign extend) tests:" )
  yield from alu_ft( alu, 0x00000001, 0, C_SRA, 0x00000001 )
  yield from alu_ft( alu, 0x00000001, 1, C_SRA, 0x00000000 )
  yield from alu_ft( alu, 0x00000011, 1, C_SRA, 0x00000001 )
  yield from alu_ft( alu, 0x00000010, 1, C_SRA, 0x00000001 )
  yield from alu_ft( alu, 0x80000000, 1, C_SRA, 0xC0000000 )
  yield from alu_ft( alu, 0x80000000, 4, C_SRA, 0xF8000000 )

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
