      SUBROUTINE L0_RUN1A_SETUP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill the geometry cell - L0 channel number array 
C-                         and light guide propagation time array.
C-                         Run 1A Geometry is set up.
C-
C-   Inputs  : none
C-   Outputs : CELL : channel number associated to scintillator tiles.
C-             INFO : (common block) light guide propagation times.
C-   Controls: none
C-
C-   Created  29-JAN-1994   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:LV0PARAM.INC'
      INTEGER CELL(2,5,9,4)
      INTEGER J
C----------------------------------------------------------------------
C
C ****  DO NORTH SHORT COUNTERS
C
      CELL(2,1,1,3)= 1
      CELL(2,1,4,4)= 2
      CELL(2,1,1,1)= 3
      CELL(2,1,4,2)= 4
      CELL(2,2,6,2)= 5
      CELL(2,2,4,3)= 6
      CELL(2,2,6,4)= 7
      CELL(2,2,4,1)= 8
      CELL(2,3,5,2)= 9
      CELL(2,3,5,3)= 10
      CELL(2,3,5,4)= 11
      CELL(2,3,5,1)= 12
      CELL(2,2,7,3)= 13
      CELL(2,3,6,3)= 14
      CELL(2,2,3,4)= 15
      CELL(2,3,4,4)= 16
      CELL(2,2,7,1)= 17
      CELL(2,3,6,1)= 18
      CELL(2,2,3,2)= 19
      CELL(2,3,4,2)= 20
C
C ****  DO SOUTH SHORT COUNTERS
C
      CELL(1,1,1,1)= 37
      CELL(1,1,4,4)= 38
      CELL(1,1,1,3)= 39
      CELL(1,1,4,2)= 40
      CELL(1,2,4,3)= 41
      CELL(1,2,6,2)= 42
      CELL(1,2,4,1)= 43
      CELL(1,2,6,4)= 44
      CELL(1,3,5,4)= 45
      CELL(1,3,5,3)= 46
      CELL(1,3,5,2)= 47
      CELL(1,3,5,1)= 48
      CELL(1,3,4,4)= 49
      CELL(1,2,3,4)= 50
      CELL(1,3,6,3)= 51
      CELL(1,2,7,3)= 52
      CELL(1,3,4,2)= 53
      CELL(1,2,3,2)= 54
      CELL(1,3,6,1)= 55
      CELL(1,2,7,1)= 56
C
C ****  LONG COUNTERS
C
      DO 20  J = 1 , 9
        CELL(2,4,J,1)=24
        CELL(2,4,J,4)=23
        CELL(2,4,J,3)=22
        CELL(2,4,J,2)=21
        CELL(2,5,J,1)=32
        CELL(2,5,J,4)=31
        CELL(2,5,J,3)=30
        CELL(2,5,J,2)=29
        CELL(1,4,J,1)=60
        CELL(1,4,J,4)=57
        CELL(1,4,J,3)=58
        CELL(1,4,J,2)=59
        CELL(1,5,J,1)=68
        CELL(1,5,J,4)=65
        CELL(1,5,J,3)=66
        CELL(1,5,J,2)=67
   20 CONTINUE
C
C ****  Fill up INFO ( in ps )
C
      INFO(1,3)=1400.   ! LT1 type counters have 21 cm of light guide (LG)
      INFO(2,3)=1400.   ! LT1
      INFO(3,3)=1400.   ! LT1
      INFO(4,3)=1400.   ! LT1
      INFO(5,3)=1400.   ! LT2 DIV 6 have 21 cm of LG
      INFO(6,3)=1400.   ! LT2 DIV 6
      INFO(7,3)=1400.   ! LT2 DIV 6
      INFO(8,3)=1400.   ! LT2 DIV 6
      INFO(9,3)=1870.   ! LT3 DIV 5 have 28 cm of LG
      INFO(10,3)=1870.  ! LT3 DIV 5
      INFO(11,3)=1870.  ! LT3 DIV 5
      INFO(12,3)=1870.  ! LT3 DIV 5
      INFO(13,3)=930.   ! LT2 DIV 3 have 14 cm of LG
      INFO(14,3)=1400.  ! LT3 DIV 4 have 21 cm of LG
      INFO(15,3)=930.   ! LT2 DIV 3
      INFO(16,3)=1400.  ! LT3 DIV 4
      INFO(17,3)=930.   ! LT2 DIV 3
      INFO(18,3)=1400.  ! LT3 DIV 4
      INFO(19,3)=930.   ! LT2 DIV 3
      INFO(20,3)=1400.  ! LT3 DIV 4
      DO 11 J = 21 , 36
        INFO(J,3)= 980. ! long counters have half-length of 14.75 cm
   11 CONTINUE
      INFO(37,3)=1400.  ! LT1 type counters have 21 cm of light guide (LG)
      INFO(38,3)=1400.  ! LT1
      INFO(39,3)=1400.  ! LT1
      INFO(40,3)=1400.  ! LT1
      INFO(41,3)=1400.  ! LT2 DIV 6 have 21 cm of LG
      INFO(42,3)=1400.  ! LT2 DIV 6
      INFO(43,3)=1400.  ! LT2 DIV 6
      INFO(44,3)=1400.  ! LT2 DIV 6
      INFO(45,3)=1870.  ! LT3 DIV 5 have 28 cm of LG
      INFO(46,3)=1870.  ! LT3 DIV 5
      INFO(47,3)=1870.  ! LT3 DIV 5
      INFO(48,3)=1870.  ! LT3 DIV 5
      INFO(49,3)=1400.  ! LT3 DIV 4 have 21 cm of LG
      INFO(50,3)=930.   ! LT2 DIV 3 have 14 cm of LG
      INFO(51,3)=1400.  ! LT3 DIV 4
      INFO(52,3)=930.   ! LT2 DIV 3
      INFO(53,3)=1400.  ! LT3 DIV 4
      INFO(54,3)=930.   ! LT2 DIV 3
      INFO(55,3)=1400.  ! LT3 DIV 4
      INFO(56,3)=930.   ! LT2 DIV 3
      DO 12 J = 57 , 72
        INFO(J,3)= 980. ! long counters have half-length of 14.75 cm
   12 CONTINUE
C----------------------------------------------------------------------
  999 RETURN
      END
