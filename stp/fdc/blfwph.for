      SUBROUTINE BLFWPH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Builds the bank FWPH hanging from FGEH. This banks
C-                         contains the design values for all the passive
C-                         material of the phi module and is mainly used
C-                         in D0GEANT. 
C-
C-   Inputs  : none
C-   Outputs : 
C-
C-   Created  12-MAY-1988   Jeffrey Bantly   
C-   Updated  19-JUL-1988   Jeffrey Bantly  corrections 
C-   Updated   3-OCT-1989   Jeffrey Bantly  re-organize geometry for speedup 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INCLUDE 'D0$LINKS:IZFWAL.LINK'
      INCLUDE 'D0$LINKS:IZFWPH.LINK'
C
      INTEGER NUMVOL, NPARVL, I, LKFWPH
      INTEGER NFORMA
      PARAMETER (NUMVOL=9, NPARVL = 9 )
C
      REAL XFDPH(3),XFIR(3),XFIW(3),XFOR(3),XFOW(3),XFHS(3)
      REAL YFDPH(3),YFIR(3,2),YFOR(3,2),YFIW(3),YFOW(3),YFHS(3,2)
C
      CHARACTER*4 VOLNAM(NUMVOL), VOLMOT, PHISHP
      DATA VOLNAM / 'FDPH','FIR0','FIR1','FIW ','FOR0','FOR1','FOW ',
     &              'FHS0','FHS1'/
      DATA VOLMOT, PHISHP / 'FPH ','TUBE'/
C
C----------------------------------------------------------------------
C
C  Book Phi chamber passive materials pieces bank
C
      CALL MZFORM ( 'FWPH', '3I 1H 2I / 1H 6F 1I 1H', NFORMA)
      CALL MZBOOK ( IDVSTP, LFWPH, LFWAL, -IZFWPH, 'FWPH', 0, 0,
     &              6 + NUMVOL*NPARVL, NFORMA, 0 )
C
C     Design values for FDC Phi chamber pieces that are passive material.
C
C    LC     C/IC
C
C
C             +1       I   # of volumes ( 9 )
C             +2       I   # of parameters for each volume ( 9 )
C             +3       I   # of types of volumes ( 1 )
C
C  for each type:
C
C             +1       H   Volume type ('tube')
C             +2       I   Number of type ( 9 )
C             +3       I   Number of special 'diagonal' 
C                          reflection volumes ( 0 )
C
C
C  for each volume:
C
C             +1       H   Volume name                                 
C             +2       F   Inner radius of volume 
C             +3       F   Outer radius of volume   
C             +4       F   Half-thickness of volume
C             +5       F   X position of volume
C             +6       F   Y position of volume
C             +7       F   Z position of volume
C             +8       I   Material number
C             +9       H   Volume in which to position this volume (FWPH)
C
C     The structure of geometry for the phi chamber is:
C        FDC                           : mother volume of FD
C         --->FTH                      : theta chambers
C         --->FPH                      : phi chamber
C               --->FDPH               : phi drift mother volume
C               --->FIR0 and FIR1      : inner aluminium rings (2)
C               --->FIW                : inner aluminium wall
C               --->FOR0 and FOR1      : outer aluminium rings (2)
C               --->FOW                : outer aluminium wall
C               --->FHS0 and FHS1      : honeycomb support (2)
C
C*************************************************************************
C
C  Sizes of pieces
C
C  'TUBE' phi chamber drift mother volume...
      DATA XFDPH/ 11.00, 61.29, 7.03/
C  'TUBE' phi chamber inner aluminium rings (2)...
      DATA XFIR/   8.00, 10.36, 0.32/
C  'TUBE' phi chamber inside aluminium wall...
      DATA XFIW/  10.36, 11.00, 7.03/
C  'TUBE' phi chamber outer aluminium rings (2)...
      DATA XFOR/  61.93, 66.04, 0.32/
C  'TUBE' phi chamber outside aluminium wall...
      DATA XFOW/  61.29, 61.93, 7.03/
C  'TUBE' phi chamber drift cell (undivided)...
C     DATA XFPC/  11.00, 61.29, 7.03/
C  'TUBE' phi chamber honeycomb supports (2)...
      DATA XFHS/   8.00, 66.04, 0.535/
C
C  Position of pieces in FPH volume...
C
C  phi chamber drift mother volume...
      DATA YFDPH/ 0.00, 0.00,  8.10/
C  phi chamber inner rings (2)...
      DATA YFIR/ 0.00, 0.00,  1.39,
     2           0.00, 0.00, 14.81/
C  phi chamber outer rings (2)...
      DATA YFOR/ 0.00, 0.00,  1.39,
     2           0.00, 0.00, 14.81/
C  phi chamber inside wall...
      DATA YFIW/ 0.00, 0.00,  8.10/
C  phi chamber outside wall...
      DATA YFOW/ 0.00, 0.00,  8.10/
C  phi chamber sensitive cell...
C      DATA YFPC/ 0.00, 0.00,  8.10/
C  phi chamber honeycomb support...
      DATA YFHS/ 0.00, 0.00,  0.535,
     2           0.00, 0.00, 15.665/
C
C  Fill the arrays with the parameters
C
      IC ( LFWPH + 1 ) = NUMVOL
      IC ( LFWPH + 2 ) = NPARVL
      IC ( LFWPH + 3 ) = 1
C
      LKFWPH = LFWPH + 3
      CALL UCTOH(PHISHP, C(LKFWPH+1), 4, 4 )
      IC ( LKFWPH + 2 ) = 9
      IC ( LKFWPH + 3 ) = 0
C
      LKFWPH = LKFWPH + 3
      CALL UCTOH(VOLNAM(1), C(LKFWPH+1), 4, 4 )
      C  ( LKFWPH + 2 ) = XFDPH(1)
      C  ( LKFWPH + 3 ) = XFDPH(2)
      C  ( LKFWPH + 4 ) = XFDPH(3)
      C  ( LKFWPH + 5 ) = YFDPH(1)
      C  ( LKFWPH + 6 ) = YFDPH(2)
      C  ( LKFWPH + 7 ) = YFDPH(3)
      IC ( LKFWPH + 8 ) = 91
      CALL UCTOH(VOLMOT, C(LKFWPH+9), 4, 4 )
C
      LKFWPH = LKFWPH + NPARVL
      CALL UCTOH(VOLNAM(2), C(LKFWPH+1), 4, 4 )
      C  ( LKFWPH + 2 ) = XFIR(1)
      C  ( LKFWPH + 3 ) = XFIR(2)
      C  ( LKFWPH + 4 ) = XFIR(3)
      C  ( LKFWPH + 5 ) = YFIR(1,1)
      C  ( LKFWPH + 6 ) = YFIR(2,1)
      C  ( LKFWPH + 7 ) = YFIR(3,1)
      IC ( LKFWPH + 8 ) = 93
      CALL UCTOH(VOLMOT, C(LKFWPH+9), 4, 4 )
C
      LKFWPH = LKFWPH + NPARVL
      CALL UCTOH(VOLNAM(3), C(LKFWPH+1), 4, 4 )
      C  ( LKFWPH + 2 ) = XFIR(1)
      C  ( LKFWPH + 3 ) = XFIR(2)
      C  ( LKFWPH + 4 ) = XFIR(3)
      C  ( LKFWPH + 5 ) = YFIR(1,2)
      C  ( LKFWPH + 6 ) = YFIR(2,2)
      C  ( LKFWPH + 7 ) = YFIR(3,2)
      IC ( LKFWPH + 8 ) = 93
      CALL UCTOH(VOLMOT, C(LKFWPH+9), 4, 4 )
C
      LKFWPH = LKFWPH + NPARVL
      CALL UCTOH(VOLNAM(4), C(LKFWPH+1), 4, 4 )
      C  ( LKFWPH + 2 ) = XFIW(1)
      C  ( LKFWPH + 3 ) = XFIW(2)
      C  ( LKFWPH + 4 ) = XFIW(3)
      C  ( LKFWPH + 5 ) = YFIW(1)
      C  ( LKFWPH + 6 ) = YFIW(2)
      C  ( LKFWPH + 7 ) = YFIW(3)
      IC ( LKFWPH + 8 ) = 93
      CALL UCTOH(VOLMOT, C(LKFWPH+9), 4, 4 )
C
      LKFWPH = LKFWPH + NPARVL
      CALL UCTOH(VOLNAM(5), C(LKFWPH+1), 4, 4 )
      C  ( LKFWPH + 2 ) = XFOR(1)
      C  ( LKFWPH + 3 ) = XFOR(2)
      C  ( LKFWPH + 4 ) = XFOR(3)
      C  ( LKFWPH + 5 ) = YFOR(1,1)
      C  ( LKFWPH + 6 ) = YFOR(2,1)
      C  ( LKFWPH + 7 ) = YFOR(3,1)
      IC ( LKFWPH + 8 ) = 93
      CALL UCTOH(VOLMOT, C(LKFWPH+9), 4, 4 )
C
      LKFWPH = LKFWPH + NPARVL
      CALL UCTOH(VOLNAM(6), C(LKFWPH+1), 4, 4 )
      C  ( LKFWPH + 2 ) = XFOR(1)
      C  ( LKFWPH + 3 ) = XFOR(2)
      C  ( LKFWPH + 4 ) = XFOR(3)
      C  ( LKFWPH + 5 ) = YFOR(1,2)
      C  ( LKFWPH + 6 ) = YFOR(2,2)
      C  ( LKFWPH + 7 ) = YFOR(3,2)
      IC ( LKFWPH + 8 ) = 93
      CALL UCTOH(VOLMOT, C(LKFWPH+9), 4, 4 )
C
      LKFWPH = LKFWPH + NPARVL
      CALL UCTOH(VOLNAM(7), C(LKFWPH+1), 4, 4 )
      C  ( LKFWPH + 2 ) = XFOW(1)
      C  ( LKFWPH + 3 ) = XFOW(2)
      C  ( LKFWPH + 4 ) = XFOW(3)
      C  ( LKFWPH + 5 ) = YFOW(1)
      C  ( LKFWPH + 6 ) = YFOW(2)
      C  ( LKFWPH + 7 ) = YFOW(3)
      IC ( LKFWPH + 8 ) = 93
      CALL UCTOH(VOLMOT, C(LKFWPH+9), 4, 4 )
C
      LKFWPH = LKFWPH + NPARVL
      CALL UCTOH(VOLNAM(8), C(LKFWPH+1), 4, 4 )
      C  ( LKFWPH + 2 ) = XFHS(1)
      C  ( LKFWPH + 3 ) = XFHS(2)
      C  ( LKFWPH + 4 ) = XFHS(3)
      C  ( LKFWPH + 5 ) = YFHS(1,1)
      C  ( LKFWPH + 6 ) = YFHS(2,1)
      C  ( LKFWPH + 7 ) = YFHS(3,1)
      IC ( LKFWPH + 8 ) = 94
      CALL UCTOH(VOLMOT, C(LKFWPH+9), 4, 4 )
C
      LKFWPH = LKFWPH + NPARVL
      CALL UCTOH(VOLNAM(9), C(LKFWPH+1), 4, 4 )
      C  ( LKFWPH + 2 ) = XFHS(1)
      C  ( LKFWPH + 3 ) = XFHS(2)
      C  ( LKFWPH + 4 ) = XFHS(3)
      C  ( LKFWPH + 5 ) = YFHS(1,2)
      C  ( LKFWPH + 6 ) = YFHS(2,2)
      C  ( LKFWPH + 7 ) = YFHS(3,2)
      IC ( LKFWPH + 8 ) = 94
      CALL UCTOH(VOLMOT, C(LKFWPH+9), 4, 4 )
C
C-----------------------------------------------------------------------
  999 RETURN
      END
