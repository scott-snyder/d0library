      SUBROUTINE BLFWAL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Builds the bank FWAL hanging from FGEH. This 
C-                         bank contains the design values for the overall 
C-                         theta and phi modules and is mainly used in
C-                         D0GEANT. 
C-
C-   Inputs  : 
C-   Outputs : 
C-
C-   Created  12-JUL-1988   Jeffrey Bantly   
C-   Updated   3-OCT-1989   Jeffrey Bantly  re-organize geometry for speedup 
C-   Updated  11-MAY-1992   Robert E. Avery  Change MZFORM call to correspond
C-                              correctly to what is in bank. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INCLUDE 'D0$LINKS:IZFWAL.LINK'
C
      INTEGER NUMVOL, NPARVL, I, NFORMA, LKFWAL
      PARAMETER (NUMVOL=3, NPARVL = 11 )
      INTEGER IOFS(NPARVL-2, NUMVOL)
C
      REAL XFTHA(5),XFTHB(5),XFPH(5) 
      REAL YFTH(3,2),YFPH(3) 
C
      CHARACTER*4 VOLNAM(NUMVOL), VOLMOT
      DATA VOLNAM / 'FTH ','FTH ','FPH '/
      DATA VOLMOT / 'FDC '/
C
C----------------------------------------------------------------------
C
C   Book the passive materials head bank FWAL
C
      CALL MZFORM ( 'FWAL', '2I / 1H 8F 1I 1H', NFORMA)
      CALL MZBOOK ( IDVSTP, LFWAL, LFGEH, -IZFWAL, 'FWAL', 3, 3,
     &              2 + NUMVOL*NPARVL, NFORMA, 0 )
C
C     Design values for FDC modules Theta and Phi.
C
C    LC     C/IC
C
C             +1       I   Number of volumes                      3
C             +2       I   Number of parameters for each volume  11
C
C  for each volume:  ( J = 2 + (volume - 1) * parameters per volume )
C
C             +1       H   Volume name                                 
C             +2       F   Inner radius of volume 
C             +3       F   Outer radius of volume   
C             +4       F   Half-thickness of volume
C             +5       F   Beginning angle of volume
C             +6       F   Ending angle of volume
C             +7       F   X position of volume
C             +8       F   Y position of volume
C             +9       F   Z position of volume
C            +10       I   Rotation number
C            +11       H   Volume in which to position this volume (FDC)
C
C        FDC                          : mother volume of FDC (in FGEH)
C         --->FTH                     : theta chamber, inner
C         --->FTH                     :              , outer
C         --->FPH                     : phi chamber
C
C*************************************************************************
C
C  Sizes of pieces
C
C  'TUBS' theta chamber mother volume...
      DATA XFTHA/  7.50, 66.04, 5.05, 32., 148./
      DATA XFTHB/  7.50, 66.04, 5.05, -45., 45./
C  'TUBE' phi chamber mother volume...
      DATA XFPH/  8.00, 66.04, 8.10, 0.0, 0.0/
C
C  Position of theta and phi chambers in FDC volume...
C
      DATA YFTH/ 0.00, 0.00,  13.15,
     2           0.00, 0.00, -13.15/
      DATA YFPH/ 0.00, 0.00, 0.00/
C
C  Fill the arrays with the parameters
C
      IC ( LFWAL + 1 ) = NUMVOL
      IC ( LFWAL + 2 ) = NPARVL
C
      LKFWAL = LFWAL + 2
      CALL UCTOH(VOLNAM(1), C(LKFWAL+1), 4, 4 )
      C  ( LKFWAL + 2 ) = XFTHA(1)
      C  ( LKFWAL + 3 ) = XFTHA(2)
      C  ( LKFWAL + 4 ) = XFTHA(3)
      C  ( LKFWAL + 5 ) = XFTHA(4)
      C  ( LKFWAL + 6 ) = XFTHA(5)
      C  ( LKFWAL + 7 ) = YFTH(1,1)
      C  ( LKFWAL + 8 ) = YFTH(2,1)
      C  ( LKFWAL + 9 ) = YFTH(3,1)
      IC ( LKFWAL + 10) = 9007
      CALL UCTOH(VOLMOT, C(LKFWAL+11), 4, 4 )
C
      LKFWAL = LKFWAL + NPARVL
      CALL UCTOH(VOLNAM(2), C(LKFWAL+1), 4, 4 )
      C  ( LKFWAL + 2 ) = XFTHB(1)
      C  ( LKFWAL + 3 ) = XFTHB(2)
      C  ( LKFWAL + 4 ) = XFTHB(3)
      C  ( LKFWAL + 5 ) = XFTHB(4)
      C  ( LKFWAL + 6 ) = XFTHB(5)
      C  ( LKFWAL + 7 ) = YFTH(1,2)
      C  ( LKFWAL + 8 ) = YFTH(2,2)
      C  ( LKFWAL + 9 ) = YFTH(3,2)
      IC ( LKFWAL + 10) = 9001
      CALL UCTOH(VOLMOT, C(LKFWAL+11), 4, 4 )
C
      LKFWAL = LKFWAL + NPARVL
      CALL UCTOH(VOLNAM(3), C(LKFWAL+1), 4, 4 )
      C  ( LKFWAL + 2 ) = XFPH(1)
      C  ( LKFWAL + 3 ) = XFPH(2)
      C  ( LKFWAL + 4 ) = XFPH(3)
      C  ( LKFWAL + 5 ) = XFPH(4)
      C  ( LKFWAL + 6 ) = XFPH(5)
      C  ( LKFWAL + 7 ) = YFPH(1)
      C  ( LKFWAL + 8 ) = YFPH(2)
      C  ( LKFWAL + 9 ) = YFPH(3)
      IC ( LKFWAL + 10) = 9001
      CALL UCTOH(VOLMOT, C(LKFWAL+11), 4, 4 )
C
C  Create and build banks FWTA, FWTB, FWPH hanging from FWAL
C
      CALL BLFWTA
      CALL BLFWTB
      CALL BLFWPH
C
C---------------------------------------------------------------------
  999 RETURN
      END
