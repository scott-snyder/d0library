      SUBROUTINE BLFGEH_V2
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create and fill banks FGEH, FMAT, FWAL, FDRT
C-      New version comes first in linear chain.
C-
C-   Inputs  :
C-   Outputs :
C-
C-   Based on BLFGEH, Created   6-JUN-1988   Jeffrey Bantly   
C-   Created   13-AUG-1992   Robert E. Avery   
C-      Store rotation numbers for FDC units (N&S) in FGEH bank, words 9 & 10.
C-      Move FDC in by ~ 1 7/8" 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZFGEH.LINK'
      INTEGER IRUMIN, IRUMAX, NFORM, LKSFDC
      INTEGER LZLAST
      REAL XFD(3), YFD(3)
C
      DATA IRUMIN, IRUMAX /0, 999999/
      DATA XFD, YFD / 7.50, 68.00, 18.41, 0.00, 0.00, 115.295/
C-------------------------------------------------------------------
C
C  Create a permanent link area FDCPRM, if not already done
C
      CALL FSPLNK
C
C  Books the bank SFDC as the top level FDC bank in IXSTP
C
      IF ( LSFDC .LE. 0 ) CALL BKSFDC(LKSFDC)
C
C  Books the top level Geometry bank FGEH, in linear chain if already exists.
C
      CALL MZFORM ( 'FGEH', '2I 6F 2I', NFORM )
      LFGEH = LZLAST(0,LSFDC-IZFGEH)
      IF ( LFGEH.GT.0 ) THEN
        CALL MZBOOK ( IDVSTP, LFGEH, LFGEH, 0, 'FGEH',
     &     3, 3, 10, NFORM, 0 )
      ELSE
        CALL MZBOOK ( IDVSTP, LFGEH, LSFDC, -IZFGEH, 'FGEH',
     &     3, 3, 10, NFORM, 0 )
      ENDIF
C
C ****  Fill bank FGEH
C              +1       I  Minimun Run number valid with this set
C              +2       I  Maximum Run number valid with this set
C              +3       F  Inner radius   \
C              +4       F  Outer radius    \     Overall size and position
C              +5       F  Half-thickness   \    of one FDC design frame in 
C              +6       F  Xcenter          /    the D0 frame, other is in
C              +7       F  Ycenter         /     negative Z direction.
C              +8       F  Zcenter        /
C              +9       I  Rotation      /    for NFDC (in Negative z)
C              +10      I  Rotation     /     for SFDC (in positive z)
C----------------------------------------------------------------------
C
      IC(LFGEH - 5)  = 2
      IC(LFGEH)      = IBSET(IC(LFGEH),0)
      IC(LFGEH + 1)  = IRUMIN
      IC(LFGEH + 2)  = IRUMAX
      C (LFGEH + 3)  = XFD(1)
      C (LFGEH + 4)  = XFD(2)
      C (LFGEH + 5)  = XFD(3)
      C (LFGEH + 6)  = YFD(1)
      C (LFGEH + 7)  = YFD(2)
      C (LFGEH + 8)  = YFD(3)
      C (LFGEH + 8)  = YFD(3)
      IC(LFGEH + 9)  = 9001
      IC(LFGEH + 10) = 9009
C
C ****  Create material bank FMAT
C
      CALL BLFMAT
C
C ****  Create Passive parts of the detector bank FWAL and sub-banks
C
      CALL BLFWAL
C
C ****  Create drift cells description bank FDRT and sub-banks
C
      CALL BLFDRT
C
C-------------------------------------------------------------------------
  999 RETURN
      END