      SUBROUTINE BLFDRT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fills bank FDRT hanging from FGEH. It is as
C-                         header for the FDTA, FDTB, and FTPH banks
C-
C-   Inputs  : none
C-   Outputs :
C-
C-   Created  12-MAY-1988   Jeffrey Bantly   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INCLUDE 'D0$LINKS:IZFDRT.LINK'
      INTEGER NFORMA
C
C----------------------------------------------------------------------
C
C  Book the FDC sensitive volumes head bank FDRT
C
      CALL MZFORM ( 'FDRT', '1I / 9F', NFORMA)
      CALL MZBOOK ( IDVSTP, LFDRT, LFGEH, -IZFDRT, 'FDRT', 3, 3,
     &              10, NFORMA, 0 )
C
C       Design Values for the FDC drift cells
C
C             +1       I   Number of banks hanging from FDRT      3
C
C        FDC                          : mother volume of FDC  (in FGEH)
C         --->FTH                     : theta chamber, inner  (in FWAL)
C         --->FTH                     :              , outer  (in FWAL)
C         --->FPH                     : phi chamber           (in FWAL)
C
C*************************************************************************
C
C  Fill the arrays with the parameters
C
      IC ( LFDRT + 1 ) = 3
C
C  Create and build banks FDTA, FDTB, FDPH hanging from FDRT
C
      CALL BLFDTA
      CALL BLFDTB
      CALL BLFDPH
C
C-------------------------------------------------------------------------
  999 RETURN
      END
