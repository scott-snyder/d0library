      SUBROUTINE BKVMAT(NMAT,LVMAT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Book VMAT, the VTX materials bank 
C-
C-   Inputs  :  NMAT  = number of materials
C-   Outputs :  LVMAT = link to the VMAT bank
C-   Controls:  none
C-
C-   Created  18-JUN-1989   Thomas G. Trippe
C-   Updated  14-JUN-1990   Peter Grudberg  IXSTP ==> IDVSTP 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVMAT.LINK'
C
      INTEGER NMAT, NWMAT, LVMAT, NFORM, ISETVN
      PARAMETER ( NWMAT = 11 )
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL MZFORM ( 'VMAT', '2I / 1I 5H 5F', NFORM)
      ENDIF
      CALL MZBOOK ( IDVSTP, LVMAT, LVGEH, -IZVMAT, 'VMAT', 0, 0,
     +              NMAT*NWMAT+2, NFORM, 0 )
      IC(LVMAT) = ISETVN(IC(LVMAT),1)   ! Set version number
      IC(LVMAT+1) = NMAT
      IC(LVMAT+2) = NWMAT
C
  999 RETURN
      END
