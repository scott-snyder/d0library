      SUBROUTINE BKVWAL(NUMVOL,LVWAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Book VWAL, the VTX volumes 
C-
C-   Inputs  :  NUMVOL = number of volumes
C-   Outputs :  LVWAL  = link to the VWAL bank
C-   Controls:  none
C-
C-   Created  21-JUN-1989   Thomas G. Trippe
C-   Updated  14-JUN-1990   Peter Grudberg  IXSTP ==> IDVSTP 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVWAL.LINK'
C
      INTEGER NUMVOL, NPARVL, LVWAL, NFORMA, ISETVN
      PARAMETER (NPARVL = 7 )
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL MZFORM ( 'VWAL', '2I / 1H 5I 1H', NFORMA)
      ENDIF
      CALL MZBOOK ( IDVSTP, LVWAL, LVGEH, -IZVWAL, 'VWAL', 0, 0,
     +              2 + NUMVOL*NPARVL, NFORMA, 0 )
      IC(LVWAL) = ISETVN(IC(LVWAL),1)   ! Set version number
      IC(LVWAL+1) = NUMVOL
      IC(LVWAL+2) = NPARVL
  999 RETURN
      END
