      SUBROUTINE BKVGEH(LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book VGEH, the VTX geometry header
C-
C-   Inputs  :  none
C-   Outputs : LBANK = address of the created bank
C-                   = 0 if an error has occured.
C-   Controls:  none
C-
C-   Created   3-JUL-1989   Thomas G. Trippe
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVGEH.LINK'
C
      INTEGER NFORM,LBANK,ISETVN
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL MZFORM ( 'VGEH', '2I -F', NFORM )
      ENDIF
C
      IF (LSVTX.EQ.0) THEN
        LBANK = 0
        GO TO 999
      ENDIF
C
      CALL MZBOOK ( IDVSTP, LVGEH, LSVTX, -IZVGEH, 'VGEH',
     +     4, 4, 23, NFORM, 0 )
      LBANK=LVGEH
      IC ( LVGEH ) = ISETVN(IC(LVGEH),1)   ! Set version number
C
  999 RETURN
      END
