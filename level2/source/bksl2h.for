      SUBROUTINE BKSL2H(LSL2H)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book SL2H bank that hangs below SCAL
C-
C-   Inputs  :
C-   Outputs : LSL2H    Link to bank
C-   Controls:
C-
C-   Created  13-NOV-1990   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSL2H.LINK'
      INTEGER LSL2H
C----------------------------------------------------------------------
      LSL2H = 0
      IF (LSTPH .LE. 0) CALL INZSTP
C
      LSL2H = LC(LSTPH - IZSL2H)
      IF (LSL2H .LE. 0) THEN
        CALL MZBOOK(IDVSTP,LSL2H,LSTPH,-IZSL2H,'SL2H',50,50,2,2,0)
        IC(LSL2H + 1) = 1               ! Version number
      END IF

  999 RETURN
      END
