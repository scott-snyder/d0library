      SUBROUTINE EZZNID(ID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return next available ID for an SRCP bank.
C-   This ID points to a slot in the SRCP link area. If an ID is
C-   available then the counter NSRCP is incremented.
C-
C-   Inputs  : None
C-   Outputs : ID       [I]     Next available pointer into link area.
C-   Controls:
C-
C-   Created  10-MAY-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER ID
C
      INTEGER I
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
C----------------------------------------------------------------------
      ERRSRC = EZS_SUCCESS
C
      ID = 0
      IF ( NSRCP .GE. MXSRCP  ) THEN
        ERRSRC = EZS_TOOMANY_BANKS
        GOTO 999
      ENDIF
C
C ****  Some slots may be empty because of previously dropped
C ****  banks. Find next available slot for an SRCP bank
C
      NSRCP = NSRCP + 1 ! Increment SRCP bank counter
      ID = NSRCP
      I = 0
      DO WHILE ( I .LT. MXSRCP )
        I = I + 1
        IF ( KSRCP(I) .LE. 0 ) THEN
          ID = I
          GOTO 999
        ENDIF
      ENDDO
  999 RETURN
      END
