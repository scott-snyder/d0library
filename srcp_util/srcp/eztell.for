      SUBROUTINE EZTELL (BKNAME,NN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return name of currently selected SRCP bank.
C-
C-   Inputs  : None
C-
C-   Outputs : BKNAME      Bank name. Up to 32 characters.
C-             NN          Number of characters in name
C-   Controls: None
C-
C-   Created   9-MAR-1989   Harrison B. Prosper
C-   Updated  15-MAY-1990   Harrison B. Prosper   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) BKNAME
      INTEGER NN,I,J
C
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:NMSRCP.INC'
C----------------------------------------------------------------------
C
C       ISRCP is the pointer to address of the
C       selected bank
C
C ****  Clear error flag
C
      ERRSRC = EZS_SUCCESS
C
      IF ( ISRCP .GT. 0 ) THEN
        BKNAME = MSRCP(ISRCP)
        CALL WORD (BKNAME,I,J,NN)
      ELSE
        ERRSRC = EZS_BANK_NOTSELECTED
        BKNAME = ' '
        NN = 0
      ENDIF
C
  999 RETURN
      END
