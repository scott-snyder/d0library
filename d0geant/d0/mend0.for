      LOGICAL FUNCTION MEND0()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-                      Menu 7 D0
C-
C-
C-   Returned value  : TRUE
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-   Calledby: LUIGET
C-
C-   Created  01-May-1987   Stephan Linn
C-   Updated  30-JUN-1989   Rajendran Raja
C-
C-   Updated  14-JUL-1989   Harrison B. Prosper
C-   Made into logical function. Added D0COM.INC
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
c      INCLUDE 'D0$INC:D0COM.INC'
C
      CHARACTER*40 NAME
      CHARACTER*32 CMD
      INTEGER MPAR
C
C----------------------------------------------------------------------
      MEND0 = .TRUE.

      CALL KUPATL(CMD,MPAR)

      IF( CMD .EQ. 'DISPLAY_SWITCH')THEN
        CALL KUGETI(DTRK)
        CALL KUGETI(DHIT)
        CALL KUGETI(DDIG)
        GOTO 999
      ENDIF

  999 RETURN
      END
