      SUBROUTINE EZUDMP (LUNOUT,OUTLIN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : User hook call by EZDUMP to write out one
C-                         line. If LUN = 0 or 6 call INTMSG.
C-
C-   Inputs  : LUNOUT      Output unit number
C-             OUTLIN      String to be written out
C-   Outputs :
C-   Controls:
C-
C-   Created  10-MAR-1989   Harrison B. Prosper
C-   Updated  12-Feb-1992   Herbert Greenlee
C-      UNIX version.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LUNOUT,L
      CHARACTER*(*) OUTLIN
      CHARACTER*132 CTEMP
C----------------------------------------------------------------------
      L = LEN(OUTLIN)
      IF ( (LUNOUT .GT. 0) .AND. (LUNOUT .NE. 6) ) THEN
        CTEMP = ' '//OUTLIN(1:L)
        WRITE(UNIT=LUNOUT,FMT='(A)')CTEMP(1:L+1)
      ELSE
        CTEMP = ' '//OUTLIN(1:L)
        CALL INTMSG(CTEMP(1:L+1))
      ENDIF
  999 RETURN
      END
