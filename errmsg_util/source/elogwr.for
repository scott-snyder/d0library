      SUBROUTINE ELOGWR(LUN,MSG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write to logging device LUN for error message
C-      logging.  THIS VERSION SIMPLY WRITES TO A FORTRAN FILE
C-
C-   Inputs  :
C-              LUN     logical unit of device
C-              MSG     message, 1st character as carriage control
C-   Outputs : None
C-   Controls: None
C-
C-   Created   3-JAN-1989   James T. Linnemann
C-   Modified 16-AUG-1992   sss - compile on ibm
C-   Modified 25-NOV-1995   sss - compile on linux
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LUN,I,J,K,N
      CHARACTER*(*) MSG
C&IF SIUNIX, IBMAIX, LINUX
C&      CHARACTER*132 CTEMP
C&ENDIF
C----------------------------------------------------------------------
      CALL SWORDS(MSG,I,J,N)
      IF(N.GT.0)  THEN
        K = MIN(J,I+131)
        IF (LUN.NE.6) THEN
C&IF SIUNIX, IBMAIX, LINUX
C&        CTEMP = ' '//MSG(I:K)
C&        WRITE(LUN,100) CTEMP
C&ELSE
          WRITE(LUN,100) ' '//MSG(I:K)
C&ENDIF
        ELSE
          CALL EWRNWR(MSG)  !special treatment of screen messages
        ENDIF
      ENDIF

  999 RETURN
  100 FORMAT(A)
      END
