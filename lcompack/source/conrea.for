      REAL FUNCTION CONREA(TXT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert a character string of numbers to
C-                         REAL
C-
C-   Inputs  : TXT: String to be converted
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-   Updated  19-MAY-1989   Jan S. Hoftun  (Check for two decimal points) 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TXT
      INTEGER TRULEN,J,K,I
      CHARACTER*1 IC
      LOGICAL NOPOINT
C----------------------------------------------------------------------
      J=TRULEN(TXT)
      NOPOINT=.TRUE.
      DO 100 K=1,J
        IC=TXT(K:K)
        IF((IC.LT.'0'.OR.IC.GT.'9').AND.IC.NE.'-'.AND.IC.NE.'+'.AND.
     *  IC.NE.' '.AND.IC.NE.'.'.AND.IC.NE.'E'.AND.IC.NE.'e') THEN
          CALL INTMSG('0Illegal character for REAL-->'//IC//CHAR(7))
          GOTO 10
        ENDIF
        IF(IC.EQ.'.') THEN
          IF(NOPOINT) THEN
            NOPOINT=.FALSE.
          ELSE
            CALL INTMSG('0Two decimal points found!'//CHAR(7))
            GOTO 10
          ENDIF
        ENDIF
  100 CONTINUE
      READ(TXT(1:J),402) CONREA
  402 FORMAT(E80.0)
      GOTO 20
   10 CONTINUE
      CONREA=-999999.99
   20 CONTINUE
      RETURN
      END
