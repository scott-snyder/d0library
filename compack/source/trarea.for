      REAL FUNCTION TRAREA(TXT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert a character string of numbers to
C-                         REAL.Output error messages for full screen mode.
C-
C-   Inputs  : TXT: String to be converted
C-   Outputs : None
C-   Controls: None
C-
C-   Created 19-MAY-1989   Jan S. Hoftun  (From CONREA)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TXT
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER TRULEN,J,K,I,LIBPUT
      CHARACTER*1 IC
      LOGICAL NOPOINT
C----------------------------------------------------------------------
      J=TRULEN(TXT)
      NOPOINT=.TRUE.
      DO 100 K=1,J
        IC=TXT(K:K)
        IF((IC.LT.'0'.OR.IC.GT.'9').AND.IC.NE.'-'.AND.IC.NE.'+'.AND.
     *  IC.NE.' '.AND.IC.NE.'.'.AND.IC.NE.'E'.AND.IC.NE.'e') THEN
          IF(SPLFLG.OR..NOT.FULSCR) THEN
            CALL INTMSG('0Illegal character for REAL-->'//IC//CHAR(7))
          ELSE
            I=LIBPUT(' Illegal character for REAL-->'//IC//CHAR(7),
     &        PBROWS-2,1,1)
          ENDIF
          GOTO 10
        ENDIF
        IF(IC.EQ.'.') THEN
          IF(NOPOINT) THEN
            NOPOINT=.FALSE.
          ELSE
          IF(SPLFLG.OR..NOT.FULSCR) THEN
            CALL INTMSG('0Two decimal points found!'//CHAR(7))
          ELSE
            I=LIBPUT('0Two decimal points found!'//CHAR(7),
     &        PBROWS-2,1,1)
          ENDIF
            GOTO 10
          ENDIF
        ENDIF
  100 CONTINUE
      READ(TXT(1:J),402) TRAREA
  402 FORMAT(E80.0)
      GOTO 20
   10 CONTINUE
      TRAREA=-999999.99
   20 CONTINUE
      RETURN
      END
