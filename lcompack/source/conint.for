      INTEGER FUNCTION CONINT(TXT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert a character string of numbers to
C-                         INTEGER
C-
C-   Inputs  : TXT: String to be converted
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TXT
      INTEGER I,K,J,NEG,TRULEN
      LOGICAL INPROG
      CHARACTER*1 IC
      CHARACTER*40 LOCTXT
C----------------------------------------------------------------------
      I=0
      INPROG=.FALSE.
      NEG=1
      LOCTXT=TXT
      IF(TRULEN(LOCTXT).GT.0) THEN
        DO WHILE (LOCTXT(1:1).EQ.' ')
          LOCTXT=LOCTXT(2:)
        ENDDO
        IF((TRULEN(LOCTXT).GT.9.AND.LOCTXT(1:1).NE.'-').OR.
     *   TRULEN(LOCTXT).GT.10) THEN
          CALL INTMSG('0String too LONG to be converted to integer'//
     &         CHAR(7))
          I=9999999
        ELSE
          DO 1001 K=1,TRULEN(LOCTXT)
            IF(LOCTXT(K:K).EQ.'-'.AND..NOT.INPROG) NEG=-1
            IF(LOCTXT(K:K).NE.' ') THEN
              INPROG=.TRUE.
              IC=LOCTXT(K:K)
              IF((IC.LT.'0'.OR.IC.GT.'9').AND.IC.NE.'-'.AND.IC.NE.'+')
     &        THEN
                CALL INTMSG('0Illegal character for INTEGER-->'//
     &               IC//CHAR(7))
                GOTO 1002
              ELSE
                READ(LOCTXT(K:K),902,ERR=1002,END=1002) J
  902           FORMAT(I1)
                I=I*10+J
              ENDIF
            ELSE
              IF(INPROG) GOTO 1002
            ENDIF
 1001     CONTINUE
 1002     CONTINUE
        ENDIF
        CONINT=NEG*I
      ELSE
        CALL INTMSG('0Illegal character for INTEGER-->'//IC//CHAR(7))
        CONINT=9999999
      ENDIF
      RETURN
      END
