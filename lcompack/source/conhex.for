      INTEGER FUNCTION CONHEX(TXT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert a character string in HEX format to
C-                         INTEGER
C-
C-   Inputs  : TXT: String to be converted
C-   Outputs : None
C-   Controls: None
C-
C-   Updated  15-NOV-1988   Jan S. Hoftun   Use character string to
C-                                          look for match
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TXT
      INTEGER IC,I,K,TRULEN,JLEN
      LOGICAL INPROG,NEG
      CHARACTER*40 LOCTXT
      CHARACTER*80 TRANUP
      CHARACTER*16 VALIDS
      DATA VALIDS/'0123456789ABCDEF'/
C----------------------------------------------------------------------
      I=0
      INPROG=.FALSE.
      LOCTXT=TRANUP(TXT)
      DO WHILE (LOCTXT(1:1).EQ.' ')
        LOCTXT=LOCTXT(2:)
      ENDDO
      JLEN=TRULEN(LOCTXT)
      IC=INDEX(VALIDS,LOCTXT(1:1))
      IF(JLEN.GT.8) THEN
        CALL OUTMSG('0Converting string would cause integer overflow!'
     &              //CHAR(7))
        I=-1
      ELSE
        IF(JLEN.EQ.8.AND.IC.GT.8) THEN
          NEG=.TRUE.
          LOCTXT(1:1)=VALIDS(IC-8:IC-8)
        ELSE
          NEG=.FALSE.
        ENDIF
        DO 1001 K=1,JLEN
          IF(LOCTXT(K:K).NE.' ') THEN
            INPROG=.TRUE.
            IC=INDEX(VALIDS,LOCTXT(K:K))
            IF(IC.EQ.0) THEN
              CALL INTMSG('0Illegal character for HEX integer-->'//
     &             LOCTXT(K:K)//CHAR(7))
              I=-1
              GOTO 1002
            ELSE
              I=I*16+(IC-1)
            ENDIF
          ELSE
            IF(INPROG) GOTO 1002
          ENDIF
 1001   CONTINUE
 1002   CONTINUE
      ENDIF
      IF(NEG) THEN
C        CONHEX=-2147483648+I
        CONHEX = IBSET(I,31)
      ELSE
        CONHEX=I
      ENDIF
      RETURN
      END
