      INTEGER FUNCTION CONTRY(TXT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Try to convert a string to an integer, return
C-                         0 if not a real integer found first in the string.
C-                         Used to find commands #'s typed at prompt.
C-
C-   Inputs  : TXT: String to be converted
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TXT
      INTEGER I,K,J,NEG
      LOGICAL INPROG
      CHARACTER*1 IC
C----------------------------------------------------------------------
      I=0
      INPROG=.FALSE.
      NEG=1
      DO 1001 K=1,LEN(TXT)
        IF(TXT(K:K).EQ.'-'.AND..NOT.INPROG) NEG=-1
        IF(TXT(K:K).NE.' ') THEN
          IF(TXT(K:K).GT.'9'.OR.TXT(K:K).LT.'0') THEN
            GOTO 1002
          ENDIF
          INPROG=.TRUE.
          READ(TXT(K:K),902,ERR=1002,END=1002) J
  902     FORMAT(I1)
          I=I*10+J
        ELSE
          IF(INPROG) GOTO 1002
        ENDIF
 1001 CONTINUE
 1002 CONTINUE
      CONTRY=NEG*I
      RETURN
      END
