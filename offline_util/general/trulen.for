      INTEGER FUNCTION TRULEN(INTXT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find real length of string, excluding trailing
C-                         blanks.
C-
C-   Inputs  : INTXT: String to find length of.
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) INTXT
      INTEGER K,I
C----------------------------------------------------------------------
      I=0
      DO 900 K=LEN(INTXT),1,-1
        IF(INTXT(K:K).NE.' ') THEN
          I=K
          GOTO 901
        ENDIF
  900 CONTINUE
  901 CONTINUE
      TRULEN=I
      RETURN
      END
