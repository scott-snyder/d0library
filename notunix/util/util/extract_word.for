      SUBROUTINE EXTRACT_WORD (SUBSTR,STRING,II,JJ,KK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Search STRING for SUBSTR and return the
C-   start II and end JJ of the word containing the specified sub-string.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   7-NOV-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) SUBSTR
      CHARACTER*(*) STRING
      INTEGER       II,JJ,KK
C
      INTEGER I,J,K,L,LL,N,III,JJJ,KKK,LLL
C----------------------------------------------------------------------
      II = 1
      JJ = 0
      LL = LEN(SUBSTR)
      LLL= LEN(STRING)
C
      III= 1
  100 CONTINUE
      CALL WORD (STRING(III:LLL),I,J,K)
      IF ( K .GT. 0 ) THEN
        IF ( INDEX(STRING(III+I-1:III+J-1),SUBSTR(1:LL)) .GT. 0 ) THEN
          II = III+I-1
          JJ = III+J-1
        ELSE
          III = III + J
          IF ( III .LE. LLL ) GOTO 100
        ENDIF
      ENDIF
C
      KK = JJ - II + 1
  999 RETURN
      END
