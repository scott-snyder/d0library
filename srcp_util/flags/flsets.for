      SUBROUTINE FLSETS(STRNG,TRUTH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       Set all flags beginning with string STRNG to TRUTH
C-   Inputs  : 
C-   STRNG = string to look for
C-   TRUTH = value to set flags 
C-
C-   Created  17-APR-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) STRNG
      INTEGER       ID,N
      LOGICAL       TRUTH,FOUND
      INCLUDE 'D0$INC:FLAGS.INC'
      INCLUDE 'D0$INC:FLAGNM.INC'
C----------------------------------------------------------------------
      N    = LEN(STRNG)
      IF ( N .GT. MAXCHR ) N = MAXCHR
      DO 1 ID =1,NUMFLG
        IF(NAMFLG(ID)(1:N).EQ.STRNG) BOOLE(ID)=TRUTH
    1 CONTINUE
  999 RETURN
      END
