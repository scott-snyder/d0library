      SUBROUTINE SPLTCH (INSTR,OUTSTR,NN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Split a string INSTR into holleriths, that is
C-                         4-character sub-strings enclosed within
C-                         single quotes, and return the result as a
C-                         concantenation of these sub-strings dilimited
C-                         by spaces.
C-
C-   Inputs  : INSTR
C-   Outputs : OUTSTR
C-             NN
C-   Controls: 
C-
C-   Created   8-NOV-1988   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) INSTR,OUTSTR
      CHARACTER*255 STRING
      INTEGER I,J,K,L,M,N,II,JJ,NN
C----------------------------------------------------------------------
      N = LEN(INSTR)
      M = LEN(OUTSTR)
      K = (N+3)/4
      II = 1
      JJ = 0
      STRING = INSTR(1:N)
      OUTSTR = ''''//STRING(II:II+3)//''''
      II = II + 4
      JJ = JJ + 7
      DO I = 2,K
        IF ( (JJ+6) .LE. M ) THEN
        OUTSTR = OUTSTR(1:JJ)//''''//STRING(II:II+3)//''''
        II = II + 4
        JJ = JJ + 7
        ELSE
          GOTO 200
        ENDIF
      ENDDO
  200 CONTINUE
      NN = JJ - 1
  999 RETURN
      END
