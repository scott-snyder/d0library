      SUBROUTINE ZBANK_HFN_TAG (LABEL,NMAX,NXT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   6-AUG-1994   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) LABEL
      CHARACTER TAG*300
      INTEGER NMAX,NXT,I,J,K,L
C----------------------------------------------------------------------
C
C ****  Look for end bracket "]"
C
      L = INDEX(LABEL,']')
      IF (L.LE.0) THEN ! no variable length stuff
        NMAX = 1
        NXT = 1
C
C ****  look for commas
C
        TAG = LABEL
        DO WHILE (INDEX(TAG,',').GT.0)
          NXT = NXT + 1
          CALL WORD(TAG,I,J,K)
          I = INDEX(TAG,',')
          TAG = TAG(I+1:J)
        END DO
      ELSE
        I = INDEX(LABEL,',')
        READ(LABEL(I+1:L-1),*)NMAX
        CALL WORD(LABEL,I,J,K)
        TAG = LABEL(L+1:J)
        DO WHILE (INDEX(TAG,',').GT.0)
          NXT = NXT + 1
          CALL WORD(TAG,I,J,K)
          I = INDEX(TAG,',')
          TAG = TAG(I+1:J)
        END DO
      END IF
  999 RETURN
      END
