      SUBROUTINE GM_STATS(ID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : COMIS routine to write out histogram
C-   statistics.
C-
C-   Inputs  : ID   [I]   Histogram ID
C-   Outputs :
C-   Controls:
C-
C-   Created  15-DEC-1992   Harrison B. Prosper
C-
C----------------------------------------------------------------------
C      IMPLICIT NONE
      INTEGER ID
      REAL    MEAN,RMS,HSTATI
      INTEGER NENTRY
      VECTOR RUNNO
C----------------------------------------------------------------------
      IF ( ID .GT. 0 ) THEN
        MEAN = HSTATI(ID,1,' ',0)
        RMS  = HSTATI(ID,2,' ',0)
        CALL HNOENT(ID,NENTRY)
C
        WRITE(6,*)  RUNNO(1), NENTRY, MEAN, RMS, ID
        WRITE(80,*) RUNNO(1), NENTRY, MEAN, RMS, ID
      ENDIF
  999 RETURN
      END
