      SUBROUTINE NEURAL_CUT_NTUPLE(XOUT,IEVENT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : XOUT   [F]   Network output
C-             IEVENT [I]   Pattern (event)
C-   Outputs : 
C-   Controls: 
C-
C-   Created  10-MAR-1995   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    XOUT
      INTEGER IEVENT
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:JETNET.INC'
C----------------------------------------------------------------------
      CALL HCDIR('//PAWC',' ')
      CALL HCDIR(HDIR_SAVE,' ')
      PATTERN_IN(1,IEVENT) = XOUT
      CALL HFN(IDNEXT,PATTERN_IN(1,IEVENT))
  999 RETURN
      END
