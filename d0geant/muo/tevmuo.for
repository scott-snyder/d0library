      LOGICAL FUNCTION TEVMUO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Called before each event by ====> GUTREV
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  23-JUL-1987   A.M.Jonckkhere
C-   Updated   2-JUN-1989   Harrison B. Prosper
C-   Made into a program-builder interface function
C-   Updated   5-AUG-1993   Jasbir Singh, Chip Stewart   - GMUH added
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC'
C----------------------------------------------------------------------
      TEVMUO = .TRUE.
      IF ( DMUO.LT.2 ) GOTO 999
C     -- initialize MSTRAK to store track trajectry...
      CALL MSTRAK('INIT')      
C     -- initialize MCALEXIT if GMUH banks are needed
      IF(SMUO(4).GT.0) THEN   ! INTIALIZE FOR PRIMARY TRACK
        CALL MCALEXIT('INIT')   
        CALL GMUH_RESET_EXIT
      END IF
  999 RETURN
      END
