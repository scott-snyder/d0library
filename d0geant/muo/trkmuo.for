      LOGICAL FUNCTION TRKMUO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : MUON - Handle any procedures needed after a
C-   full track is finished - DUMMY for now.
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-
C-   Created  13-JUL-1987   A.M.Jonckkhere
C-   Updated   2-JUN-1989   Harrison B. Prosper
C-   Made into a program-builder interface function
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC'
C----------------------------------------------------------------------
      TRKMUO = .TRUE.
      IF ( DMUO.LT.2 ) GOTO 999
C
C     -- store muon track trajectry in MUON bank...
C
      CALL MSTRAK('FILL')
      CALL MSTRAK('INIT')
      IF(SMUO(4).GT.0)CALL MCALEXIT('INIT')
  999 RETURN
      END
