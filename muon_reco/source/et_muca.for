      SUBROUTINE ET_MUCA(LMUON,ET)
C----------------------------------------------------------------------
C-   Purpose: Calculates the direct hit calorimeter cells Et for a muon.
C-
C-   Author:  Gene Álvarez
C-   Created: 4 Oct 1993
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'

      INTEGER  LMUON,LMUCA,NCELLS,CELL,IERR,J,ETA,PHI,LYR
      REAL     ET,E,THETA,CENETA,DELETA
C----------------------------------------------------------------------

      ET = -1.

      LMUCA = LQ(LMUON - 2)
      IF (LMUCA .GT. 0) THEN
        NCELLS = IQ(LMUCA + 3)
        IF (NCELLS .GT. 0) THEN
          ET = 0.
          DO 1 J = 1, NCELLS
            CELL = LMUCA + 3 + 5*(J - 1)
            ETA  = IQ(CELL + 1)
            PHI  = IQ(CELL + 2)
            LYR  = IQ(CELL + 3)
            CALL CALETA(ETA,CENETA,DELETA,IERR)
            IF (IERR .EQ. 0) THEN
              THETA = 2.*ATAN(EXP(-CENETA))
            ELSE
              THETA = PI/2.
            ENDIF
            E  = Q(CELL + 4)
            ET = ET + E*SIN(THETA)
    1     CONTINUE
        ENDIF
      ENDIF

      RETURN
      END
