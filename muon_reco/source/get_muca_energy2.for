      SUBROUTINE GET_MUCA_ENERGY2(LPMUO,MU_EM,MU_FH,MU_CH)
C----------------------------------------------------------------------
C-   Purpose: Sum the energy in the cells of the MUCA bank, in the EM,
C-            FH, and CH regions.
C-   
C-   Input:  LPMUO 
C-   Output: MU_EM
C-           MU_FH
C-           MU_CH
C-           
C-
C-   Author: K. Wyatt Merritt
C-   Created: 11-Jul-1994
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPMUO.LINK'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER I,J
      INTEGER LPMUO,LMUON,NS
      INTEGER LMUCA,NCELLS,CELL,IERR,ETA,PHI,LYR
C
      REAL MU_EM,MU_FH,MU_CH
      REAL ET,E,THETA,CENETA,DELETA
C----------------------------------------------------------------------

      MU_EM = 0.
      MU_FH = 0.
      MU_CH = 0.

      IF (LPMUO .LE. 0) RETURN
      NS    = IQ(LPMUO - 2)
      LMUON = LQ(LPMUO - NS - 2)
      IF (LMUON .LE. 0) RETURN

      LMUCA = LQ(LMUON - 2)
      IF (LMUCA .GT. 0) THEN
        NCELLS = IQ(LMUCA + 3)
        IF (NCELLS .GT. 0) THEN
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
            ET = E*SIN(THETA)
            IF ( LYR .LE. 7 ) THEN
              MU_EM = MU_EM + E
            ELSE IF (LYR .GT. 10 .AND. LYR .LE. 14) THEN
              MU_FH = MU_FH + E
            ELSE IF (LYR .GT. 14 ) THEN
              MU_CH = MU_CH + E
            ENDIF
    1     CONTINUE
        ENDIF
      ENDIF

  999 RETURN
      END

