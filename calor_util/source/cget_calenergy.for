      SUBROUTINE CGET_CALENERGY(TOTAL_ENERGY,EECN,EECC,EECS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Crude method for getting the Calorimeter 
C-                         total energy and energy by region for 
C-                         the Multiple Interaction tool.
C-
C-   Inputs  : none
C-   Outputs : TOTAL_ENERGY - total energy in whole calorimeter
C-             EECN - total energy in EC North defined by IETA < -12
C-             EECC - total energy in CC defined by |IETA| < 12
C-             EECS - total energy in EC South defined by IETA > 12
C-   Controls: none
C-
C-   Created  15-JUL-1992   Jeffrey Bantly
C-   Updated   9-JUL-1993   Jeffrey Bantly  name changed from L0_GET_CALENERGY 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER NV,NR,NCH
      INTEGER IETA,IPHI,ILYR,BITS,ICHAN
      INTEGER IER,I
      REAL    TOTAL_ENERGY,ENERGY,ENERGY_TOT
      REAL    EECN,EECC,EECS
      LOGICAL START
C
      SAVE START
C----------------------------------------------------------------------
C
      CALL GTCAEP_HEADER(NV,NR,NCH,IER)
C
      START = .TRUE.
      ENERGY_TOT = 0.0
      EECN = 0.0
      EECC = 0.0
      EECS = 0.0
      DO I = 1, NCH
        CALL GTCAEP(START,IETA,IPHI,ILYR,BITS,ENERGY,ICHAN,IER)
        START = .FALSE.
        IF ( ENERGY.GE.0.0 ) THEN
          ENERGY_TOT = ENERGY_TOT + ENERGY
          IF ( ABS(IETA).LE.12 ) THEN
            EECC = EECC + ENERGY
          ELSEIF ( IETA.GT.12 ) THEN
            EECS = EECS + ENERGY
          ELSEIF ( IETA.LT.-12 ) THEN
            EECN = EECN + ENERGY
          ENDIF
        ENDIF
      ENDDO
C
      TOTAL_ENERGY = ENERGY_TOT
C----------------------------------------------------------------------
  999 RETURN
      END
