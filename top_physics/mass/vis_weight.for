      REAL FUNCTION VIS_WEIGHT(MASS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Routine gives the efficiency of finding
C-   the top as a function of top mass.
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  10-NOV-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    MASS,DIVDIF
C
      INTEGER NMASS,IER
C
      LOGICAL first
      SAVE first
      DATA first / .true. /
      REAL    EMU_EFF(10),TM_EFF(10)
      SAVE EMU_EFF,TM_EFF
      INTEGER MPOL
C
C----------------------------------------------------------------------
      IF( first ) THEN
        FIRST = .FALSE.
        CALL EZPICK('TOP_MASS_RCP')
        CALL EZGETA('TOP_MASS_EFF',0,0,0,NMASS,IER)
        CALL EZGET('TOP_MASS_EFF',TM_EFF,IER)
        CALL EZGET('EMU_EFFICIENCIES',EMU_EFF,IER)
        CALL EZGET('POLYNOMIAL_DEGREE',MPOL,IER)
        CALL EZRSET
      ENDIF
C
      VIS_WEIGHT = DIVDIF(EMU_EFF,TM_EFF,NMASS,MASS,MPOL)
C
C
  999 RETURN
      END
