      SUBROUTINE TOP_FIT_CALCULATE_WEIGHTS(TFMASS,TOP1,TOP2,
     &  RWT,DWT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : After the 2C fit is done, this routine calculates
C-   the RR/DG weights for the solution.
C-
C-   Inputs  : TFMASS = FITTED TOP MASS
C-             TOP1(4),TOP2(4) = FITTED 4 VECTORS 
C-   Outputs : RWT = 1/SIGMA DSIGMA/DLIPS
C-             DWT = DALITZ_GOLDSTEIN WEIGHT
C-   Controls: 
C-
C-   Created  21-MAR-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TOP_SOLNSE.INC'
      DOUBLE PRECISION TFMASS,TOP1(*),TOP2(*),RWT,DWT
      REAL    TMASS_LOS,TMASS_HIS,DELMASS_S,WMASS_S
      INTEGER IER
      LOGICAL first
      SAVE first
      DATA first / .true. /
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL INRCP('TOP_MASS_RCP',IER)
        CALL EZPICK('TOP_MASS_RCP')
        CALL EZGET('TMASS_LO',TMASS_LOS,IER)
        CALL EZGET('TMASS_HI',TMASS_HIS,IER)
        CALL EZGET('DELMASS',DELMASS_S,IER)
C
        CALL UCOPYSD(TMASS_LOS,TMASS_LO,1)
        CALL UCOPYSD(TMASS_HIS,TMASS_HI,1)
        CALL UCOPYSD(DELMASS_S,DELMASS,1)
        CALL EZRSET
C
        CALL CALCULATE_CROSS_SECTION
        NCNFGE = 1  !Only one configuration
      ENDIF
      NTOPS = 1  !ONLY 1 SOLUTION
      NSOLS(NTOPS) = 1
      TOP_MASS(NTOPS) = TFMASS
      CALL UCOPYDD(TOP1,T1(1,1,NTOPS),4)
      CALL UCOPYDD(TOP2,T2(1,1,NTOPS),4)
C
      CALL CALCULATE_WEIGHTS
      RWT = RWEIGHT(1,1)
      DWT = WEIGHTD(1,1)
C
  999 RETURN
      END
