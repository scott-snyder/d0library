      SUBROUTINE MAKE_DEBUG_PLOTS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : MAKE DEBUG PLOTS OF CROSS SECTION,
C-   CONFIGURATIONS
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   8-JUN-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:KINEQ.INC'
      INCLUDE 'D0$INC:TOP_SOLNS.INC'
      INCLUDE 'D0$INC:TOP_SOLNSE.INC'
      INTEGER IER
      INTEGER NDSMEAR
      DOUBLE PRECISION   PN,PSUM(5)
      INTEGER I
      REAL    BUFFER(200)
      REAL    MPAIR
C
      LOGICAL MAKE_DEBUG
      LOGICAL first
      SAVE first
      DATA first / .true. /
C----------------------------------------------------------------------

      IF( first ) THEN
        first = .false.
        CALL EZPICK('TOP_MASS_RCP')
        CALL EZGET_l('MAKE_DEBUG_PLOTS',MAKE_DEBUG,IER)
        CALL DO_HBOOK('DEBUG_HISTS')
        CALL EZGET_i('NUMBER_DEBUG_SMEARS',NDSMEAR,IER)
        CALL EZRSET
      ENDIF
C
      IF(.NOT.MAKE_DEBUG)RETURN
C
      DO I = 1 , NDSMEAR
        PN = DSQRT(PNUT(1)**2+PNUT(2)**2)
        BUFFER(1) = LEPTON1(4)
        BUFFER(2) = LEPTON2(4)
        BUFFER(3) = PN
        BUFFER(4) = JET1(4)
        BUFFER(5) = JET2(4)
        BUFFER(6) = JET3(4)
C
        CALL DO_HFN('DILEPTON',500,BUFFER)
        CALL GENERATE_CONFIG
C
        CALL PAIR_MASSD(LEPTON1,LEPTON2,PSUM)
        MPAIR = PSUM(5)
        CALL DO_HF1(501,MPAIR,1.0)
      ENDDO
C
      MAKE_DEBUG=.FALSE.  !ONLY ONCE
C
  999 RETURN
      END
