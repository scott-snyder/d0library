      REAL FUNCTION JET_CORR_CC(ET,ETA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns corrected jet energy based on test
C-                         beam responses. For jets in the CC.
C-                         See D0note #1595 for details.
C-
C-   Inputs  : ET     -  uncorrected jet Et
C-             ETA    -  jet eta
C-   Outputs : JETSCALE - corrected jet Et
C-   Controls:
C-
C-   Created  24-SEP-1992   Andrew J. Milder
C-   Modified 20-MAR-1992   Rich Astur " Add corrections for out-of-cone,
C-                            underlying event and zero suppression"
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL ET,A(3),ETA,THETA,ENERGY,FACTOR
      LOGICAL FIRST
      DATA A / -2.109,  0.927,  0.0000796 /
C- Em scale
      REAL EMSCALE_OFFSET         ! Correction for 6% down shift in emscale
      PARAMETER( EMSCALE_OFFSET = .06 ) ! From Z mass reconstruction
C- Underlying event
      REAL EPERSTER               ! CDF measures 1.2 GeV Et per unit area
      PARAMETER( EPERSTER = 1.2 ) ! in eta-phi. See 1992 inc. jet. xsection
C                                 ! paper.
C- Out of cone correction (multiplicative) corr = 1.028 + .639/E
      REAL CONE_CORR(2)           ! From testbeam particle library R=.7
      DATA CONE_CORR/ .639, 1.028/ ! Fit from thesis of A. Milder
C Fit for zsp loss above 50 GeV is .98 + .032*E - .0000817*E**2 (from EC)
C Fit for zsp loss below 50 GeV is -1.9+ .145*E - .00142*E**2   (from CC)
C diff between fits is 1% at overlap point (50 GeV)
      REAL ZSP_LOSS_E(3,2)
      INTEGER ICORR
      DATA ZSP_LOSS_E / .98,  .032,  -.0000817 ,
     &                 -1.9,  .145,  -.00142 /
C----------------------------------------------------------------------
C
C  QUADRATIC fit used
C  Version 1.0 includes:
C                 i) Pushpa Bhat's latest low-e single particle
C                    responses as of 11/10/92.
C                ii) EM crack response ( negligible effect.)
C               iii) FH crack response ( ~1% effect.)
C  Version 2.0 includes:
C                 i) Out-of-cone corrections from testbeam pion library
C                ii) Underlying event estimate from CDF 1992 jet papers
C               iii) Zero suppression effects from real data
C                iv) Correction for EM scale
C
C  Since version 1.0 corrections come from PYTHIA jet fragmentation,
C  we will make some of the version 2.0 corrections FIRST and then apply
C  version 1. We will do so in this order: Add energy for zsp, divide
C  by out of cone fraction lost, then apply version 1 and em scale to 
C  get real energy and then subtract underlying event.
C
C----------------------------------------------------------------------
      IF (ABS(ETA).GT.1.5) THEN
        CALL ERRMSG('ETA TOO BIG','JET_CORR_CC',
     &    'USE THIS ROUTINE ONLY FOR JETS IN THE CC','W')
      ENDIF
C
C Must correct jet energy, not Et
C
      THETA = 2.*ATAN(EXP(-ETA))
      ENERGY = ET/SIN(THETA)
C
C Correct energy for zsp
C
      IF ( ENERGY .LE. 50. ) THEN         ! Which fit do we use?
        ICORR = 2
      ELSE
        ICORR = 1
      ENDIF
      ENERGY = ENERGY + ZSP_LOSS_E(1,ICORR) + ENERGY*ZSP_LOSS_E(2,ICORR)
     &  + (ENERGY**2)*ZSP_LOSS_E(3,ICORR)
C
C: Divide energy by the fraction that left the cone
C
      ENERGY = CONE_CORR(1) + ENERGY*CONE_CORR(2)
      
C
      FACTOR = -A(2)+SQRT(A(2)**2+4.*A(3)*(ENERGY-A(1)))
      FACTOR = FACTOR/(2.*ENERGY*A(3))
      IF (FACTOR.LT.1.) FACTOR = 1.
      ENERGY = ENERGY*FACTOR
C
C: Correct for EM scale
C
      ENERGY = ENERGY*(1. + EMSCALE_OFFSET )
C      
C: Finally, subtract out the underlying event (assume R=.7)
C
      ENERGY = ENERGY - EPERSTER*3.14*(.7**2)

      JET_CORR_CC = ENERGY*SIN(THETA)       ! Return Et, not E
  999 RETURN
      END
