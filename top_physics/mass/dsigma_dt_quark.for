      DOUBLE PRECISION FUNCTION DSIGMA_DT_QUARK(T)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate the differential cross section
C-   dsigma/dthat qqbar to t tbar
C-
C-   Inputs  :  T = T-HAT, SH = S-HAT TMASS=TOP quark mass
C-   Outputs : 
C-   Controls: 
C-
C-   Created   8-FEB-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PARTON_KINE.INC'
      INCLUDE 'D0$INC:PI.DEF'
      DOUBLE PRECISION    ASQ,BSQ,CSQ,DSQ,MSQ
      DOUBLE PRECISION    T,U
C----------------------------------------------------------------------
      MSQ = TMASS*TMASS
      U = 2.0*MSQ - T - SH
      ASQ = 4.0*((T-MSQ)**2 + (U-MSQ)**2 +2.0*MSQ*SH)/(9.0*SH**2)
C
      DSIGMA_DT_QUARK = CONV_PB*ASQ*PI*(ALPHA_S/SH)**2  !In Pb/GeV
C
  999 RETURN
      END
