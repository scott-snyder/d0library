      DOUBLE PRECISION FUNCTION DSIGMA_DT_GLUON(T)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate the differential cross section
C-   dsigma/dthat gg to t tbar
C-
C-   Inputs  : T = T-HAT, SH = S-HAT TMASS=TOP quark mass
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
      ASQ = 6.0*(T-MSQ)*(U-MSQ)/(SH*SH)
C
C
      BSQ = (4.*(U-MSQ))/(3.*(T-MSQ))
      BSQ = BSQ - 8.*MSQ*(T+MSQ)/(3.*(T-MSQ)**2)
      BSQ = BSQ + 3.*((T-MSQ)*(U-MSQ)+MSQ*(U-T))
     &  /(SH*(T-MSQ))
C
      CSQ = (4.*(T-MSQ))/(3.*(U-MSQ))
      CSQ = CSQ - 8.*MSQ*(U+MSQ)/(3.*(U-MSQ)**2)
      CSQ = CSQ + 3.*((U-MSQ)*(T-MSQ)+MSQ*(T-U))
     &  /(SH*(U-MSQ))
C
      DSQ = -MSQ*(SH-4.*MSQ)/(3.*(T-MSQ)*(U-MSQ))
C
      ASQ = ASQ + BSQ + CSQ + DSQ
C
      DSIGMA_DT_GLUON = 0.125*CONV_PB*ASQ*PI*(ALPHA_S/SH)**2  !in pb/GeV
C
  999 RETURN
      END
