      SUBROUTINE DET_ETA(Z,TH,ETA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Given Z of vertex and physics theta return detector eta
C-
C-   Inputs  : 
C-   Z  = z of vertex
C-   TH = physics theta
C-   Output :
C-   ETA = detector eta (eta obtained with z=0)
C-
C-   Created   7-FEB-1992   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      REAL Z,TH,ETA,THD,CAL_TH
      THD=CAL_TH(TH,Z)
      ETA=-ALOG(TAN(THD/2.))
      RETURN
      END
