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
C-   Updated  11-JUL-1993   Pushpa C. Bhat  Take care of -ve tan value 
C-
C----------------------------------------------------------------------
      REAL Z,TH,ETA,THD,CAL_TH,TANTHD
      THD=CAL_TH(TH,Z)
      CALL HF1(81,THD,1.)
      TANTHD=TAN(THD/2.)
      IF(TANTHD.GT.0.)THEN
        ETA=-ALOG(TANTHD)
      ELSEIF(TANTHD.EQ.0)THEN
        ETA=0.
      ELSE
        ETA=9999.
      ENDIF
      RETURN
      END
