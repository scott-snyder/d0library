      FUNCTION ATTB(X)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   5-NOV-1993   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      REAL X,ATTB
      REAL XF(16),YF(17),DY,DX
      INTEGER I,J
      DATA XF/0.01,0.03,0.05,0.07,0.09,0.11,0.13,0.15,0.17,
     1  0.19,0.21,0.23,0.25,0.27,0.29,0.31/  
      DATA YF/25.,43.,50.,44.,38.,32.,26.,20.,14.,10.,8.,
     1  6.,5.,4.,3.,2.,1./
C----------------------------------------------------------------------
      J=1
      DO 1 I=2,16
        IF(X.GT.XF(I)) J=I
    1 CONTINUE
      ATTB=YF(J)+(YF(J)-YF(J+1))*(XF(J)-X)/.02
      ATTB=.0125*ATTB
  999 RETURN
      END
