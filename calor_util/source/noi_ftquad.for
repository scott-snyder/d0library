      FUNCTION NOI_FTQUAD(YA,YB,YC,TA,TB,TC,T_VAL,XPK,UPK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-                 FITS A QUADRATIC FUNCTION TO 3 POINTS (Y,T)
C-                 THEN INTERPOLATES Y VALUE FOR T= T_VAL
C-   Inputs  :  3 points (YA,TA), (YB,TB), (YC,TC)
C-              T_VAL = T at which fit function value is
C-                        desired
C-   Outputs :  NOI_FTQUAD = fit function value at T_VAL
C-   Controls: None
C-
C-   Created   17-SEP-1991   Peter Nemethy and Allen I. Mincer
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL NOI_FTQUAD
      REAL AA,BB
      REAL YA,YB,YC,TA,TB,TC,T_VAL,UB,UC,XB,XC,X_VAL
      REAL XPK,UPK
C
C#######EXECUTIONN BEGINS#########
C
      XB=TB-TA
      XC=TC-TA
      UB=YB-YA
      UC=YC-YA
      X_VAL=T_VAL-TA
      BB=(UB/XB**2-UC/XC**2)/(1/XB-1/XC)
      AA=UB/XB**2-BB/XB
      XPK=-999.9
      UPK=0.0
      IF(AA.NE.0.0)THEN
        XPK=-0.5*BB/AA
        UPK=AA*XPK**2+BB*XPK
      ENDIF
      NOI_FTQUAD=AA*X_VAL**2+BB*X_VAL
      RETURN
      END
