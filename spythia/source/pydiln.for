C*********************************************************************
 
      FUNCTION PYDILN(X)
 
C...Dilogarithm function.
      DOUBLE PRECISION PYDILN,X,Y,YP
 
C...Map range x>0.5 to range x<0.5.
      IF(X.LE.0.5D0) THEN
        Y=X
      ELSE
        Y=1D0-X
      ENDIF
 
C...Evaluate series expansion.
      PYDILN=0D0
      YP=1D0
      DO 100 K=1,20
      YP=YP*Y
      IF(YP.LT.1D-30) GOTO 110
      PYDILN=PYDILN+YP/(K*K)
  100 CONTINUE
 
C...Map back for range x>0.5.
  110 IF(X.GT.0.5) PYDILN=1.644934067D0-LOG(X)*LOG(Y)-PYDILN
 
      RETURN
      END
