      SUBROUTINE INTERP(X,Y,N,X0,Y0)
C-------------------------------------------------------------------------
C                                                                         -
C- GIVEN A SET OF 'N' POINTS DEFINED BY THEIR COORDINATES "X(I)" AND "Y(  -
C- COMPUTE ,BY LINEAR INTERPOLATION, THE ORDINATE "Y0" WHEN THE ABSCISSA  -
C- "X0" IS GIVEN                                                          -
C-                                                                        -
C-                                                                        -
C--------------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER N,I
      REAL XREF,X0,Y0,XINF,XSUP
      REAL X(N),Y(N)
C
      XREF=X0
      IF((X0-X(1))*(X0-X(N)).GT.0.)THEN
C  X0 OUTSIDE THE BOUNDS
            XINF=AMIN1(X(1),X(N))
            XSUP=X(1)+X(N)-XINF
            XREF=AMIN1(X0,XSUP)
            XREF=AMAX1(XREF,XINF)
      END IF
      DO 50 I=1,N-1
      IF((XREF-X(I))*(XREF-X(I+1)).LE.0.)GO TO 60
  50  CONTINUE
  60  CONTINUE
      IF(X(I+1).NE.X(I))THEN
            Y0=Y(I)+(XREF-X(I))*(Y(I+1)-Y(I))/(X(I+1)-X(I))
      ELSE
            Y0=.5*(Y(I+1)+Y(I))
      END IF
      RETURN
      END
