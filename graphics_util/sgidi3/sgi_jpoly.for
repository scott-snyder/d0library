      SUBROUTINE JPOLY(X,Y,N)
C  PLOT A POLYLINE
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      REAL X(*),Y(*)
      IF(N.GT.256) RETURN
      IF(HCPY) THEN
        DO I=1,N
          PARRAY(1,I)=X(I)
          PARRAY(2,I)=Y(I)
          PARRAY(3,I)=0.
        ENDDO
        CALL DEV_PLIN(N,PARRAY)
      ELSE
        DO I=1,N
          CALL DRAW2(X(I),Y(I))
        ENDDO
      ENDIF
      VPOSN(1)=X(N)
      VPOSN(2)=Y(N)
      END
