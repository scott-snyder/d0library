      SUBROUTINE JRPOLY(X,Y,N)
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      REAL X(*),Y(*)
      IF(N.GT.256) RETURN
      DO I=1,N
        VPOSN(1)=VPOSN(1)+X(I)
        VPOSN(2)=VPOSN(2)+Y(I)
        PARRAY(1,I)=VPOSN(1)
        PARRAY(2,I)=VPOSN(2)
        PARRAY(3,I)=0.
        IF(.NOT.HCPY) THEN
          IF(I.EQ.1) THEN
            CALL MOVE2(VPOSN(1),VPOSN(2))
          ELSE
            CALL DRAW2(VPOSN(1),VPOSN(2))
          ENDIF
        ENDIF
      ENDDO
      IF(HCPY) THEN
        CALL DEV_PLIN(N,PARRAY)
      ENDIF
      END
