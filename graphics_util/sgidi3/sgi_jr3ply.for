      SUBROUTINE JR3PLY(X,Y,Z,N)
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      REAL X(*),Y(*),Z(*)
      IF(N.GT.256) RETURN
      DO I=1,N
        VPOSN(1)=VPOSN(1)+X(I)
        VPOSN(2)=VPOSN(2)+Y(I)
        VPOSN(3)=VPOSN(3)+Z(I)
        PARRAY(1,I)=VPOSN(1)
        PARRAY(2,I)=VPOSN(2)
        PARRAY(3,I)=VPOSN(3)
        IF(.NOT.HCPY) THEN
          IF(I.EQ.1) THEN
            CALL MOVE(VPOSN(1),VPOSN(2),VPOSN(3))
          ELSE
            CALL DRAW(VPOSN(1),VPOSN(2),VPOSN(3))
          ENDIF
        ENDIF
      ENDDO
      IF(HCPY) THEN
        CALL DEV_PLIN(N,PARRAY)
      ENDIF
      END
