      SUBROUTINE JRPOLY(X,Y,N)
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      REAL X(*),Y(*)
      IF(N.GT.1999) RETURN
      IF(HCPY) THEN
        PARRAY(1,1)=X(1)
        PARRAY(2,1)=Y(1)
        PARRAY(3,1)=0.
        DO I=2,N
          PARRAY(1,I)=PARRAY(1,I)+X(I)
          PARRAY(2,I)=PARRAY(2,I)+Y(I)
          PARRAY(3,I)=0.
        ENDDO
        CALL DEV_PLIN(N,PARRAY)
        GO TO 10
      ENDIF
      CALL J_TR2XY(XPOSN,YPOSN,XPOLY(1),YPOLY(1))
      DO I=1,N
        IF(I.EQ.1) THEN
          XI=XPOSN+X(I)
          YI=YPOSN+Y(I)
          CALL J_TR2XY(XI,YI,XPOLY(2),YPOLY(2))
        ELSE
          XI=XI+X(I)
          YI=YI+Y(I)
          CALL J_TR2XY(XI,YI,XPOLY(I+1),YPOLY(I+1))
        ENDIF
      ENDDO
      NN=N+1
      IATB=ILINE
      CALL J_PLOT_ARRAY(NN,XPOLY,YPOLY)
   10 XPOSN=XI
      YPOSN=YI
      IF(.NOT.PUTS)RETURN
      CALL J_PUTSGN(IRPOLY,1,N)
      CALL J_PUTSGN(IRPOLY,N,X)
      CALL J_PUTSGN(-IRPOLY,N,Y)
      END
