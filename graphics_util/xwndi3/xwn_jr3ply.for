      SUBROUTINE JR3PLY(X,Y,Z,N)
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      REAL X(*),Y(*),Z(*)
      IF(N.GT.1999) RETURN
      IF(HCPY) THEN
        PARRAY(1,1)=XPOSN
        PARRAY(2,1)=YPOSN
        PARRAY(3,1)=ZPOSN
        DO I=1,N
          PARRAY(1,I)=PARRAY(1,I)+X(I)
          PARRAY(2,I)=PARRAY(2,I)+Y(I)
          PARRAY(3,I)=PARRAY(3,I)+Z(I)
        ENDDO
        CALL DEV_PLIN(N,PARRAY)
        GO TO 10
      ENDIF
      CALL J_TR3XYZ(XPOSN,YPOSN,ZPOSN,XPOLY(1),YPOLY(1),ZPOLY(1))
      DO I=1,N
        IF(I.EQ.1) THEN
          XI=XPOSN+X(I)
          YI=YPOSN+Y(I)
          ZI=ZPOSN+Z(I)
          CALL J_TR3XYZ(XI,YI,ZI,XPOLY(2),YPOLY(2),ZPOLY(2))
        ELSE
          XI=XI+X(I)
          YI=YI+Y(I)
          ZI=ZI+Y(I)
          CALL J_TR3XYZ(XI,YI,ZI,XPOLY(I+1),
     &             YPOLY(I+1),ZPOLY(I+1))
        ENDIF
      ENDDO
      NN=N+1
      IATB=ILINE
      CALL J_PLOT_ARRAY(NN,XPOLY,YPOLY)
   10 XPOSN=XI
      YPOSN=YI
      ZPOSN=ZI
      IF(.NOT.PUTS)RETURN
      CALL J_PUTSGN(IR3PLY,1,N)
      CALL J_PUTSGN(IR3PLY,N,X)
      CALL J_PUTSGN(IR3PLY,N,Y)
      CALL J_PUTSGN(-IR3PLY,N,Z)
      END
