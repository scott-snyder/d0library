      SUBROUTINE JR3PLY(X,Y,Z,N)
      INCLUDE 'D0$INC:DI3INC.INC'
      REAL X(*),Y(*),Z(*)
      IF(N.GT.1999) GO TO 1000
      CALL J_TR3XYZ(XPOSN,YPOSN,ZPOSN,XPOLY(1),YPOLY(1),ZPOLY(1))
      DO 500 I=1,N
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
      IF(IPICID.NE.0) CALL J_PIKDO(XI,YI,ZI)
  500 CONTINUE
      NN=N+1
      CALL J_PLOT_ARRAY(VD_ID,ATB,NN,XPOLY,YPOLY)
      XPOSN=XI
      YPOSN=YI
      ZPOSN=ZI
      IF(.NOT.PUTS)RETURN
      CALL J_PUTSGN(IR3PLY,1,N)
      CALL J_PUTSGN(IR3PLY,N,X)
      CALL J_PUTSGN(IR3PLY,N,Y)
      CALL J_PUTSGN(-IR3PLY,N,Z)
 1000 RETURN
      END
