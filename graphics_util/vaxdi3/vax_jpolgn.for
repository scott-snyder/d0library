      SUBROUTINE JPOLGN(X,Y,N)
      INCLUDE 'D0$INC:DI3INC.INC'
      REAL X(2),Y(2)
      INTEGER*4 ATBTEM
      LOGICAL PUTSV
      IF(N.LE.0.OR.N.GT.1999)RETURN
      PUTSV=PUTS
      PUTS=.FALSE.
      XSAVE=XPOSN
      YSAVE=YPOSN
      NN=N+1
      XTEM=X(1)
      YTEM=Y(1)
      DO 500 I=1,N
        CALL J_TR2XY(X(I),Y(I),XPOLY(I),YPOLY(I))
  500 CONTINUE
      CALL J_TR2XY(XTEM,YTEM,XPOLY(NN),YPOLY(NN))
      IF(IPSTYL.EQ.0)GO TO 630
      CALL JCOLOR(IPCOLO)
      CALL J_PLOT_ARRAY(VD_ID,ATB_PLY,NN,XPOLY,YPOLY)
      IF(IPEDGE.GT.0) GO TO 620
      CALL JCOLOR(0)
  630 CALL J_PLOT_ARRAY(VD_ID,ATB,NN,XPOLY,YPOLY)
  620 XPOSN=XSAVE
      YPOSN=YSAVE
      CALL JCOLOR(ICOLOR)
      IF(.NOT.PUTSV)RETURN
      PUTS=.TRUE.
      CALL J_PUTSGN_i(I2PLGN,1,N)
      CALL J_PUTSGN(I2PLGN,N,X)
      CALL J_PUTSGN(-I2PLGN,N,Y)
      IF(IPICID.EQ.0)RETURN
      DO 3000 II=1,N
        CALL J_PIKDO(X(II),Y(II),0.)
 3000 CONTINUE
 1000 RETURN
      END
