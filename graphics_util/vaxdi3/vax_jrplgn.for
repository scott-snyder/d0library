      SUBROUTINE JRPLGN(X,Y,N)
      INCLUDE 'D0$INC:DI3INC.INC'
      INTEGER*4 ATBTEM
      REAL X(*),Y(*)
      LOGICAL PUTSV
      IF(N.LE.0.OR.N.GT.1999)RETURN
      PUTSV=PUTS
      PUTS=.FALSE.
      XSAVE=XPOSN
      YSAVE=YPOSN
      NN=N+1
      DO 500 I=1,N
        IF(I.EQ.1) THEN
            XI=XPOSN+X(I)
            YI=YPOSN+Y(I)
            CALL J_TR2XY(XI,YI,XPOLY(NN),YPOLY(NN))
           ELSE
            XI=XI+X(I)
            YI=YI+Y(I)
        ENDIF
        CALL J_TR2XY(XI,YI,XPOLY(I),YPOLY(I))
      IF(IPICID.NE.0) CALL J_PIKDO(XI,YI,0.)
  500 CONTINUE
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
      CALL J_PUTSGN_i(IRPLGN,1,N)
      CALL J_PUTSGN(IRPLGN,N,X)
      CALL J_PUTSGN(-IRPLGN,N,Y)
 1000 RETURN
      END
