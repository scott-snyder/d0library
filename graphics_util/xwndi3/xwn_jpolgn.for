      SUBROUTINE JPOLGN(X,Y,N)
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      REAL X(*),Y(*)
      LOGICAL PUTSV
      IF(N.LE.0.OR.N.GT.1999)RETURN
      PUTSV=PUTS
      PUTS=.FALSE.
      ICSAVE=ICOLOR
      XSAVE=XPOSN
      YSAVE=YPOSN
      NN=N+1
      XTEM=X(1)
      YTEM=Y(1)
      IF(HCPY) THEN
        DO I=1,N
          PARRAY(1,I)=X(I)
          PARRAY(2,I)=Y(I)
          PARRAY(3,I)=0.
        ENDDO
        IF(IPSTYL.NE.0) THEN
          CALL JCOLOR(IPCOLO)
          CALL DEV_POLF(N,PARRAY)
          IF(IPEDGE.EQ.0) THEN
            CALL JCOLOR(0)
            CALL DEV_POLY(N,PARRAY)
          ENDIF
        ELSE
          CALL DEV_POLY(N,PARRAY)
        ENDIF
        GO TO 620
      ENDIF
      DO I=1,N
        CALL J_TR2XY(X(I),Y(I),XPOLY(I),YPOLY(I))
      ENDDO
      CALL J_TR2XY(XTEM,YTEM,XPOLY(NN),YPOLY(NN))
      IF(IPSTYL.EQ.0)GO TO 630
      CALL JCOLOR(IPCOLO)
      IATB=IPLGN
      CALL J_PLOT_ARRAY(NN,XPOLY,YPOLY)
      IF(IPEDGE.GT.0) GO TO 620
      CALL JCOLOR(0)
  630 IATB=ILINE
      CALL J_PLOT_ARRAY(NN,XPOLY,YPOLY)
  620 XPOSN=XSAVE
      YPOSN=YSAVE
      CALL JCOLOR(ICSAVE)
      IF(.NOT.PUTSV)RETURN
      PUTS=.TRUE.
  800 IF(.NOT.PUTS) RETURN
      CALL J_PUTSGN(I2PLGN,1,N)
      CALL J_PUTSGN(I2PLGN,N,X)
      CALL J_PUTSGN(-I2PLGN,N,Y)
      END
