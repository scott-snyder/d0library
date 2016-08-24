      SUBROUTINE JR3PGN(X,Y,Z,N)
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      REAL X(*),Y(*),Z(*)
      IF(IDEBUG.GT.10) TYPE *,' JR3PGN-N,',N
      IF(N.LE.0.OR.N.GT.256) RETURN
      DO I=1,N
        VPOSN(1)=VPOSN(1)+X(I)
        VPOSN(2)=VPOSN(2)+Y(I)
        VPOSN(3)=VPOSN(3)+Y(I)
        PARRAY(1,I)=VPOSN(1)
        PARRAY(2,I)=VPOSN(2)
        PARRAY(3,I)=VPOSN(3)
      ENDDO
      IF(IPSTYL.EQ.0) THEN
        CALL JCOLOR(ICOLOR)
        GO TO 555
      ENDIF
      CALL JCOLOR(IPCOLO)
      XSAVE=VPOSN(1)
      YSAVE=VPOSN(2)
      ZSAVE=VPOSN(3)
      IF(HCPY) THEN
        CALL DEV_POLF(N,PARRAY)
      ELSE
        CALL POLF(N,PARRAY)
      ENDIF
      IF(IPEDGE.GT.0) GO TO 510
C  GO TO WHITE AND DRAW THE EDGE
      CALL JCOLOR(7)
  555 IF(HCPY) THEN
        CALL DEV_POLY(N,PARRAY)
      ELSE
        CALL POLY(N,PARRAY)
      ENDIF
  510 VPOSN(1)=XSAVE
      VPOSN(2)=YSAVE
      VPOSN(3)=ZSAVE
C  RESTORE COLOR
      CALL JCOLOR(ICOLOR)
      END