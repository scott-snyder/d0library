      SUBROUTINE J3PLGN(X,Y,Z,N)
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      REAL X(*),Y(*),Z(*)
      IF(IDEBUG.GT.10) TYPE *,' J3PLGN-N,',N
      IF(N.LE.0.OR.N.GT.256) RETURN
      DO I=1,N
        PARRAY(1,I)=X(I)
        PARRAY(2,I)=Y(I)
        PARRAY(3,I)=Z(I)
      ENDDO
      IF(IPSTYL.EQ.0) THEN
        CALL JCOLOR(ICOLOR)
        GO TO 555
      ENDIF
      CALL JCOLOR(IPCOLO)
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
  510 VPOSN(1)=X(1)
      VPOSN(2)=Y(1)
      VPOSN(3)=Z(1)
C  RESTORE COLOR
      CALL JCOLOR(ICOLOR)
      END
