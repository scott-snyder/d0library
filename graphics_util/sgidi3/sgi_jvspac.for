C DEC/CMS REPLACEMENT HISTORY, Element JVSPAC.FOR
C *1     3-JUN-1992 14:56:16 LUPE "Release of DI3GL after split"
C DEC/CMS REPLACEMENT HISTORY, Element JVSPAC.FOR
      SUBROUTINE JVSPAC(X1,X2,Y1,Y2)
C  INSCRIBE THE LARGEST POSSIBLE CENTERED RECTANGLE WITHIN THE
C  SCREEN WINDOW
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      IF(IDEBUG.GT.0) TYPE *,' JVSPAC:',X1,X2,Y1,Y2
      if(idebwr.gt.0) then
        write(idebwr,8008) x1,x2,y1,y2
 8008   format(' JVSPAC - x1,x2,y1,y2:',4f)
      endif
      VCXMN=X1
      VCXMX=X2
      VCYMN=Y1
      VCYMX=Y2
C  ADJUST VIEWSPACE ASPECT RATIO
      HVS=VCYMX-VCYMN
      WVS=VCXMX-VCXMN
      VASP=HVS/WVS
      IF(RASP.EQ.0) CALL J_SETRASP
C!!!KILL THIS UNTIL JDEVWN IS WORKING
C      IF(VASP.LE.RASP) THEN
C        XMARG=0.
C        YMARG=(RASP-VASP)/2.
C      ELSE
C        YMARG=0.
C        XMARG=(VASP-RASP)/2.
C      ENDIF
      IFSPAC=1
C  SET VIEWPORT TO FULL BY DEFAULT
      CALL JVPORT(X1,X2,Y1,Y2)
      END
