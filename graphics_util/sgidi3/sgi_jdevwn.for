C DEC/CMS REPLACEMENT HISTORY, Element JDEVWN.FOR
C *1     3-JUN-1992 13:41:11 LUPE "Release of DI3GL after split"
C DEC/CMS REPLACEMENT HISTORY, Element JDEVWN.FOR
      SUBROUTINE JDEVWN(IDEV,X1,X2,Y1,Y2)
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      IF(IDEBUG.GT.0) TYPE *,' JDEVWN:',X1,X2,Y1,Y2
      if(idebwr.gt.0) then
        write(idebwr,8008) idev,x1,x2,y1,y2
 8008   format(' JDEVWN (DISABLED) - idev,x1,x2,y1,y2:',/,i,4f)
      endif
      VCXMN=X1
      VCXMX=X2
      VCYMN=Y1
      VCYMX=Y2
C
C!!! Do nothing for now until calling order tolerance is established
      if(ibomb.eq.0) return
C
C  ADJUST VIEWSPACE ASPECT RATIO
      HVS=Y2-Y1
      WVS=X2-X1
      VASP=HVS/WVS
C!!!KILL THIS UNTIL JDEVWN IS WORKING
C      IF(VASP.LE.RASP) THEN
C  VIEWSPACE SHORTER THAN VIEWSCREEN
C        XMARG=0.
C        YMARG=(RASP-VASP)*HEIGHT/2.
C      ELSE
C  VIEWSPACE NARROWER THAN VIEWSCREEN
C        YMARG=0.
C        XMARG=-(RASP-VASP)*WIDTH/2.
C      ENDIF
C      type *,' JVSPAC-RASP,VASP,XMARG,YMARG',rasp,vasp,xmarg,ymarg
      IFSPAC=1
C  SET VIEWPORT TO FULL BY DEFAULT.
      CALL JVPORT(X1,X2,Y1,Y2)
      END
