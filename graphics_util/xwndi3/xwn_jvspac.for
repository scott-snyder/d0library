      SUBROUTINE JVSPAC(X1,X2,Y1,Y2)
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      VCXMNS=X1
      VCXMXS=X2
      VCYMNS=Y1
      VCYMXS=Y2
C  ADJUST VIEWSPACE ASPECT RATIO
      HVS=Y2-Y1
      WVS=X2-X1
      VASP=HVS/WVS
      IF(VASP.LE.RASP) THEN
C  VIEWSPACE SHORTER THAN VIEWSCREEN
        XMARG=0.
        YMARG=(RASP-VASP)*HEIGHT/2.
      ELSE
C  VIEWSPACE NARROWER THAN VIEWSCREEN
        YMARG=0.
        XMARG=-(RASP-VASP)*WIDTH/2.
      ENDIF
      VCXMN=X1
      VCYMN=Y1
      VCXMX=X2
      VCYMX=Y2
      IFSPAC=1
C  SET VIEWPORT TO FULL BY DEFAULT.
      CALL JVPORT(X1,X2,Y1,Y2)
      END
