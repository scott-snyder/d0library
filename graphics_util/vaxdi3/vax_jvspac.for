      SUBROUTINE JVSPAC(X1,X2,Y1,Y2)
      REAL X1,X2,Y1,Y2
      INCLUDE 'D0$INC:DI3INC.INC'
      VCXMN=X1
      VCXMX=X2
      VCYMN=Y1
      VCYMX=Y2
C  ADJUST VIEWSPACE ASPECT RATIO
      HVS=VCYMX-VCYMN
      WVS=VCXMX-VCXMN
      VASP=HVS/WVS
      IF(VASP.LE.RASP) THEN
C  VIEWSPACE SHORTER THAN VIEWSCREEN
        XMARG=0.
C        YMARG=(RASP-VASP)*HEIGHT/(2.*RASP)
        YMARG=(RASP-VASP)*HEIGHT/2.
      ELSE
C  VIEWSPACE NARROWER THAN VIEWSCREEN
        YMARG=0.
C        XMARG=-(RASP-VASP)*WIDTH/(2.*RASP)
        XMARG=-(RASP-VASP)*WIDTH/2.
      ENDIF
      IFSPAC=1
C  SET VIEWPORT TO FULL BY DEFAULT????
      CALL JVPORT(X1,X2,Y1,Y2)
      END