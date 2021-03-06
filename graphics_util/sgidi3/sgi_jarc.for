C DEC/CMS REPLACEMENT HISTORY, Element JARC.FOR
C *1     3-JUN-1992 13:07:11 LUPE "Release of DI3GL after split"
C DEC/CMS REPLACEMENT HISTORY, Element JARC.FOR
      SUBROUTINE JARC(X0,Y0,Z0,R,NSEG,A0,A1)
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      REAL VV(3)
      IF(NSEG.GE.1)THEN
        NS=NSEG
      ELSE
        NS=100
      ENDIF
      IF(IDEBUG.GT.0) TYPE *,' JARC-X0,Y0,Z0,R,NS,A0,A1:',
     &                            X0,Y0,Z0,R,NS,A0,A1
      DPHI=(3.14159/180.)*(A1-A0)/FLOAT(NS)
      PHI=(3.14159/180.)*A0
      CSD=COS(DPHI)
      SND=SIN(DPHI)
      CS=COS(PHI)
      SN=SIN(PHI)
      IF(HCPY) THEN
        CALL DEV_MOVE(CS*R+X0,SN*R+Y0,Z0)
      ELSE
        CALL J3MOVE(CS*R+X0,SN*R+Y0,Z0)
      ENDIF
      DO I=2,NS+1
        CSPR=CS*CSD-SN*SND
        SNPR=SN*CSD+CS*SND
        IF(HCPY) THEN
          CALL DEV_MOVE(CSPR*R+X0,SNPR*R+Y0,Z0)
        ELSE
          CALL J3MOVE(CSPR*R+X0,SNPR*R+Y0,Z0)
        ENDIF
        CS=CSPR
        SN=SNPR
      ENDDO
      END
