      SUBROUTINE JCIRCL(X0,Y0,Z0,R,NSIDS)
C-------------------------------------------------------------------
C-
C-   Pourpose and Methods: Draws a circle using GL 
C-
C-   Inputs: X0   [R]: x coodinate for the center of the circle 
C-           Y0   [R]: y coordenate for the center of the circle
C-           Z0   [R]: z coordenate for the center of the circle
C-           R    [R]: Radios of the circle
C-           NSIDS[I]: 
C-
C-   Outputs: None
C-  
C-   Created   04-JUN-1992   Mike Shupe
C-   Updated   03-DEC-1992   Lupe Howell Clean up
C-
C-------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
C
      INTEGER NSIDS,NS,I
      REAL    X0,Y0,Z0,R,DPHI,CSD,SND,CS,SN,CSPR,SNPR
C--------------------------------------------------------------------
      IF(NSIDS.GE.3)THEN
        NS=NSIDS
      ELSE
        NS=100
      ENDIF
      IF(IDEBUG.GT.0) TYPE *,' JCIRCL-X0,Y0,Z0,R,NS:',
     &              X0,Y0,Z0,R,NS
      DPHI=2.*3.14159/FLOAT(NS)
      CSD=COS(DPHI)
      SND=SIN(DPHI)
      CS=1.
      SN=0.
      PARRAY(1,1)=CS*R+X0
      PARRAY(2,1)=SN*R+Y0
      PARRAY(3,1)=Z0
      DO I=2,NS
        CSPR=CS*CSD-SN*SND
        SNPR=SN*CSD+CS*SND
        PARRAY(1,I)=CSPR*R+X0
        PARRAY(2,I)=SNPR*R+Y0
        PARRAY(3,I)=Z0
        CS=CSPR
        SN=SNPR
      ENDDO
      IF(IPSTYL.EQ.0) THEN
        CALL JCOLOR(ICOLOR)
        GO TO 555
      ENDIF
      CALL JCOLOR(IPCOLO)
      IF(HCPY) THEN
        CALL DEV_POLF(NS,PARRAY)
      ELSE
C        CALL POLF(NS,PARRAY)
      ENDIF
      IF(IPEDGE.GT.0) GO TO 510
C  GO TO WHITE AND DRAW THE EDGE
      CALL JCOLOR(7)
  555 CONTINUE
      IF(HCPY) THEN
        CALL DEV_POLY(NS,PARRAY)
      ELSE
C        CALL POLY(NS,PARRAY)
C        CALL MOVE(PARRAY(1,1),PARRAY(2,1),PARRAY(3,1))
C        DO I=2,NS
C          CALL DRAW(PARRAY(1,I),PARRAY(2,I),PARRAY(3,I))
C        ENDDO
        CALL POLYN(NS,PARRAY)
        CALL DRAW(PARRAY(1,1),PARRAY(2,1),PARRAY(3,1))
      ENDIF
  510 CONTINUE
C  RESTORE COLOR
      CALL JCOLOR(ICOLOR)
      END
