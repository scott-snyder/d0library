      SUBROUTINE PHI_TRD(POINT,PHIT,PHI,CELL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Returned value  :
C-   Inputs  :     POINT    real(3)    x,y,z of a point on the track
C-                 PHIT     real       PHI [0,2*pi] of the track
C-   Outputs :     PHI      real(3)    PHI [0,2*pi] of the line i
C-                                     (i=1,2,3 for the 3 TRD layers)
C-                                     connecting the intersection of the track
C-                                     with the layer i and the center of D0
C-                 CELL     integer(3) geometrically hit cell number [1,256]
C-                                     for layers 1,2,3
C-
C-   Controls: none
C-
C-   Created   1-JUN-1993   Alain PLUQUET
C-   Updated  15-OCT-1993   Alain PLUQUET  adds 512 wire case
C-   Updated  25-NOV-1993   Alain PLUQUET  NUMBER_OF_WIRES(3)-->(6)
C-   Updated  17-MAR-1994   A. Zylberstejn  simplify 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL POINT(3),PHIT,PHI(3),K,K1,K2,R(3),X,Y,X1,Y1,X2,Y2,D1,D2
      REAL DELTA,XT,YT,CPHI,SPHI
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER LAYER,CELL(3),NUMBER_OF_WIRES(6)
      DATA R/26.3,36.85,47.4/
C----------------------------------------------------------------------
      XT=POINT(1)
      YT=POINT(2)
      CPHI=COS(PHIT)
      SPHI=SIN(PHIT)
      DO LAYER=1,3
        DELTA=
     &    R(LAYER)**2-(XT*SPHI-YT*CPHI)**2
        IF (DELTA.GT.0.) THEN
          K1=-(XT*CPHI+YT*SPHI)+SQRT(DELTA)
          X1=XT+K1*CPHI
          Y1=YT+K1*SPHI
          K2=-(XT*CPHI+YT*SPHI)-SQRT(DELTA)
          X2=XT+K2*CPHI
          Y2=YT+K2*SPHI
          D1=(XT-X1)**2+(YT-Y1)**2
          D2=(XT-X2)**2+(YT-Y2)**2
          IF (D1.LT.D2) THEN
            PHI(LAYER)=ATAN2(Y1,X1)
          ELSE
            PHI(LAYER)=ATAN2(Y2,X2)
          ENDIF
        ELSEIF (DELTA.EQ.0.) THEN
          K=-(XT*CPHI+YT*SPHI)
          X=XT+K*CPHI
          Y=YT+K*SPHI
          PHI(LAYER)=ATAN2(Y,X)
        ELSE
          PHI(LAYER)=999.
        ENDIF
        IF (PHI(LAYER).LT.0.) PHI(LAYER)=PHI(LAYER)+TWOPI
        CALL GET_TRD_NBW(NUMBER_OF_WIRES)
        CELL(LAYER)=INT(PHI(LAYER)/(TWOPI/NUMBER_OF_WIRES(LAYER)))+1
      ENDDO
      END
