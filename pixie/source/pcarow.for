      SUBROUTINE PCAROW (X1,Y1,X2,Y2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draws a poligone to make a mark 
C-
C-   Inputs  : X1,Y1   TAIL OF ARROW
C              X2,Y2   HEAD OF ARROW
C-   Outputs :
C-   Controls:
C-
C-   Created   1-SEP-1989   CARY Y. YOSHIKAWA
C-   Updated  19-DEC-1989   Lupe Rosas( Adjusting to use color table )
C-   Modified 18-AUG-1991   Nobu Oshima( Use PXCOLR for E&S's JPOLGN )
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      REAL HEADX(3),HEADY(3),X(4),Y(4),X1,Y1,X2,Y2,L,LL
      REAL THETA,PHI,W,PI
      REAL RLEVEL
C-
      CALL JIQDIL(RLEVEL)
C-
      PI=3.1415927
      L=2.
      W=5.
      THETA=30 *(2*PI/360)
      LL=L*COS(THETA/2)
      PHI=ATAN2(Y2-Y1,X2-X1)
      HEADX(1)=X2
      HEADY(1)=Y2
      HEADX(2)=X2+L*COS(PHI+PI-THETA/2)
      HEADY(2)=Y2+L*SIN(PHI+PI-THETA/2)
      HEADX(3)=X2+L*COS(PHI+PI+THETA/2)
      HEADY(3)=Y2+L*SIN(PHI+PI+THETA/2)
      X(1)=X2+LL*COS(PHI+PI)-W/2*COS(PI/2-PHI)
      Y(1)=Y2+LL*SIN(PHI+PI)+W/2*SIN(PI/2-PHI)
      X(2)=X2+LL*COS(PHI+PI)+W/2*COS(PI/2-PHI)
      Y(2)=Y2+LL*SIN(PHI+PI)-W/2*SIN(PI/2-PHI)
      X(3)=X1+W/2*COS(PI/2-PHI)
      Y(3)=Y1-W/2*SIN(PI/2-PHI)
      X(4)=X1-W/2*COS(PI/2-PHI)
      Y(4)=Y1+W/2*SIN(PI/2-PHI)
      IF(RLEVEL .EQ. -2.) THEN
        CALL PXCOLR('MAG')
      ELSE
        CALL PXCOLFILL('MAG')
      ENDIF
      CALL JPOLGN (HEADX,HEADY,3)
      CALL JPOLGN(X,Y,4)
      END
