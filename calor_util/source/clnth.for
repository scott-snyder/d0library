      SUBROUTINE CLNTH(VTX,DIR,ZCN,THCN,NS,S)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find intersections of a line with a cone of
C-                         given opening angle and vertex z-position
C-
C-   Inputs  : VTX  A point on the line
C-             DIR  Direction cosines of the line
C-             ZCN  Z-position of the vertex of the cone (xcn=ycn=0)
C-             THCN Opening angle of the cone
C-   Outputs : NS   Number of intersections (0 or 2)
C-             S(2) List of the arc lengths of the inter. along the line
C-   Controls:
C-
C-   Created  24-JUL-1989   Michael W. Peters
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PI.DEF'
      REAL VTX(3),DIR(3),ZCN,THCN
      INTEGER NS
      REAL S(2)
      REAL TSQTH
      DOUBLE PRECISION A,B,C,RT
C
      IF(ABS(THCN-HALFPI).GT..001) THEN
        TSQTH=TAN(THCN)**2
        A=DIR(1)**2+DIR(2)**2-DIR(3)**2*TSQTH
        B=DIR(1)*VTX(1)+DIR(2)*VTX(2)-DIR(3)*(VTX(3)-ZCN)*TSQTH
        C=VTX(1)**2+VTX(2)**2-(VTX(3)-ZCN)**2*TSQTH
        RT=B**2-A*C
        IF(A.EQ.0..OR.RT.LT.0.) THEN
          NS=0
        ELSE
          RT=SQRT(RT)
          S(1)=(-B+RT)/A
          S(2)=(-B-RT)/A
          NS=2
        ENDIF
      ELSEIF(DIR(3).NE.0.) THEN
        S(1)=-VTX(3)/DIR(3)
        NS=1
      ELSE
        NS=0
      ENDIF
  999 RETURN
      END
