      SUBROUTINE CLINPH_FAST_XYZ(VTX,DIR,XYZ,ARGSOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds XYZ of first calor  hit by line
C-
C-   Inputs  : VTX(3)         Point origin of the line
C-             DIR(3)         Direction cosines of the line
C-   Outputs : XYZ(3)        X,Y,Z of track hit point in calorimeter
C-             ARGSOK         Flag for arg errors
C-   Controls:
C-
C-   Created 14-APR-1992   W.G.D.Dharmaratna, same as CLINPH_FAST
C-                         except the output is x,y and z
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER LAYER,IETA
      REAL    CC_EM3_RAD,CENRAD,DRAD,EC_EM3_ZED(2),CENZED,DZED,AROKIN
      REAL    TILT
      REAL    SEM3_CC(2),SEM3_EC
      INTEGER NS_EM3_CC,NS_EM3_EC
      INTEGER I
      REAL    VTX(*),DIR(*),XYZ(3)
      REAL  ZCC,SCC
      INTEGER ICC,IERR,ARGSOK
      REAL    POINT_INT(3)
      REAL    CC_CELL_ZED(-13:13),EC_CELL_RAD(13:NETAL)
C
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        IETA=1
        LAYER = 3
        CALL CALRAD(IETA,LAYER,CC_EM3_RAD,DRAD,CENZED,DZED,AROKIN)
        IETA= 32
        CALL CALZED(IETA,LAYER,EC_EM3_ZED(1),DZED,CENRAD,DRAD,
     &      TILT,AROKIN)
        IETA = -32
        CALL CALZED(IETA,LAYER,EC_EM3_ZED(2),DZED,CENRAD,DRAD,
     &      TILT,AROKIN)
      ENDIF
C
C ****  FIND INTERSECTION WITH CC_EM3 CYLINDER
C
      ARGSOK = 0
      CALL CLNRD(VTX,DIR,CC_EM3_RAD,NS_EM3_CC,SEM3_CC)
      IF(NS_EM3_CC.EQ.0)THEN
        ARGSOK = 1
        GO TO 999
      ENDIF
C
      SCC = SEM3_CC(1)
      IF(SCC.LT.SEM3_CC(2))SCC = SEM3_CC(2)
      ZCC = VTX(3) + SCC*DIR(3)
      ICC = 1
      IF(ZCC.GT.EC_EM3_ZED(1).OR.ZCC.LT.EC_EM3_ZED(2))ICC =0
C
      IF(ICC.EQ.1)THEN                  ! IN CC
        DO I = 1,3
          XYZ(I) = VTX(I) + DIR(I)*SCC
        ENDDO
      ELSE                              ! IN EC
        IF(ZCC.GT.0.0)THEN
          CALL CLNZD(VTX,DIR,EC_EM3_ZED(1),NS_EM3_EC,SEM3_EC)
        ELSE
          CALL CLNZD(VTX,DIR,EC_EM3_ZED(2),NS_EM3_EC,SEM3_EC)
        ENDIF
        DO I = 1,3
          XYZ(I) = VTX(I) + DIR(I)*SEM3_EC
        ENDDO
      ENDIF
C
  999 RETURN
      END
