
      SUBROUTINE MUPLN(XP,DIRC,XC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds coordiante of intercept of a track with
C-                         muon iron system. 
C-
C-   Inputs  : XP(3)     Where track vector defined
C-             DIRC(3)   Dirction cosines of track
C-
C-   Outputs : XC(3)     intercept coordinates
C-   Controls:
C-
C-   Created   3-JUL-1992   SHAHRIAR ABACHI
C-                          The intercept point is on the closer edge 
C-                          from collision point    
C-   Updated   29-Sep-1992  Atsushi Taketani Use STP information
C-                          The intercept point is sitting on center
C-                          of magnet, not edge of magnet.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL XP(3),DIRC(3),XC(3)
      INTEGER I,IHIT,IH
      REAL EPS
      REAL XH(3),YH(3),ZH(3),XR,YR,ZR
      REAL DX,DY,DZ,HX(3),HY(3),HZ(3)
      DATA EPS /1.E-12/
C
      IF(DIRC(3) .GE. 0) THEN            ! South  EF
        CALL MUZBND(21,DZ)
      ELSE
        CALL MUZBND(25,DZ)               ! North EF
      ENDIF
C
      IF(DIRC(2) .GE. 0) THEN            ! TOP CF
        CALL MUZBND(2,DY)
      ELSE
        CALL MUZBND(4,DY)                ! Bottom CF
      ENDIF
C
      IF(DIRC(1) .GE. 0) THEN            ! East CF
        CALL MUZBND(1,DX)
      ELSE
        CALL MUZBND(3,DX)                ! West CF
      ENDIF
C
      CALL MUPLNYZ(DX,XP,DIRC,HX,IHIT)
      XR = SQRT(HX(1)**2 + HX(2)**2 + HX(3)**2)
      IF(IHIT .EQ. 0) XR = 1.E9
      IH = 1
C
      CALL MUPLNZX(DY,XP,DIRC,HY,IHIT)
      YR = SQRT(HY(1)**2 + HY(2)**2 + HY(3)**2)
      IF(IHIT .EQ. 0) YR = 1.E9
      IF(YR .LT. XR) IH = 2
C
      CALL MUPLNXY(DZ,XP,DIRC,HZ,IHIT)
      ZR = SQRT(HZ(1)**2 + HZ(2)**2 + HZ(3)**2)
      IF(IHIT .EQ. 0) ZR = 1.E9
      IF(ZR .LT. XR .AND. ZR .LT. YR) IH = 3
C
      IF(IH .EQ. 1) THEN
        DO I=1,3
          XC(I) = HX(I)
        ENDDO
      ELSEIF(IH .EQ. 2) THEN
        DO I=1,3
          XC(I) = HY(I)
        ENDDO
      ELSEIF(IH .EQ. 3) THEN
        DO I=1,3
          XC(I) = HZ(I)
        ENDDO
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
