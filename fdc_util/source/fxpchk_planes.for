      SUBROUTINE FXPCHK_PLANES(HALF,SECTOR,SECTD1,WIRES,DRIFTS,
     &                         L1HIT,L2HIT,VARIANCE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Perform an intersecting planes fit on two
C-                         candidate cross Phi sector chains.
C-
C-   Inputs  : HALF,SECTOR  = Logical address of home sector
C-             SECTD1       = Sector of adjacent sector
C-             WIRES(NBPSEN)= Sense wires on chains
C-             DRIFTS( " )  = Drift distances of hits on chains
C-             ZPOS  ( " )  = Wire Z positions of hits on chains
C-             L1HIT,L2HIT  = Last hit on 1st and 2nd chain
C-   Outputs : VARIANCE     = Variance of fit to line of hits
C-
C-   Created  12-JUN-1991   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER HALF,SECTOR               ! logical address of home sector
      INTEGER SECTD1                    ! sector of adjacent sector
      INTEGER WIRES(NBPSEN)             ! sense wires on chains
      INTEGER L1HIT,L2HIT               ! last hits on 1st and 2nd chain
      INTEGER I                         ! loop counter
      INTEGER IERR                      ! error check
C
      REAL    DRIFTS(NBPSEN)            ! drift distances of hits on chains
      REAL    A,B                       ! coefficients of 2-D line 
      REAL    A1,B1,C1,D1,A2,B2,C2,D2   ! coefficients of 3-D plane fit
      REAL    CHISQ                     ! chi-square of line fit
      REAL    DIST                      ! calculated distance to line
      REAL    LA,LB,LC,LD               ! coefficients of 3-D line fit
      REAL    RESID                     ! residuals of fit to line of hits
      REAL    VARIANCE                  ! variance of fit to line of hits
      REAL    X(3),Y(3),Z(3)            ! three 3-D points to form plane
      REAL    X1,Y1,Z1,X2,Y2,Z2         ! two points on fitted 3-D lines
      REAL    XC,YC,ZC                  ! position of wire centers
      REAL    XIN,YIN,RIN               ! entry point for 3-D line
      REAL    XOUT,YOUT,ROUT            ! exit point for 3-D line
      REAL    XT,YT                     ! location of point along line
C
      LOGICAL OK                        ! TRUE if three 3-D points okay
C
C----------------------------------------------------------------------
C
      VARIANCE = 9999.0
C
C  Calculate three 3-D points for use in determining a 3-D plane
C  in home sector.
C
      CALL FPHXYZ(HALF,SECTOR,1,L1HIT,WIRES,DRIFTS,X,Y,Z,OK)
      IF(.NOT. OK) GOTO 999
C
C  Calculate the 3-D plane coefficients from the three points
C  in home sector.
C
      CALL FPLANE(3,X,Y,Z,A1,B1,C1,D1,IERR)
      IF(IERR.NE.0) GOTO 999
C
C  Calculate three 3-D points for use in determining a 3-D plane
C  in adjacent sector.
C
      CALL FPHXYZ(HALF,SECTD1,L1HIT+1,L2HIT,WIRES,DRIFTS,X,Y,Z,OK)
      IF(.NOT. OK) GOTO 999
C
C  Calculate the 3-D plane coefficients from the three points
C  in adjacent sector.
C
      CALL FPLANE(3,X,Y,Z,A2,B2,C2,D2,IERR)
      IF(IERR.NE.0) GOTO 999
C
C  Calculate two points on the line of intersection of the two planes
C  and, using those points, the coefficients of the 3-D line.
C
      CALL FDILIN(HALF,A1,B1,C1,D1,A2,B2,C2,D2,X1,Y1,Z1,X2,Y2,Z2)
C
      IF( (Z2-Z1).EQ. 0.0 ) GOTO 999
      LA=(X2-X1)/(Z2-Z1)
      LB=(Y2-Y1)/(Z2-Z1)
      LC=-1.*( LA*(X2-X1) + LB*(Y2-Y1) ) / (Z2-Z1)
      LD=-1.*( LA*X1 + LB*Y1 + LC*Z1 )
C
C  Calculate the Phi chamber entry and exit points of the 3-D line.
C  Cut on the radial extent of the Phi chamber.
C
      CALL GTFALH(HALF,1,0,SECTOR,WIRES(1),XC,YC,ZC)
      XIN=X1+LA*(ZC-Z1)
      YIN=Y1+LB*(ZC-Z1)
      RIN=( XIN**2. + YIN**2. )**.5
      CALL GTFALH(HALF,1,0,SECTD1,WIRES(L2HIT),XC,YC,ZC)
      XOUT=X1+LA*(ZC-Z1)
      YOUT=Y1+LB*(ZC-Z1)
      ROUT=( XOUT**2. + YOUT**2. )**.5
      IF( (RIN.LT.8.0) .OR. (RIN.GT.60.) ) GOTO 999
      IF( (ROUT.LT.8.0) .OR. (ROUT.GT.60.) ) GOTO 999
      IF( ABS(RIN-ROUT).GT. 60.0 ) GOTO 999
C
C  Calculate the residuals and chi-square of the hits on this line.
C  Compare the distance between a point on the line and the sense wire
C  of the hit (at that wire's Z position) and the actual drift distance
C  for the hit.
C
      CHISQ = 0.
      DO I=1,L2HIT
        IF(I.LE.L1HIT) THEN
          CALL GTFALH(HALF,1,0,SECTOR,WIRES(I),XC,YC,ZC)
        ELSE
          CALL GTFALH(HALF,1,0,SECTD1,WIRES(I),XC,YC,ZC)
        ENDIF
        IF(YC.NE.0.0) THEN
          A=1
          B=-1.*XC/YC
        ELSEIF(XC.NE.0.0) THEN
          A=-1.*YC/XC
          B=1
        ELSE
          GOTO 999
        ENDIF
        XT=X1+(LA*(ZC-Z1))
        YT=Y1+(LB*(ZC-Z1))
        DIST = (A*XT+B*YT) / ( (A**2. + B**2.)**.5 )
        RESID = ABS(DRIFTS(I)) - ABS(DIST)
        CHISQ = CHISQ + (RESID*RESID)
      ENDDO
      VARIANCE = CHISQ/(FLOAT(L2HIT-2)*0.04)
C
C----------------------------------------------------------------------
  999 RETURN
      END
