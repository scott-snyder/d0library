      SUBROUTINE MTC_LINE_CALV1(IETAIN,IPHIIN,ILYRMIN,
     &  POINT,COSDIR,CHICALIN)
C----------------------------------------------------------------------
C- MTC_LINE_CALV1: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods :  Fit a line through a set of points
C-      in the calorimeter from layer number ILYRMIN to layer 17.
C-      Call this routine when the fraction of
C-      1x1 layers hit is 1.0 and the chisquared is very low.
C-      The points fit in this routine include the vertex position and
C-      the geometric center of the calorimeter cells in the projective
C-      tower at ieta+ietain,iphi+iphiin where ieta and iphi are the
C-      index of the current central eta phi in /MTC_ETOWERS/.
C-      The output fitted line is defined by direction cosines COSDIR
C-      and an arbitrary  point POINT(3) through which the line passes.
C-      The linear fit is weighted according to the uncertainties in the
C-      points used:  for the vertex, the standard uncertainty is used,
C-      for the cal points, uncertainty = half the extent of the cell in z
C-
C-   Inputs  : /MTC_ETOWERS/
C-             IETAIN,IPHIIN - described above
C-             ILYRMIN - minimum layer to look at in the calorimeter
C-   Outputs :
C-      POINT(3), COSDIR(3) - define the fitted line
C-      CHICALIN - the chisquared of the fit -
C-              equal to the sum of the squares of the perpendicular
C-              distance from the fitted line to the calorimeter
C-              cell points
C-       If the input arguments are inappropriate:
C-              If abs(ietain) or abs(iphiin) are gt ithi, or if
C-              there is no energy in the given tower, then
C-              POINT(IXYZ) = 0., COSDIR(IXYZ) = 0., CHICALIN = -50.
C-
C-   Created   18-JAN-1994      Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- input ...
      INCLUDE 'D0$INC:MTC_ETOWERS.INC'
      INTEGER IETAIN,IPHIIN,ILYRMIN
C- output
      REAL POINT(3), COSDIR(3), CHICALIN
C- uncertainty in z ...
      REAL ZLONG, EZPNTS(18)
C- local ...
      INTEGER ILYR
      REAL X,Y,Z, XPNTS(18),YPNTS(18),ZPNTS(18)
      INTEGER OK, NLAYERS, IMAX,IXYZ
      DATA IMAX/0/
C----------------------------------------------------------------------
      IF(IMAX.EQ.0) THEN
        IMAX = ITHI+1
      END IF
C- make sure that ietain and ip2 are in range ...
      IF(ABS(IETAIN).GE.IMAX .OR. ABS(IPHIIN).GE.IMAX) GO TO 666
C- check for no energy in tower ...
      IF(ENTOWER(IETAIN,IPHIIN,18).LE.0.) GO TO 667
C- Initialize number of hit layers ...
      NLAYERS = 0
C- make the first point the vertex position ...
      NLAYERS = NLAYERS + 1
      XPNTS(NLAYERS) = VTXTWR(1)
      YPNTS(NLAYERS) = VTXTWR(2)
      ZPNTS(NLAYERS) = VTXTWR(3)
      EZPNTS(NLAYERS) = DVTXTWR(3)
C- Look for all cells in this tower with non0 energy ...
      DO 770 ILYR=ILYRMIN,17
        IF(ENTOWER(IETAIN,IPHIIN,ILYR).GT.0.) THEN
C- Get the x,y,z coordinates ...
          CALL MTC_GET_XYZ(IETAIN,IPHIIN,ILYR, X,Y,Z, OK)
          IF(OK.NE.0) THEN
            WRITE(6,86) IETAIN,IPHIIN,ILYR
            GO TO 770
          END IF
   86     FORMAT(' MTC_LINE_CALV1:  error getting x,y,z',3I5)
C- Get the z width of the cell ...
          CALL MTC_GET_LONGZ(IETAIN,IPHIIN,ILYR, ZLONG, OK)
          IF(OK.NE.0) THEN
            WRITE(6,85) IETAIN,IPHIIN,ILYR
            GO TO 770
          END IF
   85     FORMAT(' MTC_LINE_CALV1:  error getting zlong',3I5)
C- add the point to the array ...
          NLAYERS = NLAYERS + 1
          XPNTS(NLAYERS) = X
          YPNTS(NLAYERS) = Y
          ZPNTS(NLAYERS) = Z
          EZPNTS(NLAYERS) = ZLONG/2.
        END IF
  770 CONTINUE                                  ! loop over layers
C- fit the points to a line in 3 space ...
C- Require at least 3 cal layers be used ...
      IF(NLAYERS.LE.3) GO TO 667
      CALL MTC_LINEPNTDIR(NLAYERS,XPNTS, YPNTS, ZPNTS, EZPNTS,
     &  POINT,COSDIR,CHICALIN)
      GO TO 999
C----------------------------------------------------------------------
  666 CONTINUE
      WRITE(6,88) IETAIN,IPHIIN,ILYRMIN
   88 FORMAT(' MTC_LINE_CALV1: fatal error !!! illegal input ',3I4)
  667 CONTINUE
      DO 8 IXYZ=1,3
        POINT(IXYZ) = 0.
        COSDIR(IXYZ) = 0.
    8 CONTINUE
      CHICALIN = -50.
C----------------------------------------------------------------------
  999 RETURN
      END
