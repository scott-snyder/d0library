      SUBROUTINE MTC_LINEPNTDIR(NPNTS,XPNTS,YPNTS,ZPNTS,EZPNTS,
     &  POINT,COSDIR,CHICALIN)
C----------------------------------------------------------------------
C- MTC_LINEPNTDIR: part of MTC (Muon Tracking in the Calorimeter) package
C-
C- Purpose and Methods : Perform a weighted linear fit in 3-space
C-      to the input points XPNTS(),YPNTS(),ZPNTS().
C-      The uncertainty in the ZPNTS() array is EZPNTS().
C-
C- Inputs  : NPNTS - the number of points to be fit
C-           XPNTS(), YPNTS(), ZPNTS() - x,y,z coordinates
C-           EZPNTS() - uncertainty in z coordinates
C- Outputs : POINT(3) - a point on the 3-D fit line
C-           COSDIR(3) - direction cosines of the fit line --
C-             make sure the direction of the direction cosines is
C-             pointed in the direction from the first point in the
C-             input array to the last
C-           CHICALIN - chi squared
C- CALLS:
C-   MTC_LINEFIT to get the linear fits to the input points in the xz and
C-              yz planes
C-   MTC_LINTOCOS to turn the slopes and intercepts from the 2 linear
C-              fits into a point on the 3D match line and 3 direction
C-              cosines
C-   MTC_CHILIN is a functn which calculates the track residual - the
C-              square root of the sum of the squares of the perpendicular
C-              distance of the input points from the fit line ...
C-
C-   Created  18-JAN-1994       Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- input
      REAL XPNTS(*), YPNTS(*), ZPNTS(*), EZPNTS(*)
      INTEGER NPNTS
C- output
      REAL POINT(3),COSDIR(3),CHICALIN
C- functn
      REAL MTC_CHILIN
C- local
      REAL ZXSLP,ZXINT, ZYSLP,ZYINT
      REAL A(3),B(3),C, XMAG1,XMAGN
      INTEGER index1,index2
      INTEGER idir,ipnts
      REAL    avg,dist,c1,c2,c3
C- need some constants ...
      REAL PI, TWOPI, HALFPI
      PARAMETER (PI=3.141593,TWOPI=6.283185,HALFPI=1.570796)
C----------------------------------------------------------------------
C- get the linear fits in the xz and yz planes
      CALL MTC_LINEFIT(NPNTS,XPNTS,ZPNTS,EZPNTS, zxslp,zxint)
      CALL MTC_LINEFIT(NPNTS,YPNTS,ZPNTS,EZPNTS, zyslp,zyint)
C----------------------------------------------------------------------
C- turn the slopes and intercepts from the 2 linear fits into
C- a point on the line and direction cosines ...
      CALL MTC_LINTOCOS(ZXINT,ZXSLP, ZYINT,ZYSLP, point,cosdir)
      IF(cosdir(1).EQ.0..AND.cosdir(2).EQ.0..AND.cosdir(3).EQ.0.) THEN
        WRITE(6,80) (cosdir(ipnts),ipnts=1,3)
   80   FORMAT(' MTC_LINEPNTDIR:  error - bad cosdir ',3f7.4)
        go to 81
      END IF
C----------------------------------------------------------------------
C- make sure the direction cosines point toward outside the detector
C- (no longer neccessarily from the first pt to the last) ...
C- get the vector components along the fitted line ...
      A(1) = 5.0* COSDIR(1)
      A(2) = 5.0* COSDIR(2)
      A(3) = 5.0* COSDIR(3)
C- choosing the first and last points,
C- designate as APNT(1:3) the one furthest from the origin,
C- and BPNT(1:3) the one closest to the origin
      xmag1 = xpnts(1)**2 + xpnts(1)**2 + xpnts(1)**2
      xmagn = xpnts(npnts)**2 + xpnts(npnts)**2 + xpnts(npnts)**2
      IF(xmag1.GE.xmagn) THEN
        index1 = 1
        index2 = npnts
      ELSE
        index1 = npnts
        index2 = 1
      END IF
C- find the angle between the fitted line and a line from an
C- inner to an outer (further from origin) point in the fit
      B(1) = XPNTS(INDEX1) - XPNTS(index2)
      B(2) = YPNTS(INDEX1) - YPNTS(index2)
      B(3) = ZPNTS(INDEX1) - ZPNTS(index2)
C- C is the dot product of vectors A and B ...
      C    = A(1)*B(1) + A(2)*B(2) + A(3)*B(3)
Ccc      WRITE(6,*) a(1),a(2),a(3), b(1),b(2),b(3), c
      IF(C.LT.0.) THEN
        COSDIR(1) = -COSDIR(1)
        COSDIR(2) = -COSDIR(2)
        COSDIR(3) = -COSDIR(3)
      END IF
C----------------------------------------------------------------------
C- Put the point at the average location of all points in the fit 
C- in the direction of the largest direction cosine ...
      avg = 0.
      c1 = abs(cosdir(1))
      c2 = abs(cosdir(2))
      c3 = abs(cosdir(3))
      IF(C3.GT.C1 .AND. C3.GT.C2) THEN
        DO 10 ipnts=1,npnts
          avg = avg + zpnts(ipnts)
   10   CONTINUE
        IDIR = 3
      ELSE IF(C1.GE.C3 .AND. C1.GT.C2) THEN
        DO 11 ipnts=1,npnts
          avg = avg + xpnts(ipnts)
   11   CONTINUE
        IDIR = 1
      ELSE
        DO 12 ipnts=1,npnts
          avg = avg + ypnts(ipnts)
   12   CONTINUE
        IDIR = 2
      END IF
      avg  = avg / float(npnts)
      dist = (avg-point(idir)) / cosdir(idir)
      A(1) = dist * COSDIR(1)
      A(2) = dist * COSDIR(2)
      A(3) = dist * COSDIR(3)
      point(1) = point(1) + a(1)
      point(2) = point(2) + a(2)
      point(3) = point(3) + a(3)
   81 CONTINUE
C----------------------------------------------------------------------
C- calculate the chi squared ...
      CHICALIN = MTC_CHILIN(NPNTS,XPNTS,YPNTS,ZPNTS,POINT,COSDIR)
C----------------------------------------------------------------------
  999 RETURN
      END
