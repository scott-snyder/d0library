      REAL FUNCTION MTC_CHILIN(NLAYER,XPNTS,YPNTS,ZPNTS,PNT1,COSDIR)
C----------------------------------------------------------------------
C- MTC_CHILIN: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : Calculate the chisquared - a measure of
C-      the goodness of fit of the line defined by the point PNT1(3)
C-      and the direction cosines COSDIR(3)
C-      to the NLAYER 3-d points (x,y,z).
C-
C-   Inputs  : NLAYER - number of points
C-             XPNTS - x coordinates
C-             YPNTS - y coordinates
C-             ZPNTS - z coordinates
C-             PNT1(3) - a point on the line of the best fit
C-             COSDIR(3) - direction cosines of the best fit line
C-   Output  : MTC_CHILIN a number equal to the value of the sqrt of
C-      the sum of the squares of the perpendicular distance from
C-      the x,y,z points to the fit line
C-      divided by the number of points
C-
C-   Created   6-OCT-1993   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- Input ...
      INTEGER NLAYER
      REAL XPNTS(*),YPNTS(*),ZPNTS(*)
      REAL PNT1(3),COSDIR(3)
C- local
      INTEGER JLAYER,IERR, IDUM
      REAL SUM,PNT2(3),CALPNT(3),DIST
C----------------------------------------------------------------------
C- get another point on the line so I can call MTC_PERPDIST ...
      PNT2(1) = PNT1(1) + 5.0* COSDIR(1)
      PNT2(2) = PNT1(2) + 5.0* COSDIR(2)
      PNT2(3) = PNT1(3) + 5.0* COSDIR(3)
      SUM = 0.
      DO 11 JLAYER=1,NLAYER
        CALPNT(1) = XPNTS(JLAYER)
        CALPNT(2) = YPNTS(JLAYER)
        CALPNT(3) = ZPNTS(JLAYER)
        CALL MTC_PERPDIST(CALPNT,PNT1,PNT2,DIST,IERR)
        IF(IERR.NE.0) WRITE(6,80) (CALPNT(IDUM),IDUM=1,3),
     &    (PNT1(IDUM),IDUM=1,3),(PNT2(IDUM),IDUM=1,3)
        SUM = SUM + DIST**2
   11 CONTINUE
      MTC_CHILIN = SQRT(SUM) / FLOAT(NLAYER)
C----------------------------------------------------------------------
   80 FORMAT(' MTC_CHILIN: error calling MTC_PERPDIST !!!',
     &  3F8.0,2X,3F8.0,2X,3F8.0)
  999 RETURN
      END
