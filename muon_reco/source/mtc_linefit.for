      SUBROUTINE MTC_LINEFIT(NPTS,AVECT,BVECT, EBVECT, ABSLP,ABINT)
C----------------------------------------------------------------------
C- MTC_LINEFIT: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : Do a weighted linear fit to a set of
C-      NPTS points with coordinates AVECT(1:NPTS), BVECT(1:NPTS).
C-      The uncertainties in the points BVECT() are EBVECT().
C-
C-   Inputs  : NPTS - the number of sets of points
C-             AVECT(1:NPTS) - the 'x' values
C-             BVECT(1:NPTS) - the 'y' values
C-             EBVECT(1:NPTS) - the 'y' value uncertainties
C-   Outputs : abslp - a slope
C-             abint - an intercept
C-
C-   Created   18-JAN-1994      Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- input and output
      INTEGER NPTS
      REAL    AVECT(NPTS),BVECT(NPTS),EBVECT(NPTS)
      REAL    ABSLP,ABINT
C- local
      REAL    WEIGHTS(18),VAR
      INTEGER IPTS,KEY
      REAL    alfa, xg, yg, vg, valfa, chisq
C- need some constants ...
      REAL PI, TWOPI, HALFPI
      PARAMETER (PI=3.1415927,TWOPI=6.2831853,HALFPI=1.5707963)
C----------------------------------------------------------------------
C- at least 2 points ?
      IF(NPTS.LE.1) WRITE(6,88) NPTS
   88 FORMAT(' MTC_LINEFIT:  error NPTS = !!!',I6)
C- weights are the inverse square of the uncertainty
      DO 10 IPTS=1,NPTS
        WEIGHTS(IPTS) = 1. / (EBVECT(IPTS)**2)
   10 CONTINUE

C- 2-MAY-1994 Persistent crashes in LFITW have caused me to consider
C- other fitting options ... try one of Daria's routines below ...
cc      KEY = 0                           ! don't skip any points
cc      CALL LFITW(AVECT,BVECT,WEIGHTS,NPTS,KEY, ABSLP,ABINT,VAR)

      CALL FITLIN( AVECT, BVECT, WEIGHTS, NPTS,
     &             alfa, xg, yg, vg, valfa, chisq)
      ABSLP = TAN(ALFA)
      ABINT = YG - XG*ABSLP
C----------------------------------------------------------------------
      RETURN
      END
