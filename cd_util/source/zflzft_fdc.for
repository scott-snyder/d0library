      SUBROUTINE ZFLZFT_FDC(LZFIT,LFDCT,ZVERT,NHIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Put information from fit of FDC track into 
C-    ZFIT bank when the ZTRK is only an FDC track.
C-
C-   Inputs  : LZFIT = link to ZFIT bank
C-             LFDCT = link to FDCT bank
C-             USE_VERTEX = TRUE to redo fit using vertex point
C-   Outputs : NHIT = number of hits on track
C-   Controls: 
C-
C-   Created   9-MAR-1992   Susan K. Blessing
C-   Updated   6-APR-1992   Susan K. Blessing  Put number of hits on track
C-    in ZFIT in +6 and +7 words.  Put number of DOF in +28 and +29 words.
C-   Updated  22-APR-1993   Susan K. Blessing  Replace USE_VERTEX in call
C-    with ZVERT to be able to fit to a specified vertex.  Fix setting
C-    of vertex used bits.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER NHIT
      INTEGER LZFIT,LFDCT
      INTEGER LADDER(0:2)
      INTEGER IQTRAK(26)
      INTEGER TRACK
C
      REAL THETA,PHI
      REAL STHETA,CTHETA,SINPHI,COSPHI
      REAL Z0
      REAL ZVERT
      REAL QTRAK(26),QHSEC(3,34)
      EQUIVALENCE(IQTRAK,QTRAK)
C
      LOGICAL EDGE
C
C----------------------------------------------------------------------
C
C Check if edge track, in which case vertex point has already been used
      EDGE = BTEST(IQ(LFDCT+1),1)
C
      IF (ZVERT.NE.-9999..AND..NOT.EDGE) THEN
C
C Do fit using vertex point
        CALL FITFDC_VERTEX(LFDCT,ZVERT,QTRAK)
C
      ELSE
C
C Get track information
        TRACK = IQ(LFDCT-5)
        CALL GTFDCT(TRACK,QTRAK,QHSEC,LADDER)
C
      END IF
C
C Fill in ZFIT bank
C
C Number of hits on track
      NHIT = IQTRAK(2)
C Use this same number for the number of X-Y hits and Z coordinates on track
      IQ(LZFIT+6) = NHIT
      IQ(LZFIT+7) = NHIT
C
C Number of DOF for track fit.  Number of points in fit minus 4.  
C NOT the same as the number of hits on the track minus 4.
      IQ(LZFIT+28) = IQTRAK(25) - 4
      IQ(LZFIT+29) = IQTRAK(25) - 4
C
C CHISQ of fit
      Q(LZFIT+8) = QTRAK(19)
      Q(LZFIT+9) = QTRAK(19)
C
C X0
      Q(LZFIT+11) = QTRAK(4)
      Q(LZFIT+17) = SQRT(ABS(QTRAK(9)))
C
C Y0
      Q(LZFIT+12) = QTRAK(5)
      Q(LZFIT+19) = SQRT(ABS(QTRAK(13)))
C
C Calculate an R0
      Q(LZFIT+14) = SQRT(Q(LZFIT+11)**2 + Q(LZFIT+12)**2)
C
C Z0
      CALL FGETZ0(IQ(LFDCT-5),Z0)
      Q(LZFIT+15) = Z0
C
C PHI and THETA
      PHI = QTRAK(6)
      THETA = QTRAK(22)
      Q(LZFIT+10) = PHI
      Q(LZFIT+16) = QTRAK(23)
      Q(LZFIT+13) = THETA
      Q(LZFIT+18) = QTRAK(24)
C
C Calculate the direction cosines
C
      STHETA = SIN(THETA)
      CTHETA = COS(THETA)
      SINPHI = SIN(PHI)
      COSPHI = COS(PHI)
      Q(LZFIT+20) = STHETA * COSPHI
      Q(LZFIT+22) = STHETA * SINPHI
      Q(LZFIT+24) = CTHETA
      Q(LZFIT+21) = SQRT( COSPHI**2 * CTHETA**2 * Q(LZFIT+18)**2 +
     &                    SINPHI**2 * STHETA**2 * Q(LZFIT+16)**2)
      Q(LZFIT+23) = SQRT( SINPHI**2 * CTHETA**2 * Q(LZFIT+18)**2 +
     &                    COSPHI**2 * STHETA**2 * Q(LZFIT+16)**2)
      Q(LZFIT+25) = STHETA * Q(LZFIT+18)
C
C If vertex used in fit or edge track, set bits indicating vertex point 
C has been used
      IF (EDGE.OR.ZVERT.NE.-9999.) THEN
        IQ(LZFIT+2) = IBSET(IQ(LZFIT+2),0)
        IQ(LZFIT+2) = IBSET(IQ(LZFIT+2),1)
      END IF
C
  999 RETURN
      END
