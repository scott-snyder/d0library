      SUBROUTINE FLINSG(HALF,NLADD,LADDRS,SAME)
C------------------------------------------------------------------------
C
C    Purpose and Methods : Make FDC track candidates (ladders) by linking
C                      track segments found in inner and outer THETA
C                      chambers and in PHI chamber (UNIT=0,1,2,respectively).
C                      Loop over segments in inner THETA chamber and match
C                      segments in outer THETA chamber  by comparing  their
C                      parameters (phi and theta); match segments from
C                      PHI chamber by comparing phi.
C
C    Input  : HALF                = FDC side
C    Output : NLADD               = number of ladders
C             LADDRS(0:2,ILADD)   = segments on ladder ILADD
C
C-   Created   x-DEC-1988   Daria Zieminska
C-   Updated   8-FEB-1990   Jeffrey W. Bantly  fix minor bugs
C-   Updated  26-FEB-1990   Jeffrey Bantly  make PATH dependent
C-   Updated   9-APR-1990   Jeffrey Bantly  allow for layer 1,2 ladders
C-   Updated   8-NOV-1990   Jeffrey Bantly  check max drift in Phi and
C-                                          change PHIAV calculation
C-   Updated  26-APR-1991   Jeffrey Bantly  cleanup PARAMS,RCP
C-   Updated   6-JUN-1991   Susan K. Blessing  Check on number of associated
C-    delay line hits for theta segments.  If there are no associated
C-    delay line hits, open up the angle tolerances for matching segments.
C-   Updated   7-JUN-1991   Susan K. Blessing  Let FTRAKS set NLADD to zero.
C-   Updated  11-JUL-1991   Susan K. Blessing  Change size of LADDRS,NLADD
C-    arrays.  Don't need HALF information.
C-   Updated  29-AUG-1991   Robert E. Avery  Call ERRMSG if too many ladders.
C-   Updated   5-DEC-1991   Susan K. Blessing  Use a more clever method
C-    of matching for segments without delay lines.  Fix up a couple of
C-    small things.
C-   Updated  19-DEC-1991   Susan K. Blessing
C-    Call FTWO_TO_THREE_FIT_ALL(.TRUE.) to tell FTWO_TO_THREE that the next
C-    two layer ladders it receives were built by FLINSG and the RCP options
C-    about building three layer ladders are to be overridden.
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block.
C-   Updated  19-MAR-1992   Susan K. Blessing  Check whether segments
C-    have been used already.  Needed for road tracking.
C-   Updated   6-APR-1992   Robert E. Avery  Suppress error messages for
C-    Production passes.
C-   Updated   2-APR-1993   Susan K. Blessing  Use theta slopes to match
C-    segments for ladders along with the "theta" for theta segments.
C-    Remove building two layer ladders.  Only build three layer ladders here.
C-    Add SAME flag to tell if FLISNG is being called again for the same event
C-    and half in case MXTRAK was hit.
C-   Updated  28-JUN-1993   Susan K. Blessing  Remove IZUSER.LINK.
C-   Updated  17-AUG-1994   Susan K. Blessing  Use FSECTOR_IN_ROAD to check
C-    if segments are in road before building ladders.
C-   Updated  30-AUG-1994   Susan K. Blessing  Add check on slope of segments
C-    before using them.
C
C------------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER HALF,LAYER,SEC,SEC1,SEC2,QUAD
      INTEGER NPHI,H,U,QU,W,UB,LOGCHA
      INTEGER NLADD,LADDRS(0:2,MXTRAK),IL,LAYER1
      INTEGER SEG0,SEG1,SEG2
      INTEGER IBIT,JBIT,LOC,LOC1,LOC2,ILAD,IER
      INTEGER LSEG(0:2),NSEG(0:2)
      INTEGER GZFSEG,LZFIND,LZFVAL,NZBANK
      INTEGER NDELAY0,NDELAY1
      INTEGER QU1,QU2
      INTEGER SEG0A,SEG0A1
C
      REAL TPHI,BASEPHI,TPHI2
      REAL PHI,PHI1,PHI2,PHIAV,R1,Z1,DR1
      REAL DELPHI,TOLPHI(2),TOLTHE,TOLPDR,MAXDR1
      REAL SLOPE,SLOPE1,SLOPE2,TOLSLOPE
      REAL THETA,THETA1
      REAL THETA_SLOPE_MAX,PHI_SLOPE_MAX
C
      LOGICAL FIRST,GOODSC
      LOGICAL PRODUC, PRODFL
      LOGICAL SAME
      LOGICAL FSECTOR_IN_ROAD
C
      SAVE FIRST,TOLPDR,TOLPHI,TOLTHE,TOLSLOPE,PRODFL
      SAVE THETA_SLOPE_MAX,PHI_SLOPE_MAX
C
      DATA FIRST/.TRUE./
      DATA THETA_SLOPE_MAX,PHI_SLOPE_MAX/2*100./
C
C------------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('TOLPDR',TOLPDR,IER)
        CALL EZGET_rarr('TOLPHI',TOLPHI,IER)
        CALL EZGET('TOLTHE',TOLTHE,IER)
        CALL EZGET('TOLSLOPE',TOLSLOPE,IER)
        CALL EZGET('THETA_SLOPE_MAX',THETA_SLOPE_MAX,IER)
        CALL EZGET('PHI_SLOPE_MAX',PHI_SLOPE_MAX,IER)
        CALL EZRSET
        FIRST = .FALSE.
        PRODFL = PRODUC()
C
      ENDIF
C
C Get number of segments in each layer
C
      DO LAYER = 0, 2
C
        LSEG(LAYER) = GZFSEG(HALF,LAYER)
        IF (.NOT.SAME) THEN
C LFSEG=0 means no segments found
          IF (LSEG(LAYER).GT.0) THEN
            NSEG(LAYER) = NZBANK(IXCOM,LSEG(LAYER))
          ELSE
            NSEG(LAYER) = 0
          END IF
        END IF
C
      END DO
C
      IF (.NOT.SAME) THEN
        SEG0A = 1
      END IF
C
C Loop over segments in inner Theta chamber
C
      DO 100 SEG0 = SEG0A,NSEG(0)
        SEG0A1 = SEG0
        LOC = LZFIND(IXCOM,LSEG(0),SEG0,-5)
C
C Check if segment has been used
        IF (BTEST(IQ(LOC),IUSED)) GO TO 100
C
C Check slope
        SLOPE = Q(LOC+30)
        IF (ABS(SLOPE).GT.THETA_SLOPE_MAX) GO TO 100
C        
C Check if sector segment is in the road
        LOGCHA = IQ(LOC+2)
        CALL FCODER(LOGCHA,H,U,QU,SEC,W,UB,1)
        IF (.NOT.FSECTOR_IN_ROAD(H,U,QU,SEC)) GO TO 100
C
C Check for delay lines
        CALL FCHECK_DL(HALF,0,SEG0,NDELAY0)
C
        PHI = Q(LOC+20)
        THETA = Q(LOC+21)
C
C Match segment in outer THETA sector.
C Match slopes
        LOC1 = LZFVAL(IXCOM,LSEG(1),SLOPE,TOLSLOPE,30)
C
        IF (LOC1.GT.0) THEN
  200     CONTINUE
C
C Check if segment has been used
          IF (BTEST(IQ(LOC1),IUSED)) GO TO 101
C
C Check slope
          SLOPE1 = Q(LOC1+30)
          IF (ABS(SLOPE1).GT.THETA_SLOPE_MAX) GO TO 101
C
C Check if sector segment is in the road
          LOGCHA = IQ(LOC1+2)
          CALL FCODER(LOGCHA,H,U,QU1,SEC1,W,UB,1)
          IF (.NOT.FSECTOR_IN_ROAD(H,U,QU1,SEC1)) GO TO 101
C
          THETA1 = Q(LOC1+21)
          IF (ABS(SLOPE-SLOPE1).LT.TOLSLOPE.AND.
     &        ABS(THETA-THETA1).LT.TOLTHE ) THEN
C
C Check for delay lines
            SEG1 = IQ(LOC1-5)
            CALL FCHECK_DL(HALF,1,SEG1,NDELAY1)
C
C If both SEG0 and SEG1 have delay lines, use TOLPHI(1) as the matching
C tolerance.  If both are missing delay lines, difference must be 2pi/8.
C If only one has a delay line, difference is between 0 and 2pi/8+a little
C due to errors in finding the delay line which is present.
C
            IF (NDELAY0.GT.0.AND.NDELAY1.GT.0) THEN
              TPHI = TOLPHI(1)
              BASEPHI = 0.
              TPHI2 = TOLPHI(2)
            ELSE IF (NDELAY0.GT.0.OR.NDELAY1.GT.0) THEN
              TPHI = TWOPI/8. + TOLPHI(1)/2.
              BASEPHI = 0.
              TPHI2 = PI/8. + TOLPHI(2)/2.
            ELSE
              TPHI = .001
              BASEPHI = TWOPI/8.
              TPHI2 = PI/8. + .001
            END IF
C
            PHI1 = Q(LOC1+20)
            DELPHI = ABS(PHI1-PHI)
            IF (DELPHI.GT.PI) DELPHI = TWOPI-DELPHI
C
C Cut on phi difference
            IF (DELPHI.LT.BASEPHI+TPHI.AND.DELPHI.GT.BASEPHI-TPHI) THEN
C
C Do not allow matches when inner theta sector is more than two sectors
C farther out than outer theta sector.
              GOODSC = (SEC1-SEC.GE.-2)
              IF (GOODSC) THEN
C
C  Match segment in PHI sector.
C
                IF (ABS(PHI1-PHI).GT.PI) THEN
                  PHIAV = MIN(PHI1,PHI)-DELPHI/2.
                  IF (PHIAV.LT.0.) PHIAV = PHIAV+TWOPI
                ELSE
                  PHIAV = MIN(PHI1,PHI)+DELPHI/2.
                ENDIF
                IF (PHIAV.LT.TPHI2.OR.
     &            PHIAV.GT.TWOPI-(TPHI2)) THEN
                  LOC2 = LZFIND(IXCOM,LSEG(2),1,-5)
                ELSE
                  LOC2 = LZFVAL(IXCOM,LSEG(2),PHIAV,TPHI2,36)
                ENDIF
                IF (LOC2.EQ.0) GO TO 400
C
  500           CONTINUE
C
C Check if segment has been used
                IF (BTEST(IQ(LOC2),IUSED)) GO TO 103
C
C Check slope
                SLOPE2 = Q(LOC2+55)
                IF (ABS(SLOPE2).GT.PHI_SLOPE_MAX) GO TO 103
C
C Check if segment sector is in road
                LOGCHA = IQ(LOC2+2)
                CALL FCODER(LOGCHA,H,U,QU2,SEC2,W,UB,1)
                IF (.NOT.FSECTOR_IN_ROAD(H,U,QU2,SEC2)) GO TO 103
C
                PHI2 = Q(LOC2+36)
                DR1 = Q(LOC2+37)
                Z1  = Q(LOC2+38)
C
                R1  = ABS(Z1*((SLOPE+SLOPE1)/2.))
                MAXDR1 = R1*TAN(5.*RADIAN)  ! maximum possible drift for radius
                IF (ABS(DR1).LE.MAXDR1+TOLPDR) THEN
C
C  Using drift distance for first hit and theta from THETA segments to be
C  matched we can calculate phi of PHI segment:
C
                  IF (R1 .NE. 0.) THEN
                    PHI2 = PHI2+ATAN(DR1/R1)
                  ENDIF
                  DELPHI = ABS(PHI2-PHIAV)
                  IF (DELPHI.GT.PI) DELPHI = TWOPI-DELPHI
                  IF (DELPHI.LT.TPHI2) THEN
C
                    SEG2 = IQ(LOC2-5)
                    NPHI = NPHI+1
C
                    NLADD = NLADD+1
                    LADDRS(0,NLADD) = SEG0
                    LADDRS(1,NLADD) = SEG1
                    LADDRS(2,NLADD) = SEG2
C
                    IF (NLADD.GE.MXTRAK) GO TO 999
                  ENDIF
                ENDIF
  103           CONTINUE
                LOC2 = LQ(LOC2)
                IF (LOC2.GT.0) GO TO 500
C
  400           CONTINUE
              ENDIF
            ENDIF
          ENDIF
  101     CONTINUE
          LOC1 = LQ(LOC1)
          IF (LOC1.GT.0) GO TO 200
        ENDIF
  100 CONTINUE
C
C---------------------------------------------------------------------
  999 CONTINUE
C
      SEG0A = SEG0A1
C
      IF ( .NOT.PRODFL ) THEN
        IF ( NLADD.GE.MXTRAK ) THEN
          NLADD = MXTRAK
          CALL ERRMSG('FDC-Too-Many-Ladders','FLINSG',
     &      ' number of potential FDC tracks greater than MXTRAK',
     &      'W')
        ENDIF
      ENDIF
C
      RETURN
      END
