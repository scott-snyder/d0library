      SUBROUTINE FLINSG_2LAY(HALF,NLADD,LADDRS,SAME)
C------------------------------------------------------------------------
C
C    Purpose and Methods : Make FDC track candidates (ladders) by linking
C                      track segments found in inner and outer THETA
C                      chambers and in PHI chamber (UNIT=0,1,2,respectively).
C                      Loop over segments in inner THETA chamber and match
C                      segments in outer THETA chamber  by comparing  their
C                      parameters (phi and theta); match segments from
C                      PHI chamber by comparing phi.
C                      Make only two layer ladders from unused segments.
C                      Based on FLINSG.
C
C    Input  : HALF                = FDC side
C    Output : NLADD               = number of ladders
C             LADDRS(0:2,ILADD)   = segments on ladder ILADD
C
C-   Created  26-DEC-1991   Susan K. Blessing
C-   Updated  19-MAR-1992   Susan K. Blessing  Change checking to see if
C-    segments have been used already.
C-   Updated   6-APR-1992   Robert E. Avery  Suppress error messages for
C-    Production passes.
C-   Updated   2-APR-1993   Susan K. Blessing  Use theta slopes to match
C-    segments for ladders.
C-   Updated  13-APR-1993   Susan K. Blessing  Add SAME flag to tell if
C-    FLINSG_2LAY is being called again for the same event and half in
C-    case MXTRAK was hit.
C-   Updated  10-JUN-1993   Susan K. Blessing  Set SEG0A1,SEG1A1,SEG1B1 to
C-    1.
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
      INTEGER HALF,MODULE,LAYER,SEC,SEC1,SEC2
      INTEGER NPHI,NTHETA,H,U,QU,W,UB,LOGCHA
      INTEGER NLADD,LADDRS(0:2,MXTRAK),IL,LAYER1
      INTEGER SEG0,SEG1,SEG2
      INTEGER LAD(0:2)
      INTEGER IBIT,JBIT,LOC,LOC1,LOC2,ILAD,IER
      INTEGER LSEG(0:2),NSEG(0:2)
      INTEGER GZFSEG,LZFIND,LZFVAL,NZBANK
      INTEGER NDELAY0,NDELAY1
      INTEGER SEG0A,SEG1A,SEG2A,SEG1B,SEG2B
      INTEGER SEG0A1,SEG1A1,SEG2A1,SEG1B1,SEG2B1
C
      REAL TPHI,BASEPHI,TPHI2
      REAL PHI,PHI1,PHI2,PHIAV,R1,Z1,DR1
      REAL THETA,THETA1
      REAL DELPHI,TOLPHI(2),TOLTHE,TOLPDR,MAXDR1
      REAL SLOPE,SLOPE1,SLOPE2,TOLSLOPE
      REAL THETA_SLOPE_MAX,PHI_SLOPE_MAX
C
      LOGICAL FIRST,GOODSC
      LOGICAL PRODUC, PRODFL
      LOGICAL SAME
      LOGICAL FSECTOR_IN_ROAD
C
      SAVE FIRST,TOLPDR,TOLPHI,TOLTHE,PRODFL
      SAVE SEG0A,SEG1A,SEG2A,SEG1B,SEG2B,NSEG
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
        CALL EZGET('TOLPHI',TOLPHI,IER)
        CALL EZGET('TOLTHE',TOLTHE,IER)
        CALL EZGET('TOLSLOPE',TOLSLOPE,IER)
        CALL EZGET('THETA_SLOPE_MAX',THETA_SLOPE_MAX,IER)
        CALL EZGET('PHI_SLOPE_MAX',PHI_SLOPE_MAX,IER)
        CALL EZRSET
        PRODFL = PRODUC()
        FIRST = .FALSE.
      ENDIF
C
C Tell FTWO_TO_THREE that that the next two layer ladders it receives
C were built by FLINSG and the RCP options about building three layer
C ladders are to be overridden.
      CALL FTWO_TO_THREE_FIT_ALL(.TRUE.)
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
        SEG1A = 1
        SEG2A = 1
        SEG1B = 1
        SEG2B = 1
        SEG0A1 = 1
        SEG1A1 = 1
        SEG1B1 = 1
      END IF
C
C Loop over segments in inner Theta chamber
C
      LOC = LZFIND(IXCOM,LSEG(0),SEG0A,-5)
      DO 100 SEG0 = SEG0A,NSEG(0)
        SEG0A1 = SEG0
C
C Check if this segment has been used
        IF (.NOT.BTEST(IQ(LOC),IUSED)) THEN
C
C Check slope
          SLOPE = Q(LOC+30)
          IF (ABS(SLOPE).LE.THETA_SLOPE_MAX) THEN
C
C Check if sector segment is in the road
            LOGCHA = IQ(LOC+2)
            CALL FCODER(LOGCHA,H,U,QU,SEC,W,UB,1)
            IF (FSECTOR_IN_ROAD(H,U,QU,SEC)) THEN
C
C Check for delay lines
              CALL FCHECK_DL(HALF,0,SEG0,NDELAY0)
C
              PHI = Q(LOC+20)
              THETA = Q(LOC+21)
C
C Match segment in outer THETA sector.
C
              LOC1 = LZFVAL(IXCOM,LSEG(1),SLOPE,TOLSLOPE,30)
C
              IF (LOC1.GT.0) THEN
  200           CONTINUE
C
                SEG1 = IQ(LOC1-5)
                IF (SEG1.GE.SEG1A) THEN
C Check if outer layer segment has been used
                  IF (.NOT.BTEST(IQ(LOC1),IUSED)) THEN
C
C Check slope
                    SLOPE1 = Q(LOC1+30)
                    IF (ABS(SLOPE1).LE.THETA_SLOPE_MAX) THEN
C
C Check if sector segment is in the road
                      LOGCHA = IQ(LOC1+2)
                      CALL FCODER(LOGCHA,H,U,QU,SEC1,W,UB,1)
                      IF (FSECTOR_IN_ROAD(H,U,QU,SEC1)) THEN
C
                        SEG1A1 = SEG1
                        THETA1 = Q(LOC1+21)
                        IF (ABS(SLOPE-SLOPE1).LT.TOLSLOPE.AND.
     &                    ABS(THETA-THETA1).LT.TOLTHE ) THEN
C
C Check for delay lines
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
                          IF (DELPHI.LT.BASEPHI+TPHI.AND.
     &                      DELPHI.GT.BASEPHI-TPHI) THEN
C
C Do not allow matches when inner theta sector is more than two sectors
C farther out than outer theta sector.
                            GOODSC = (SEC1-SEC.GE.-2)
                            IF (GOODSC) THEN
C
                              NLADD = NLADD+1
                              LADDRS(0,NLADD) = SEG0
                              LADDRS(1,NLADD) = SEG1
                              LADDRS(2,NLADD) = 0
                              IF (NLADD.GE.MXTRAK) GO TO 999
C
                            END IF
                          END IF
                        END IF
                      END IF
                    END IF
                  END IF
                END IF
                LOC1 = LQ(LOC1)
                IF (LOC1.GT.0) GO TO 200
              END IF
C
C Try matching in Phi chamber
C
              IF (NDELAY0.GT.0) THEN
                TPHI2 = TOLPHI(2)
              ELSE
                TPHI2 = PI/8. + .001
              END IF
C
              IF (PHI.LT.TPHI2.OR.PHI.GT.TWOPI-TPHI2) THEN
                LOC2 = LSEG(2)
              ELSE
                LOC2 = LZFVAL(IXCOM,LSEG(2),PHI,TPHI2,36)
              ENDIF
              IF (LOC2.GT.0) THEN
C
  600           CONTINUE
C
                SEG2 = IQ(LOC2-5)
                IF (SEG2.GE.SEG2A) THEN
C Check if phi layer segment has been used
                  IF (.NOT.BTEST(IQ(LOC2),IUSED)) THEN
C
C Check slope
                    SLOPE2 = Q(LOC2+55)
                    IF (ABS(SLOPE2).LE.PHI_SLOPE_MAX) THEN
C
C Check if sector segment is in the road
                      LOGCHA = IQ(LOC2+2)
                      CALL FCODER(LOGCHA,H,U,QU,SEC2,W,UB,1)
                      IF (FSECTOR_IN_ROAD(H,U,QU,SEC2)) THEN
C
                        SEG2A1 = SEG2A
                        PHI2 = Q(LOC2+36)
                        DR1 = Q(LOC2+37)
                        Z1  = Q(LOC2+38)
                        R1  = ABS(Z1*(SLOPE+SLOPE1)/2.)
C maximum possible drift for radius
                        MAXDR1 = R1*TAN(5.*RADIAN)
                        IF (ABS(DR1).LE.MAXDR1+TOLPDR) THEN
C
                          IF (R1 .NE. 0.) THEN
                            PHI2 = PHI2+ATAN(DR1/R1)
                          ENDIF
                          DELPHI = ABS(PHI2-PHI)
                          IF (DELPHI.GT.PI) DELPHI = TWOPI-DELPHI
                          IF (DELPHI.LT.TOLPHI(2)) THEN
C
                            NLADD = NLADD+1
                            LADDRS(0,NLADD) = SEG0
                            LADDRS(1,NLADD) = 0
                            LADDRS(2,NLADD) = SEG2
                            IF (NLADD.GE.MXTRAK) GO TO 999
                          ENDIF
                        ENDIF
                        LOC2 = LQ(LOC2)
                        IF (LOC2.GT.0) GO TO 600
                      END IF
                    END IF
                  END IF
                END IF
              END IF
            END IF
          END IF
        END IF
C
        LOC = LQ(LOC)
C
  100 CONTINUE
C
C  Matching segments in outer theta and phi.
C
      LOC1 = LZFIND(IXCOM,LSEG(1),SEG1B,-5)
  700 CONTINUE
      IF (LOC1.EQ.0) GO TO 999
C
      SEG1 = IQ(LOC1-5)
      SEG1B1 = SEG1
C
C Check if outer layer segment has been used
      IF (.NOT.BTEST(IQ(LOC1),IUSED)) THEN
C
C Check slope
        SLOPE1 = Q(LOC1+30)
        IF (ABS(SLOPE1).LE.THETA_SLOPE_MAX) THEN
C
C Check if segment is in the road
          LOGCHA = IQ(LOC1+2)
          CALL FCODER(LOGCHA,H,U,QU,SEC1,W,UB,1)
          IF (FSECTOR_IN_ROAD(H,U,QU,SEC1)) THEN
C
C Check for delay lines
            CALL FCHECK_DL(HALF,1,SEG1,NDELAY1)
            IF (NDELAY1.GT.0) THEN
              TPHI2 = TOLPHI(2)
            ELSE
              TPHI2 = PI/8. + .001
            END IF
C
            PHI1 = Q(LOC1+20)
C
C  Match segment in PHI sector.
C
            IF (PHI1.LT.TPHI2.OR.
     &        PHI1.GT.TWOPI-TPHI2) THEN
              LOC2 = LSEG(2)
            ELSE
              LOC2 = LZFVAL(IXCOM,LSEG(2),PHI1,TPHI2,36)
            ENDIF
            IF (LOC2.EQ.0) GO TO 900
  800       CONTINUE
C
            SEG2 = IQ(LOC2-5)
            IF (SEG2.GE.SEG2B) THEN
C Check if phi layer segment has been used
              IF (.NOT.BTEST(IQ(LOC2),IUSED)) THEN
C
C Check slope
                SLOPE2 = Q(LOC2+55)
                IF (ABS(SLOPE2).LE.PHI_SLOPE_MAX) THEN
C
C Check if segment is in the road
                  LOGCHA = IQ(LOC2+2)
                  CALL FCODER(LOGCHA,H,U,QU,SEC2,W,UB,1)
                  IF (FSECTOR_IN_ROAD(H,U,QU,SEC2)) THEN
C
                    SEG2B1 = SEG2
                    PHI2 = Q(LOC2+36)
                    DR1 = Q(LOC2+37)
                    Z1  = Q(LOC2+38)
                    R1  = ABS(Z1*SLOPE1)
C Maximum possible drift for radius
                    MAXDR1 = R1*TAN(5.*RADIAN)
                    IF (ABS(DR1).LE.MAXDR1+TOLPDR) THEN
C
C  Using drift distance for first hit and theta from THETA segments to be
C  matched we can calculate phi of PHI segment:
C
                      IF (R1 .NE. 0.) THEN
                        PHI2 = PHI2+ATAN(DR1/R1)
                      ENDIF
                      DELPHI = ABS(PHI2-PHI1)
                      IF (DELPHI.GT.PI) DELPHI = TWOPI-DELPHI
                      IF (DELPHI.LT.TPHI2) THEN
C
                        NLADD = NLADD+1
                        LADDRS(0,NLADD) = 0
                        LADDRS(1,NLADD) = SEG1
                        LADDRS(2,NLADD) = SEG2
                        IF (NLADD.GE.MXTRAK) GO TO 999
                      END IF
                    END IF
                  END IF
                END IF
              END IF
            END IF
            LOC2 = LQ(LOC2)
            IF (LOC2.GT.0) GO TO 800
  900       CONTINUE
          END IF
        END IF
      END IF
      LOC1 = LQ(LOC1)
      GO TO 700
C
C---------------------------------------------------------------------
  999 CONTINUE
C
      SEG0A = SEG0A1
      SEG1A = SEG1A1
      SEG1B = SEG1B1
C
      IF ( NLADD.GE.MXTRAK ) THEN
        NLADD = MXTRAK
        IF ( .NOT.PRODFL ) THEN
          CALL ERRMSG('FDC-Too-Many-Ladders','FLINSG_2LAY',
     &      ' number of potential FDC tracks greater than MXTRAK',
     &      'W')
        ENDIF
      ENDIF
C
      RETURN
      END
