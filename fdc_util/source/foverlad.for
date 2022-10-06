      SUBROUTINE FOVERLAD(HALF,NLADD,LADDRS,SAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check all segments which have not been
C-      used in ladders and make ladders of segments in overlapping
C-      sectors.  Only two layer ladders are formed.
C-
C-   Inputs  : HALF
C-   Outputs : NLADD(0:1) = Number of ladders formed in each FDC Half
C-             LADDRS() = Array of ladders
C-
C-   Created   2-JAN-1991   Susan K. Blessing
C-   Updated  26-APR-1991   Jeffrey Bantly  cleanup PARAMS,RCP
C-   Updated  17-JUN-1991   Susan K. Blessing  Change size of CONT, ICONT
C-    arrays.
C-   Updated  11-JUL-1991   Susan K. Blessing  Let FTRAKS set NLADD to zero.
C-   Updated  11-JUL-1991   Susan K. Blessing  Change size of LADDRS,NLADD
C-    arrays.  Don't need HALF information.
C-   Updated  29-AUG-1991   Robert E. Avery  Call ERRMSG if too many ladders.
C-   Updated  17-DEC-1991   Susan K. Blessing  Allow for not building
C-    theta-theta, inner theta-phi, phi-outer theta overlapping ladders.
C-   Updated  19-DEC-1991   Susan K. Blessing
C-    Call FTWO_TO_THREE_FIT_ALL(.FALSE.) to tell FTWO_TO_THREE to use
C-    the RCP options about building three layer ladders.
C-   Updated   6-JAN-1992   Susan K. Blessing  USEDSEG wasn't being set
C-    to .FALSE..
C-   Updated   6-APR-1992   Robert E. Avery  Suppress error messages for
C-    Production passes.
C-   Updated   7-DEC-1992   Susan K. Blessing  EZRSET call missing.
C-   Updated  13-APR-1993   Susan K. Blessing  Add SAME flag to tell if
C-    FOVERLAD is being called again for the same event and half in case
C-    MXTRAK was hit.
C-   Updated   7-JUN-1993   Susan K. Blessing  Set SEG0A1, SEG1A1, SEG2A1,
C-    SEG1B1, and SEG2B1 to 1.  Bug fix.
C-   Updated  17-AUG-1994   Susan K. Blessing  Use FSECTOR_IN_ROAD to check
C-    if segments are in road before building ladders.
C-   Updated  30-AUG-1994   Susan K. Blessing  Add check on slope of segments
C-    before using them.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER I
      INTEGER DUM,ADUM(50,4)
      INTEGER LAYER
      INTEGER LSEG(0:2)
      INTEGER HALF
      INTEGER IADD
      INTEGER IER
      INTEGER LAD(0:2)
      INTEGER NLADD,LADDRS(0:2,MXTRAK)
      INTEGER ICONT(62)
      INTEGER H,WIRE
      INTEGER LOC0,LOC1,LOC2
      INTEGER LOC0A,LOC1A,LOC2A
      INTEGER LOC1B,LOC2B
      INTEGER LZFIND
      INTEGER MODULE
      INTEGER NSEG(0:2)
      INTEGER SEG,SEG0,SEG1,SEG2
      INTEGER UNIT0,QDRT0,SCTR0
      INTEGER UNIT1,QDRT1,SCTR1
      INTEGER UNIT2,QDRT2,SCTR2
      INTEGER GZFSEG,NZBANK
      INTEGER SEG0A,SEG1A,SEG2A
      INTEGER SEG1B,SEG2B
      INTEGER SEG0A1,SEG1A1,SEG2A1
      INTEGER SEG1B1,SEG2B1
C
      REAL CONT(62)
      EQUIVALENCE (CONT,ICONT)
      REAL SLOPE0,SLOPE1,SLOPE2,TOLSLOPE
      REAL THETA0,THETA1,TOLTHE
      REAL THETA_SLOPE_MAX,PHI_SLOPE_MAX
C
      LOGICAL FIRST
      LOGICAL OVERLAP
      LOGICAL UBIT
      LOGICAL SAME
      LOGICAL FSECTOR_IN_ROAD
C
      LOGICAL FCHEKL
      LOGICAL TT_OVERLAD,PT_OVERLAD,TP_OVERLAD
      LOGICAL PRODUC, PRODFL
C
      SAVE FIRST,TT_OVERLAD,PT_OVERLAD,TP_OVERLAD,PRODFL
      SAVE SEG0A,SEG1A,SEG2A,SEG1B,SEG2B,NSEG
      SAVE THETA_SLOPE_MAX,PHI_SLOPE_MAX
C
      DATA FIRST/.TRUE./
      DATA TT_OVERLAD/.TRUE./
      DATA PT_OVERLAD/.TRUE./
      DATA TP_OVERLAD/.TRUE./
      DATA THETA_SLOPE_MAX,PHI_SLOPE_MAX/2*100./
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET_l('TT_OVERLAD',TT_OVERLAD,IER)
        CALL EZGET_l('PT_OVERLAD',PT_OVERLAD,IER)
        CALL EZGET_l('TP_OVERLAD',TP_OVERLAD,IER)
        CALL EZGET('TOLSLOPE',TOLSLOPE,IER)
        CALL EZGET('TOLTHE',TOLTHE,IER)
        CALL EZGET('THETA_SLOPE_MAX',THETA_SLOPE_MAX,IER)
        CALL EZGET('PHI_SLOPE_MAX',PHI_SLOPE_MAX,IER)
        CALL EZRSET
        PRODFL = PRODUC()
        FIRST = .FALSE.
      END IF
C
C Tell FTWO_TO_THREE to use the RCP options for building three layer ladders.
      CALL FTWO_TO_THREE_FIT_ALL(.FALSE.)
C
C Get number of segments in each layer
C
      DO LAYER = 0, 2
        LSEG(LAYER) = GZFSEG(HALF,LAYER)
      END DO
C
      IF (SAME) THEN
        LOC0A = LZFIND(IXCOM,LSEG(0),SEG0A,-5)
        LOC1A = LZFIND(IXCOM,LSEG(1),SEG1A,-5)
        LOC2A = LZFIND(IXCOM,LSEG(2),SEG2A,-5)
        LOC1B = LZFIND(IXCOM,LSEG(1),SEG1B,-5)
        LOC2B = LZFIND(IXCOM,LSEG(2),SEG2B,-5)
      ELSE
        DO LAYER = 0, 2
          IF (LSEG(LAYER).GT.0) THEN
            NSEG(LAYER) = NZBANK(IXCOM,LSEG(LAYER))
          ELSE
            NSEG(LAYER) = 0
          END IF
        END DO
        SEG0A = 1
        SEG1A = 1
        SEG2A = 1
        SEG1B = 1
        SEG2B = 1
        SEG0A1 = 1
        SEG1A1 = 1
        SEG2A1 = 1
        SEG1B1 = 1
        SEG2B1 = 1
        LOC0A = LSEG(0)
        LOC1A = LSEG(1)
        LOC2A = LSEG(2)
        LOC1B = LSEG(1)
        LOC2B = LSEG(2)
      END IF
C
C Loop over segments in inner layer
C
      IF (TP_OVERLAD.OR.TT_OVERLAD) THEN
        LOC0 = LOC0A
        DO SEG0 = SEG0A, NSEG(0)
          SEG0A1 = SEG0
C
C Check if segment has been used
          IF (.NOT.BTEST(IQ(LOC0),IUSED)) THEN
C
C Check slope
            SLOPE0 = Q(LOC0+30)
            IF (ABS(SLOPE0).LE.THETA_SLOPE_MAX) THEN
C
C Check if sector segment is in the road
              IADD = IQ(LOC0+2)
              CALL FCODER(IADD,H,UNIT0,QDRT0,SCTR0,WIRE,UBIT,1)
              IF (FSECTOR_IN_ROAD(H,UNIT0,QDRT0,SCTR0)) THEN
C
C Get sector information for segment
                THETA0 = Q(LOC0+21)
C
C Loop over segments in outer layer
                IF (TT_OVERLAD) THEN
                  LOC1 = LOC1A
                  DO SEG1 = SEG1A, NSEG(1)
                    SEG1A1 = SEG1
C
C Check if outer layer segment has been used
                    IF (.NOT.BTEST(IQ(LOC1),IUSED)) THEN
C
C Check slope
                      SLOPE1 = Q(LOC1+30)
                      IF (ABS(SLOPE1).LE.THETA_SLOPE_MAX) THEN
C
C Check if sector segment is in the road
                        IADD = IQ(LOC1+2)
                        CALL 
     &                    FCODER(IADD,H,UNIT1,QDRT1,SCTR1,WIRE,UBIT,1)
                        IF (FSECTOR_IN_ROAD(H,UNIT1,QDRT1,SCTR1)) THEN
C
C Get sector information for segment
                          THETA1 = Q(LOC1+21)
C
C Check slopes
                          IF (ABS(SLOPE0-SLOPE1).LT.TOLSLOPE.AND.
     &                      ABS(THETA0-THETA1).LT.TOLTHE) THEN
C
C Check if two sectors overlap
                            CALL FOVRLP(HALF,UNIT0,QDRT0,SCTR0,
     &                        HALF,UNIT1,QDRT1,SCTR1,1,DUM,ADUM,OVERLAP)
C
                            IF (OVERLAP) THEN
C
                              NLADD = NLADD + 1
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
                    LOC1 = LQ(LOC1)
                  END DO
                END IF
C
                IF (TP_OVERLAD) THEN
C Loop over segments in phi layer
                  LOC2 = LOC2A
                  DO SEG2 = SEG2A, NSEG(2)
                    SEG2A1 = SEG2
C
C Check if phi layer segment has been used
                    IF (.NOT.BTEST(IQ(LOC2),IUSED)) THEN
C
C Check slope
                      SLOPE2 = Q(LOC2+55)
                      IF (ABS(SLOPE2).LE.PHI_SLOPE_MAX) THEN
C
C Check if sector segment is in the road
                        IADD = IQ(LOC2+2)
                        CALL 
     &                    FCODER(IADD,H,UNIT2,QDRT2,SCTR2,WIRE,UBIT,1)
                        IF (FSECTOR_IN_ROAD(H,UNIT2,QDRT2,SCTR2)) THEN
C
C Check if two sectors overlap
                          CALL FOVRLP(HALF,UNIT0,QDRT0,SCTR0,
     &                      HALF,UNIT2,QDRT2,SCTR2,1,DUM,ADUM,OVERLAP)
C
                          IF (OVERLAP) THEN
C
                            NLADD = NLADD + 1
                            LADDRS(0,NLADD) = SEG0
                            LADDRS(1,NLADD) = 0
                            LADDRS(2,NLADD) = SEG2
                            IF (NLADD.GE.MXTRAK) GO TO 999
C
                          END IF
                        END IF
                      END IF
                    END IF
C
                    LOC2 = LQ(LOC2)
                  END DO
                END IF
              END IF
            END IF
          END IF
C
          LOC0 = LQ(LOC0)
        END DO
      END IF
C
C
      IF (PT_OVERLAD) THEN
C Loop over segments in outer layer
        LOC1 = LOC1B
        DO SEG1 = SEG1B, NSEG(1)
          SEG1B1 = SEG1
C
C Check if outer layer segment has been used
          IF (.NOT.BTEST(IQ(LOC1),IUSED)) THEN
C
C Check slope
            SLOPE1 = Q(LOC1+30)
            IF (ABS(SLOPE1).LE.THETA_SLOPE_MAX) THEN
C
C Check if sector segment is in the road
              IADD = IQ(LOC1+2)
              CALL FCODER(IADD,H,UNIT1,QDRT1,SCTR1,WIRE,UBIT,1)
              IF (FSECTOR_IN_ROAD(H,UNIT1,QDRT1,SCTR1)) THEN
C
C Loop over segments in phi layer
                LOC2 = LOC2B
                DO SEG2 = SEG2B, NSEG(2)
                  SEG2B1 = SEG2
C
C Check if phi layer segment has been used
                  IF (.NOT.BTEST(IQ(LOC2),IUSED)) THEN
C
C Check slope
                    SLOPE2 = Q(LOC2+55)
                    IF (ABS(SLOPE2).LE.PHI_SLOPE_MAX) THEN
C
C Check if sector segment is in the road
                      IADD = IQ(LOC2+2)
                      CALL FCODER(IADD,H,UNIT2,QDRT2,SCTR2,WIRE,UBIT,1)
                      IF (FSECTOR_IN_ROAD(H,UNIT2,QDRT2,SCTR2)) THEN
C
C Check if two sectors overlap
                        CALL FOVRLP(HALF,UNIT1,QDRT1,SCTR1,
     &                    HALF,UNIT2,QDRT2,SCTR2,1,DUM,ADUM,OVERLAP)
C
                        IF (OVERLAP) THEN
C
                          NLADD = NLADD + 1
                          LADDRS(0,NLADD) = 0
                          LADDRS(1,NLADD) = SEG1
                          LADDRS(2,NLADD) = SEG2
                          IF (NLADD.GE.MXTRAK) GO TO 999
C
                        END IF
                      END IF
                    END IF
                  END IF
C
                  LOC2 = LQ(LOC2)
                END DO
              END IF
            END IF
          END IF
C
          LOC1 = LQ(LOC1)
        END DO
      END IF
C
C----------------------------------------------------------------------
  999 CONTINUE
C
      SEG0A = SEG0A1
      SEG1A = SEG1A1
      SEG2A = SEG2A1
      SEG1B = SEG1B1
      SEG2B = SEG2B1
C
      IF ( NLADD.GE.MXTRAK ) THEN
        NLADD = MXTRAK
        IF ( .NOT.PRODFL ) THEN
          CALL ERRMSG('FDC-Too-Many-Ladders','FOVERLAD',
     &      ' number of potential FDC tracks greater than MXTRAK',
     &      'W')
        ENDIF
      ENDIF
C
      RETURN
      END
