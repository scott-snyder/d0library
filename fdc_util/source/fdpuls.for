      SUBROUTINE FDPULS(HALF,UNIT,QUAD,SECTOR,WIRE,NPULSE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : search hits in a FADC
C-
C-   Inputs  : HALF,UNIT,QUAD,SECTOR,WIRE = Logical wire location
C-   Outputs : NPULSE     = Number of found pulses
C-
C-   Created   4-NOV-1988   Jeffrey Bantly
C-   Updated  20-FEB-1990   Jeffrey Bantly  add in peds,unpacking calls
C-   Updated   1-MAR-1990   Jeffrey Bantly  use logical format and clean up
C-   Updated  21-AUG-1990   Susan K. Blessing  include electronic T0's
C-   Updated  11-SEP-1990   Robert E. Avery  Crosstalk correction:
C-      Since the data for an entire cell is now unpacked all at once
C-      (with wire 0), it is assumed that FDPULS is called from routine
C-      that loops over all wires in a cell, or at least that the first
C-      wire for which it is called is wire 0.
C-   Updated   2-JAN-1991   Susan K. Blessing  Put early MC hit recovery
C-      back in.
C-   Updated  14-JAN-1991   Jeffrey Bantly  add pulse shape subtraction
C-   Updated   9-FEB-1991   Robert E. Avery always perform bilinear conv.
C-   Updated   9-MAR-1991   Robert E. Avery add electronics gain correction
C-   Updated  20-MAR-1991   Jeffrey Bantly  put in speed ups
C-   Updated  26-APR-1991   Jeffrey Bantly  use cleaned up PARAMS & RCP
C-   Updated  12-JUL-1991   Susan K. Blessing  Remove MAXPUL from call.
C-    Replace with MX_HIT_WIRE from PARAMS file.
C-   Updated  15-JUL-1991   Robert E. Avery  Separate FADC trace corrections
C-              into new routine, FDFADC. Lots of clean up.
C-   Updated  21-OCT-1991   Robert E. Avery  Updates of pulse shape
C-    subtraction (from Jeff).  Don't look for pulses that are too late
C-    in time (based on RCP parameter MAXTIMEFACT).  Get errors from
C-    RCP.
C-   Updated   2-DEC-1991   Robert E. Avery  Fix bug in testing number of
C-                                      hits for MC data. Clean up.
C-   Updated  21-FEB-1992   Jeffrey Bantly  use close peak reset
C-   Updated   6-APR-1992   Robert E. Avery  Suppress error messages for
C-    Production passes.
C-   Updated  12-MAY-1992   Susan K. Blessing  Declare PEDS(2) (removed
C-    from FDEVNT.INC).
C-   Updated  25-MAY-1992   Robert E. Avery   Include SHIFTT in TMPUBN
C-                                                      definition.
C-   Updated  22-JUN-1992   Susan K. Blessing  Use separate threshold
C-    multipliers for delay lines rather than double delay line FADC
C-    values in FDFADC.
C-   Updated  26-JUNE-1992  Tacy M. Joffe-Minor  Change MX_HIT_WIRE to
C-                                      HITS_PER_WIRE ot allow more hits
C-   Updated  29-JUN-1992   Susan K. Blessing  The first element of LOCCLU
C-    points to the last cluster on the wire.  Reverse the order of the
C-    loop over clusters to look at the beginning of the wire first.
C-   Updated  17-NOV-1992   Susan K. Blessing  Bug fix.  Correct checking
C-    of bin with MAX_BIN.
C-   Updated   8-DEC-1992   Robert E. Avery  Bug Fix. MAX_BIN must take
C-     tzeros into account.
C-   Updated  11-JAN-1993   Susan K. Blessing  Add two more RCP parameters
C-    (each for SW and DL) for cutting on overshoot which is found as a hit.
C-   Updated  25-FEB-1993   Robert E. Avery Change calculation of risetime etc. 
C-   Updated  15-APR-1993   Susan K. Blessing  Comment out FPULSHP_SUBTR
C-    stuff since call is incorrect and it needs modification anyway.
C-   Updated  29-JUL-1993   Susan K. Blessing  Use a different weight for
C-    large pulses.  When using CLOSE_PEAK, find beginning of hit using
C-    new end of pulse.
C-   Updated   9-NOV-1993   Robert E. Avery  Changes for speed ups.
C-      B array and thresholds (THR1, THR2, THR3) are now all explicitly
C-      integers (note: previously b was quantized by int. values anyway).
C-   Updated  14-JUL-1994   Robert E. Avery  Modified in order to 
C-      make results agree with Level 2 hitfinding results. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS/LIST'
      INCLUDE 'D0$INC:FDEVNT.INC/LIST'
C
      INTEGER I,J,S,JJ
      INTEGER CLUS_NUM
      INTEGER COUNT
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE,LOGCHA
      INTEGER EXPDAT(0:LFADC-1)
      INTEGER LOCCLU(MAX_CLUS),LENCLU(MAX_CLUS)
      INTEGER NUMCLU
      INTEGER IER, IOFS
      INTEGER IFLG
      INTEGER INIT
      INTEGER ILAST,IFIRST,IPEAK,ISUML
      INTEGER PSHLEV
      INTEGER IPEV, IPREAD
      INTEGER SHIFTT, TIMCHA
      INTEGER NATUR
      INTEGER NPULSE
      INTEGER PULMAX(2)
      INTEGER RUNTYPE
      INTEGER RESET_PEAK,RESET_THETA,RESET_PHI,RESET_DL
      INTEGER TOO_WIDE,WIDE_THETA,WIDE_PHI,WIDE_DL
      INTEGER BIN
      INTEGER MAXLENGTH
      INTEGER HITS_PER_WIRE     ! The number of hits allowed per wire
      INTEGER B(LFADC)
      INTEGER THR1, THR2, THR3
      INTEGER THR1_PED, MAX_I 
      INTEGER MAX_BIN
      INTEGER ITIME
      INTEGER IPED
C      
      REAL RTIME
      REAL AREA
      REAL COEFF(100,2,2)
      REAL FIRSTBIN,SECONDBIN,THIRDBIN,HIGHBIN
      REAL FLAG
      REAL GAIN,MIPCONV
      REAL LEADING_EDGE
      REAL PULWEI(2,2),PULWEI_CUT(2)
      REAL RISETIME,FALLTIME,WIDTH 
      REAL SUM,SUMX
      REAL TENPCT_PEAK
      REAL THR1MU, THR2MU, THR3MU
      REAL THR1MU_DL, THR2MU_DL, THR3MU_DL
      REAL ETZERO,ATZERO
      REAL VELOP,VELOM
      REAL MAXBINT,MAXBINP,MAXBINDL
      REAL MAXZDL,MAXFACT
      REAL TIME_BIN
      REAL VELOCT,VELOCP,VELOCDL
      REAL ERROR_THETA, ERROR_PHI
      REAL FDC_ERROR_SLOPE
      REAL PEDS(2)
      REAL SUM_DIFF,MIN_AVE_DIFF(0:1)
C
      REAL MAXDISTT,MAXDISTP
      PARAMETER( MAXDISTT = 5.3 )       ! Max drift distance in Theta cell
      PARAMETER( MAXDISTP = 5.2 )       ! Max drift distance in Phi cell
C
      LOGICAL CLOSE_PEAK
      LOGICAL DO_RISE
      LOGICAL EARLY_HIT
      LOGICAL IOK
      LOGICAL PUL_RETRY
      LOGICAL REDO_AREA
      LOGICAL GN_INI,PD_INI,TM_INI
      LOGICAL PRODUC, PRODFL
      LOGICAL FIXED_MAXBIN 
C
      EQUIVALENCE (FLAG,IFLG)
C
      SAVE COEFF,INIT,PRODFL
      SAVE PULWEI,PULMAX,PULWEI_CUT
      SAVE SHIFTT
      SAVE TIMCHA
      SAVE THR1MU,THR2MU,THR3MU
      SAVE THR1MU_DL,THR2MU_DL,THR3MU_DL
      SAVE MAXBINT,MAXBINP,MAXBINDL
      SAVE ERROR_THETA, ERROR_PHI
      SAVE MIN_AVE_DIFF
C
      DATA INIT/0/
      DATA MAXFACT  /1.25/
      DATA CLOSE_PEAK/.FALSE./
      DATA MIN_AVE_DIFF/2*0./
      DATA PULWEI_CUT/2*9999./
      DATA FIXED_MAXBIN /.FALSE./
C
C----------------------------------------------------------------------
C
      IF (INIT.EQ.0) THEN
        INIT = 1
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('THR1MU',THR1MU,IER)
        CALL EZGET('THR2MU',THR2MU,IER)
        CALL EZGET('THR3MU',THR3MU,IER)
        CALL EZGET('THR1MU_DL',THR1MU_DL,IER)
        IF (THR1MU_DL.EQ.0) THR1MU_DL = 0.5*THR1MU
        CALL EZGET('THR2MU_DL',THR2MU_DL,IER)
        IF (THR2MU_DL.EQ.0) THR2MU_DL = 0.5*THR2MU
        CALL EZGET('THR3MU_DL',THR3MU_DL,IER)
        IF (THR3MU_DL.EQ.0) THR3MU_DL = 0.5*THR3MU
        CALL EZGET('PULWEI',PULWEI,IER)
        CALL EZGET('PULWEI_CUT',PULWEI_CUT,IER)
        CALL EZGET('PULMAX',PULMAX,IER)
        CALL EZGET('TIMCHA',TIMCHA,IER)
        CALL EZGET('SHIFTT',SHIFTT,IER)
        CALL EZGET('CLOSE_PEAK',CLOSE_PEAK,IER)
        CALL EZGET('RESET_THETA',RESET_THETA,IER)
        CALL EZGET('RESET_DL',RESET_DL,IER)
        CALL EZGET('RESET_PHI',RESET_PHI,IER)
        CALL EZGET('WIDE_THETA',WIDE_THETA,IER)
        CALL EZGET('WIDE_DL',WIDE_DL,IER)
        CALL EZGET('WIDE_PHI',WIDE_PHI,IER)
        CALL EZGET('DO_RISE',DO_RISE,IER)
        CALL EZGET('RUNTYPE',RUNTYPE,IER)
        CALL EZGET('PULSE_SHAPE_LEVEL',PSHLEV,IER)
        CALL EZGET('REDO_AREA',REDO_AREA,IER)
        CALL EZGET('VELOCT',VELOCT,IER)
        CALL EZGET('VELOCP',VELOCP,IER)
        CALL EZGET('HITS_PER_WIRE',HITS_PER_WIRE,IER)
        CALL EZGET('MIN_AVE_DIFF',MIN_AVE_DIFF,IER)
        CALL EZGET('GN_INI',GN_INI,IER)
        CALL EZGET('TM_INI',TM_INI,IER)
        CALL EZGET('FIXED_MAXBIN',FIXED_MAXBIN,IER)
C
        IF ( FIXED_MAXBIN  ) THEN
          MAXBINP  = 165		
          MAXBINT  = 180		
          MAXBINDL = 220		
        ELSE
          CALL EZGET('MAXTIMEFACT',MAXFACT,IER)
          MAXBINP = MAXFACT * MAXDISTP * 10000./VELOCP
          MAXBINT = MAXFACT * MAXDISTT * 10000./VELOCT
C  Longest delay line is quad 1, sector 4
          CALL FGTLTM(0,0,1,4,8,ETZERO,ATZERO,VELOCDL,MAXZDL)
          MAXBINDL = MAXBINT + MAXZDL/VELOCDL
C
          MAXBINP  = NINT(MAXBINP/NBPBIN) 
          MAXBINT  = NINT(MAXBINT/NBPBIN) 
          MAXBINDL = NINT(MAXBINDL/NBPBIN) 
        ENDIF
C
        CALL EZRSET
C
C  Assign errors as if track is at zero slope:
C
        ERROR_THETA = FDC_ERROR_SLOPE(0.0,0) * 10000./VELOCT
        ERROR_PHI = FDC_ERROR_SLOPE(0.0,1) * 10000./VELOCP
C
        DO J = 1, 2
          COEFF(1,J,1) = 1.
          COEFF(1,J,2) = 1.
          DO I = 2, 100
            COEFF(I,J,1) = COEFF(I-1,J,1) * PULWEI(J,1)
            COEFF(I,J,2) = COEFF(I-1,J,2) * PULWEI(J,2)
          ENDDO
        ENDDO
        PRODFL = PRODUC()
C
      ENDIF
      NPULSE=0
      EARLY_HIT = .FALSE.
C
C Get FADC trace, with all corrections
C
      CALL FDFADC( HALF,UNIT,QUAD,SECTOR,WIRE,
     &             EXPDAT,NUMCLU,LOCCLU,LENCLU)
      IF ( NUMCLU .EQ. 0) GOTO 999
C
C ****  Get logical address, and constants
C
      CALL FCODER(LOGCHA,HALF,UNIT,QUAD,SECTOR,WIRE,0,2)
      CALL FGTLPD(HALF,UNIT,QUAD,SECTOR,WIRE,PEDS(1),PEDS(2))
      IPED = NINT(PEDS(1))
      IF ( TM_INI ) THEN
        CALL FGTLTM(HALF,UNIT,QUAD,SECTOR,WIRE,ETZERO,ATZERO,
     &                                 VELOP,VELOM)
      ELSE
        ETZERO = 0.0
      ENDIF
      IF ( GN_INI ) THEN
        CALL FGTLGN(HALF,UNIT,QUAD,SECTOR,WIRE,GAIN,MIPCONV)
      ELSE
        GAIN = 1.0
      ENDIF
C
C ****  Set chamber dependent variables
C
      IF (UNIT .LE. 0) THEN
        IF (WIRE .GE. NBTSEN) THEN
          NATUR = 2
          MAX_BIN = MAXBINDL
        ELSE
          NATUR = 1
          MAX_BIN = MAXBINT
        ENDIF
      ELSE
        NATUR = 1
        MAX_BIN = MAXBINP
      ENDIF
      IF ( .NOT.FIXED_MAXBIN ) THEN  
        MAX_BIN = MAX_BIN + NINT( (ETZERO + ATZERO) / NBPBIN )
      ENDIF
C
      IF (FINDTP) THEN
        MAX_BIN = MAX_BIN + TMPUBN
      ENDIF
C
C ****  Thresholds FDC way
C
      IF (UNIT.EQ.0.AND.WIRE.GT.MXWIRT) THEN
        THR1 = THR1MU_DL * PEDS(2)
        THR2 = THR2MU_DL * PEDS(2)
        THR3 = THR3MU_DL * PEDS(2)
      ELSE
        THR1 = THR1MU * PEDS(2)
        THR2 = THR2MU * PEDS(2)
        THR3 = THR3MU * PEDS(2)
      END IF
C
      IF (THR1 .LT. 0) THR1 = 0
      IF (THR2 .LT. 2)  THR2 = 2    !Default for Zero-supp data
      IF (THR3 .LT. 10) THR3 = 10
C
C ****  Loop over clusters in channel data
C
      DO 200 CLUS_NUM = 1,NUMCLU
        IF (NPULSE .GE. HITS_PER_WIRE) GOTO 300
        IPEV = LOCCLU(CLUS_NUM) -1
C
C ****  Skip cluster if too late in time
C
        IF (IPEV.GE.MAX_BIN) GOTO 200
C
C
C ****  Calculate first differences
C
        B(1) = EXPDAT(IPEV+1)-IPED
        DO  BIN = 2,LENCLU(CLUS_NUM)
          B(BIN) = EXPDAT(IPEV+BIN)-EXPDAT(IPEV+BIN-1)
        ENDDO
C
        IFIRST = 0
        ILAST = 1
        ISUML = 0
C
C MC, Look for early hit.
C
        IF (RUNTYPE.LE.0) THEN
          IF ( IPEV.EQ.0 ) THEN
            HIGHBIN = 1
            FIRSTBIN = EXPDAT(IPEV+1)
            SECONDBIN = EXPDAT(IPEV+2)
            THIRDBIN = EXPDAT(IPEV+3)
            IF (SECONDBIN.GT.FIRSTBIN) THEN
              HIGHBIN = 2
              IF (THIRDBIN.GT.SECONDBIN) THEN
                HIGHBIN = 3
              ENDIF
            ENDIF
            IF (HIGHBIN.LE.2 .AND. FIRSTBIN.GE.75) THEN
              AREA = EXPDAT(IPEV+1)+EXPDAT(IPEV+2)+EXPDAT(IPEV+3)
              IFIRST = 1
              IPEAK = 2
              ILAST = 3
              ISUML = 3
              IF (HIGHBIN.EQ.1) IPEAK = 1
              SUM = EXPDAT(IPEV+1)
              SUMX = EXPDAT(IPEV+2)
              IF (SUMX/SUM.GE.2.) SUMX = 2.*EXPDAT(IPEV+1)
              EARLY_HIT = .TRUE.
            ENDIF
          ENDIF
        ENDIF
C
C Pulse finding starts here:
C
        DO WHILE (NPULSE .LT. HITS_PER_WIRE)
          IF ( EARLY_HIT ) GOTO 90
          COUNT = 0
          I = ILAST + 1
C
C  Find rising edge
C ****  search for three successive bins above threshold
C ****  or two successive bins above threshold with sum above threshold
C
          MAX_I = MIN( ( LENCLU(CLUS_NUM)-2 ) , ( MAX_BIN-IPEV-1) )
          THR1_PED = IPED+THR1
   10     CONTINUE
          IF (I .GT. MAX_I ) GOTO 200     ! Go to next cluster
          IF (B(I) .GT. THR1) THEN
            IF (B(I-1) .GT. THR1) THEN
              IF (EXPDAT(IPEV+I) .GT. THR1_PED) THEN
                IF (B(I+1).GT.THR1 .OR. B(I)+B(I-1).GT.THR2) THEN
                  IFIRST = MAX(I-2,1)
                  IF (B(IFIRST).LE.0.) IFIRST = I-1
                  GOTO 30
                ELSE
                  I = I + 3
                ENDIF
              ELSE
                I = I + 1
              ENDIF
            ELSE
              I = I + 1
            ENDIF
          ELSE
            I = I + 2
          ENDIF
          GOTO 10
C
C ****   found pulse - find where 1st difference returns to zero
C
   30     CONTINUE
          IOFS  = 1
          SUM = 0.
          SUMX = 0.
          MAXLENGTH = MIN(IFIRST+98,LENCLU(CLUS_NUM))
          DO I = IFIRST, MAXLENGTH
            IF (B(I).LE.0. .AND. B(I+1).LE.0.) THEN
              ILAST = I
              GOTO 50
            ENDIF
          ENDDO
          ILAST = LENCLU(CLUS_NUM)
   50     CONTINUE
C
          IF (CLOSE_PEAK) THEN
C
            IF (UNIT.EQ.0) THEN
              IF (WIRE.GE.NBTSEN) THEN
                RESET_PEAK = RESET_DL
                TOO_WIDE = WIDE_DL
              ELSE
                RESET_PEAK = RESET_THETA
                TOO_WIDE = WIDE_THETA
              END IF
            ELSE
              RESET_PEAK = RESET_PHI
              TOO_WIDE = WIDE_PHI
            END IF
C
            IF ( ILAST-IFIRST .LT. TOO_WIDE ) THEN
              IPEAK = ILAST-1
            ELSE
              IPEAK = IFIRST + RESET_PEAK
              ILAST = IFIRST + RESET_PEAK + 1
            ENDIF
          ELSE
            IPEAK = ILAST-1
          ENDIF
C
C ****  require pulse height exceeding threshold
C
          IF ( (EXPDAT(IPEV+IPEAK)-EXPDAT(IPEV+IFIRST)).LT. THR2)
     &            GOTO 100
C
C ****  require that pulse is not timing pulse
C
          IF (TIMCHA .EQ. LOGCHA) THEN
            IF (FINDTP) THEN
              IF (ABS(IPEV+ILAST-SHIFTT - TMPUBN) .LE. 5 .OR.
     &           (IPEV+ILAST-SHIFTT) .GT. TMPUBN) GOTO 100
            ENDIF
          ENDIF
C
C ****  calculate integral of pulse by truncating if a second pulse arrives
C ****  immediately, or if 3 consecutive differences are less than leading
C ****  edge threshold, or truncate if these conditions are not met after
C ****  mxtail number of bins after the pulse peak.
C
          IFLG = 0
          S = ILAST
          TENPCT_PEAK = 0.10*(EXPDAT(IPEV+IPEAK)-IPED)
          IF (TENPCT_PEAK.LT. 1.0) TENPCT_PEAK=1.0
   60     CONTINUE
          S = S+1
          COUNT = COUNT+1
C
C Avoiding spike at end of Z/S data
          IF (EXPDAT(IPEV+S) .EQ. 0.) THEN
            IF (EXPDAT(IPEV+S+1) .EQ. 0.) THEN
              ISUML = S - 1   !Marking the end of the recorded data
              GOTO 70
            ENDIF
          ENDIF
C
          IF ( ( (S+2) .GE. LENCLU(CLUS_NUM) )
     &      .OR. ( (COUNT+3) .GE. PULMAX(NATUR)) ) THEN
            ISUML = MIN0(S+2,LENCLU(CLUS_NUM))
            GOTO 70
          END IF
C
          IF (B(S).GT.THR1 .AND. B(S+1).GT.THR1 .AND.
     &      (B(S+2).GT.THR1 .OR. B(S)+B(S+1).GT.THR2)) THEN
            IFLG = IBSET(IFLG,1)               ! set overlap flag
            ISUML = S-1
            GOTO 70
          END IF
C
          IF (-B(S).LT.THR1.AND.-B(S+1).LT.THR1) THEN
            IF ( (EXPDAT(IPEV+S)-IPED) .LE. TENPCT_PEAK) THEN
              IF (-B(S+2) .LT. THR1) THEN
                ISUML = S + 2
                GOTO 70
              ENDIF
            ENDIF
          END IF
C
          IF ( EXPDAT(IPEV+S)  .LE.IPED .AND.
     &         EXPDAT(IPEV+S+1).LE.IPED ) THEN
            ISUML = S
            GOTO 70
          ENDIF
C
          GOTO 60
C
C ****  compute the area
C
   70     CONTINUE
          IF ( CLOSE_PEAK ) THEN
            IF ( ISUML .LE. IPEAK ) ISUML = IPEAK + 3
          ENDIF
          AREA = 0.
          IF ( ISUML .GE. LENCLU(CLUS_NUM) )
     &      ISUML = LENCLU(CLUS_NUM)-1
C
          DO I = IFIRST,ISUML
            IF (EXPDAT(IPEV+I) .GE. 750) IFLG = IBSET(IFLG, 0)
            AREA = AREA + FLOAT(EXPDAT(IPEV+I))
          ENDDO
C
          AREA = AREA - (ISUML-IFIRST+1)*IPED
C          AREA = AREA - ((ISUML-IFIRST+1)*PEDS(1))
          IF (AREA .LT. THR3) GOTO 100
C
C Cut on the average of the absolute value of the differences over the pulse -
C sort of a measure of flatness.  Designed to get rid of pulses caused by 
C overshoot.
          IF (UNIT.EQ.1.OR.WIRE.LE.MXWIRT) THEN
            SUM_DIFF = 0.
            DO I = IFIRST+1,ISUML
              SUM_DIFF = SUM_DIFF + ABS(B(I))
            END DO
            IF (SUM_DIFF/FLOAT(ISUML-IFIRST).LE.MIN_AVE_DIFF(UNIT)) THEN
              GO TO 100
            END IF
          END IF
C
C ****  end of pulse
C
   90     CONTINUE
C
          IOFS  = 1
          SUM = 0.
          SUMX = 0.
          IF (EXPDAT(IPEV+IPEAK).GT.PULWEI_CUT(NATUR)) THEN
            JJ = 2
          ELSE
            JJ = 1
          END IF
          DO I = IFIRST, IPEAK
            SUM  = SUM  + B(I) * COEFF(IOFS,NATUR,JJ)
            SUMX = SUMX + B(I) * COEFF(IOFS,NATUR,JJ)*FLOAT(I)
            IOFS = IOFS + 1
          ENDDO
C
          TIME_BIN = SUMX/SUM + LOCCLU(CLUS_NUM) - 0.5
          IF (FINDTP) THEN
            TIME_BIN = TIME_BIN - TMPUBN
          ENDIF
          IF (TIME_BIN .GE. MAX_BIN) GOTO 200
C
C Keep precision of level 2 hit finding
C
          ITIME = INT(64*NBPBIN*(TIME_BIN))
          RTIME = FLOAT(ITIME)/64.
C
C ****  record pulse parameters
C
          NPULSE = NPULSE+1
          HITS(1,NPULSE,WIRE) = FLOAT(LOGCHA)
          HITS(2,NPULSE,WIRE) = RTIME-ETZERO       ! Drift time
          HITS(3,NPULSE,WIRE) = GAIN*AREA       ! pulse area
          HITS(4,NPULSE,WIRE) = FLOAT((ISUML-IFIRST))*NBPBIN ! pulse width
          HITS(5,NPULSE,WIRE) =
     &      GAIN*( EXPDAT(IPEV+IPEAK) - IPED )  ! peak pulse height
          IF (UNIT.LE.0) THEN
            HITS(6,NPULSE,WIRE) = ERROR_THETA  ! Theta drift time error
          ELSE
            HITS(6,NPULSE,WIRE) = ERROR_PHI    ! Phi drift time error
          END IF
C
          HITS(7,NPULSE,WIRE) = SQRT(ABS(AREA)) ! pulse area error
          HITS(8,NPULSE,WIRE) = FLAG            ! flag
C
C ****  Calculate risetime,falltime, overshoot if requested
C
          IF (DO_RISE .OR. (PSHLEV.EQ.2)) THEN
            CALL FDRISE_SIGMA( IFIRST,IPEAK,ISUML,B,
     &                         RISETIME,FALLTIME,WIDTH )
            HITS(9,NPULSE,WIRE) = RISETIME
            HITS(10,NPULSE,WIRE) = FALLTIME
            HITS(11,NPULSE,WIRE) = WIDTH
          END IF

C ****  Perform pulse shape subtraction to find overlapping hits, if requested.
C
C          IF (PSHLEV.EQ.2) THEN
C            RISETIME = RISETIME/NBPBIN
C            LEADING_EDGE =SUMX/SUM
CC
C            CALL FPULSHP_SUBTR(EXPDAT,NPULSE,UNIT,WIRE,IFIRST,IPEAK,
C     &           ISUML,IPEV,AREA,LEADING_EDGE,PEDS(1),RISETIME,
C     &           PUL_RETRY)
CC
C            IF (PUL_RETRY) THEN
C              IF (REDO_AREA) HITS(3,NPULSE,WIRE) = GAIN*AREA  ! PA recalculated
C              IF (NPULSE .LT. HITS_PER_WIRE) THEN
C                ISUML=0
C                IFIRST=IPEAK-1
C                ILAST=IFIRST+1
C                IF( (IFIRST+IPEV).LE.1 ) THEN
C                  B(1) = EXPDAT(0)-PEDS(1)
C                  IFIRST = 1-IPEV
C                  IF ( IFIRST.LT.2 ) IFIRST=2
C                ENDIF
C                DO BIN=IFIRST,LENCLU(CLUS_NUM)
C                  B(BIN)=EXPDAT(IPEV+BIN)-EXPDAT(IPEV+BIN-1)
C                ENDDO
C              ENDIF
C            ENDIF
C          ENDIF
  100     CONTINUE              ! Look for next pulse in cluster
          EARLY_HIT = .FALSE.
C
        ENDDO
        GOTO 300
C
  200 CONTINUE                  ! Look at next cluster
C
C ****  End of work. All pulses found.
C----------------------------------------------------------------------
  300 CONTINUE
      IF ( .NOT.PRODFL ) THEN
        IF (NPULSE .GE. HITS_PER_WIRE) THEN
          CALL ERRMSG('FDC-Too-Many-Hits-Per-Wire','FDPULS',
     &    ' number of FDC hits found on wire at maximum, HITS_PER_WIRE',
     &    'W')
        ENDIF
      ENDIF
  999 CONTINUE
      RETURN
      END
