      SUBROUTINE ZEPULS(LCHN,PEDS,NPULSE,HITS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : search for hits in an FADC trace
C-
C-   Inputs  : LCHN = logical channel address
C-             PEDS(2) = pedestal and sigma
C-
C-   Outputs : NPULSE     = Number of found pulses
C-             HITS = array of pulse location and peak height
C-
C-   Created   16-OCT-1990   Susan K. Blessing   based on FDPULS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS/LIST'
C
      INTEGER MAXPUL
      INTEGER NPULSE,ISUML,J
      INTEGER I,ILAST,IFIRST,IPEAK,S
      INTEGER EXPDAT(0:LFADC-1)
      INTEGER IER, IOFS
      INTEGER PULMAX
      INTEGER COUNT, TMPADC
      INTEGER IPEV, IPREAD, LENCLU, LOCCLU
      INTEGER LCHN
C
      INTEGER MAP(0:255)
C
      REAL B(LFADC), PULWEI
      REAL SUM,SUMX,COEFF(100)
      REAL THR1, THR2, THR3
      REAL THR1MU,THR2MU,THR3MU
      REAL HITS(2,10)
      REAL PEDS(2)
C
      LOGICAL FIRST
C
      SAVE FIRST,THR1MU,THR2MU,PULWEI,PULMAX,MAP,COEFF
C
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL EZPICK('CD_ELECTRONICS_RCP')
        CALL EZGET('THR1MU',THR1MU,IER)
        CALL EZGET('THR2MU',THR2MU,IER)
        CALL EZGET('THR3MU',THR3MU,IER)
        CALL EZGET('PULWEI',PULWEI,IER)
        CALL EZGET('MAXPUL',MAXPUL,IER)
        CALL EZGET('PULMAX',PULMAX,IER)
        CALL EZGET('MAP(1)',MAP(0),IER)
        CALL VZERO( EXPDAT(0), LFADC )
        CALL VZERO( B, LFADC )
        CALL EZRSET
        COEFF(1) = 1.
        DO I = 2, 100
          COEFF(I) = COEFF(I-1) * PULWEI
        END DO
        FIRST = .FALSE.
      ENDIF
      NPULSE = 0
C
      CALL VZERO( EXPDAT(0), IPREAD )
      CALL VZERO( B, IPREAD )
C Unpack data
      CALL ZDEXPD(0,LCHN,EXPDAT)
C
C Loop over clusters in channel data
C
      IPREAD = 0
  201 LENCLU = EXPDAT(IPREAD)
      IF( LENCLU .EQ. 0 ) GOTO 909
      LOCCLU = EXPDAT(IPREAD+1)
      IPEV=IPREAD + 1
      IPREAD=IPREAD+LENCLU+2
C
C Bilinear conversion
C
      DO 101 I=1, LENCLU
        TMPADC=NINT(FLOAT(EXPDAT(IPEV+I))-PEDS(1))
        TMPADC = MAP(TMPADC)
        EXPDAT(IPEV+I)=INT(FLOAT(TMPADC)+PEDS(1))
  101 CONTINUE
C
C Calculate first differences
C
      B(1)=0.
      DO 301 I=2,LENCLU
        B(I)=EXPDAT(IPEV+I)-EXPDAT(IPEV+I-1)
  301 CONTINUE
C
C Thresholds
C
      THR1 = THR1MU * PEDS(2)
      IF (THR1 .LT. 0.75) THR1 = 0.75 !Default for Zero-supp data
      THR2 = THR2MU * PEDS(2)
      IF (THR2 .LT. 2.0) THR2 = 2.0   !Default for Zero-supp data
      THR3 = THR3MU * PEDS(2)
      IF (THR3 .LT. 10.0) THR3 = 10.0
C
C Search for three successive bins above threshold
C or two successive bins above threshold with sum above threshold
C
      IFIRST = 0
      ILAST = 1
      ISUML = 0
C
   10 CONTINUE
      COUNT = 0
      I = ILAST + 1
C
   11 IF (I .GT. LENCLU-2) GOTO 201     ! Go to next cluster
C
      IF (B(I) .GE. THR1) THEN
        IF (B(I-1) .GE. THR1) THEN
          IF (EXPDAT(IPEV+I) .GE. PEDS(1)+THR1) THEN
            IF (B(I+1).GE.THR1 .OR. B(I)+B(I-1).GE.THR2) THEN
              IFIRST=I-2
              IF(B(IFIRST).EQ.0.) IFIRST=I-1
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
      GO TO 11
C
C Found pulse - find where 1st difference returns to zero
C
   30 ILAST = IFIRST
      IOFS  = 1
      SUM = 0.
      SUMX = 0.
      DO 40 I = IFIRST, LENCLU
        IF (I-IFIRST.GT.98) GO TO 40   ! PULSE TOO LONG
        IF (B(I).LE.0. .AND. B(I+1).LE.0.) THEN
          ILAST = I
          ISUML = I
          GO TO 50
        ENDIF
        SUM  = SUM  + B(I) * COEFF(IOFS)
        SUMX = SUMX + B(I) * COEFF(IOFS)*FLOAT(I)
        IOFS = IOFS + 1
   40 CONTINUE
      ILAST = LENCLU
   50 CONTINUE
C
C ****  require pulse height exceeding threshold
C
      IPEAK = ILAST-1
      IF (FLOAT(EXPDAT(IPEV+IPEAK)-EXPDAT(IPEV+IFIRST-1)) .LE. THR2)
     &                                                      GO TO 10
C
C ****  calculate integral of pulse by truncating if a second pulse arrives
C ****  immediately, or if 3 consecutive differences are less than leading
C ****  edge threshold, or truncate if these conditions are not met after
C ****  mxtail number of bins after the pulse peak.
C
      S = ISUML
   60 CONTINUE
      S = S+1
      COUNT = COUNT+1
C
      IF (EXPDAT(IPEV+S) .EQ. 0.) THEN
        IF (EXPDAT(IPEV+S+1) .EQ. 0.) THEN
          ISUML = S - 1   !Marking the end of the recorded data
          GOTO 90
        ENDIF
      ENDIF
C
      IF ((S+2).GE.LENCLU.OR.(COUNT+3).GE.PULMAX) THEN
        ISUML = MIN0(S+2,LENCLU)
        GOTO 90
      END IF
C
      IF (B(S).GE.THR1 .AND. B(S+1).GE.THR1 .AND.
     &   (B(S+2).GE.THR1 .OR. B(S)+B(S+1).GE.THR2)) THEN
        ISUML = S-1
        GOTO 90
      END IF
C
      IF (-B(S).LE.THR1.AND.-B(S+1).LE.THR1) THEN
        IF (EXPDAT(IPEV+S) .LE. (PEDS(1)+THR1)) THEN
          IF (-B(S+2) .LE. THR1) THEN
            ISUML = S + 2
            GOTO 90
          ENDIF
        ENDIF
      END IF
C
      IF (EXPDAT(IPEV+S).LE.PEDS(1).AND.
     &  EXPDAT(IPEV+S+1).LE.PEDS(1)) THEN
        ISUML = S
        GOTO 90
      ENDIF
C
      GOTO 60
C
C ****  end of pulse
C
   90 CONTINUE
C
C ****  record pulse parameters
C
      IF (SUM .LE. 0.0) GOTO 10
      NPULSE = NPULSE+1
C Hit location
      HITS(1,NPULSE) = (SUMX/SUM + LOCCLU - .5)
C Peak pulse height
      HITS(2,NPULSE) = EXPDAT(IPEV+IPEAK)
C
C ****  Look for more pulses in cluster
C
      IF (NPULSE .LT. MAXPUL) GOTO 10
C
C ****  End of work. All pulses found
C
  909 CONTINUE
C
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
