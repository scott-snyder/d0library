      SUBROUTINE TRGR_FADC_HITS(CHAN,HIT_LOC,HIT_PH,NHIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find hit location and pulse area for hits
C-    in channel CHAN of TRGR block FADCs
C-
C-   Inputs  : CHAN = channel number
C-   Outputs : HIT_LOC = hit locations
C-             HIT_PH  = hit areas
C-             NHIT    = number of hits
C-   Controls:
C-
C-   Created   3-DEC-1992   Susan K. Blessing - based on FDTSUB_D0.
C-   Updated  26-JAN-1994   Jeffrey Bantly  make Run 1a and Run 1b compatible, 
C-                                          change check for bad data
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C INPUT
      INTEGER CHAN
C
C OUTPUT
      INTEGER NHIT
      REAL HIT_LOC(*),HIT_PH(*)
C<<
C LOCAL
      INTEGER I,COUNT
      INTEGER HIT
      INTEGER MAX_HITS
      PARAMETER (MAX_HITS = 10)
      INTEGER NFADC
      PARAMETER (NFADC = 512)
      INTEGER DATA(0:NFADC-1),DIFF(512)
      INTEGER ILAST,IFIRST,ISTART,IPEAK
      INTEGER HIT_BEG(MAX_HITS),HIT_END(MAX_HITS)
C
      REAL SUM,SUMX
      REAL THR1, THR2, THR3
      REAL DIFF_MAX
      REAL PED
C
      LOGICAL IN_PULSE
      LOGICAL FOUND
C
      SAVE THR1,THR2,THR3
C
      DATA THR1 /3./
      DATA THR2 /10./
      DATA THR3 /50./
C
C----------------------------------------------------------------------
C
C Get data
      CALL TRGR_FADC(CHAN,DATA)
C
      IF ( DATA(0).EQ.-1 ) THEN
        NHIT = -1
        GOTO 999
      ENDIF
C
C Find first differences
      DO I = 1, NFADC-1
        IF (DATA(I-1).EQ.0) THEN
          DIFF(I) = 0
        ELSE
          DIFF(I) = DATA(I) - DATA(I-1)
        ENDIF
      END DO
C
      NHIT = 0
      ISTART = 1
      DO WHILE (NHIT.LT.MAX_HITS.AND.ISTART.LT.NFADC-2)
C
        FOUND = .FALSE.
        DIFF_MAX = 0
        SUM  = 0.
        SUMX = 0.
        IN_PULSE = .FALSE.
        DO I = ISTART,NFADC-2
          IF ( (.NOT. IN_PULSE)
     &      .AND. ( DIFF(I) .GT. THR1 )
     &      .AND. ( DIFF(I+1) .GT. THR1 ) ) THEN
            IFIRST = I
            IN_PULSE = .TRUE.
          ENDIF
          IF ( IN_PULSE ) THEN
            IF ( DIFF(I) .GT. DIFF_MAX ) THEN
              DIFF_MAX = DIFF(I)
            ENDIF
            IF (  DIFF(I) .LT. THR1 ) THEN
              IPEAK = I - 1
              IF ( (DIFF_MAX .GT. THR2)
     &          .AND. ( SUM .GT. THR3 )  ) THEN
                FOUND = .TRUE.
                GOTO 100
              ELSE
                IN_PULSE = .FALSE.
                DIFF_MAX = 0
                SUM  = 0.
                SUMX = 0.
              ENDIF
            ELSE
              SUM  = SUM  + DIFF(I)
              SUMX = SUMX + DIFF(I) * I
            ENDIF
          ENDIF
        ENDDO
C
  100   CONTINUE
C
        IF (FOUND) THEN
          NHIT = NHIT + 1
          HIT_LOC(NHIT) = (SUMX/SUM - .5)
          HIT_BEG(NHIT) = IFIRST
        ELSE
          GO TO 500
        END IF
C
C Look for end of pulse
C
        FOUND = .FALSE.
        IN_PULSE = .FALSE.
        DO I = IPEAK,NFADC-2
          IF ( (.NOT. IN_PULSE)
     &      .AND. ( -DIFF(I) .GT. THR1 )
     &      .AND. ( -DIFF(I+1) .GT. THR1 ) ) THEN
            IN_PULSE = .TRUE.
          ENDIF
          IF ( IN_PULSE ) THEN
            IF ( -DIFF(I) .LT. THR1 ) THEN
              ILAST = I-1
              IF ( SUM .GT. THR3 ) THEN
                FOUND = .TRUE.
                GOTO 200
              ELSE
                IN_PULSE = .FALSE.
              ENDIF
            ENDIF
          ENDIF
        ENDDO
C
  200   CONTINUE
C
        IF (FOUND) THEN
          HIT_END(NHIT) = ILAST
        ELSE
          HIT_END(NHIT) = NFADC-2
        END IF
C
        ISTART = ILAST + 1
C
      END DO
C
  500 CONTINUE
C
C Find pedestal - look before first hit
      IF (NHIT.GE.1) THEN
        SUM = 0
        COUNT = 0
        DO I = 0, HIT_BEG(1)-2
          IF (DATA(I).GT.0) THEN
            SUM = SUM + DATA(I)
            COUNT = COUNT + 1
          END IF
        END DO
C
        IF (COUNT.EQ.0) THEN
          DO I = NFADC-1, HIT_END(NHIT)+25
            IF (DATA(I).GT.0) THEN
              SUM = SUM + DATA(I)
              COUNT = COUNT + 1
            END IF
          END DO
        END IF
C
        IF (COUNT.GT.0) THEN
          PED = SUM/FLOAT(COUNT)
        ELSE
          PED = 9.
        END IF
C
C Calculate pulse area
        DO HIT = 1, NHIT
          HIT_PH(HIT) = 0.
          DO I = HIT_BEG(HIT), HIT_END(HIT)
            HIT_PH(HIT) = HIT_PH(HIT) + DATA(I) - PED
          END DO
        END DO
      END IF
C
C----------------------------------------------------------------------
C
  999 RETURN
      END
