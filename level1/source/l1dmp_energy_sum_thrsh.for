      SUBROUTINE L1DMP_ENERGY_SUM_THRSH(LUN, QUANT, TITLE, THRESH,
     &  THRESH_MAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print the definition of a set of Global Energy Sum
C-      Thresholds.
C-
C-   Inputs  : LUN      The unit number to output to
C-             QUANT    The Lookup Quantity to print the definition for
C-             TITLE    The title for this set of definitions
C-             THRESH   The thresholds to print
C-             THRESH_MAX       The number of entries in the THRESH array
C-
C-   Outputs : File output
C-
C-   Controls: none
C-
C-   Created  11-NOV-1991   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$INC:LEVEL1_LOOKUP.INC'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:L1FW_ANDOR_AND_MISC.INC'
C
      INTEGER TRULEN
      EXTERNAL TRULEN
C
      INTEGER LUN, QUANT, THRESH_MAX
      CHARACTER*(*) TITLE
      INTEGER THRESH(0:THRESH_MAX)
C
      INTEGER THRESH_NUM, ANDOR_TERM_NUM, TERM_NAME_LEN, TERM_NUM
      INTEGER OFFSET
      REAL SCALE
      INTEGER ISTAT
      INTEGER INT_IN
      REAL    REAL_OUT
      EQUIVALENCE (INT_IN, REAL_OUT)
      CHARACTER*32 TERM_NAME
      LOGICAL FIRST_LINE
      LOGICAL OK
      INTEGER THRESH_TO_QUANT(GL_EMET_THRTYP:GL_TOTL2_THRTYP)
      DATA THRESH_TO_QUANT / EM_ET_QUANT, EM_L2_QUANT, HD_ET_QUANT,
     &  HD_L2_QUANT, TOT_ET_QUANT, TOT_L2_QUANT /
C
      WRITE (LUN,*)
      WRITE (LUN,*) TITLE
      FIRST_LINE = .TRUE.
      DO THRESH_NUM = 0, THRESH_MAX
        DO TERM_NUM = ANDOR_NUM_MIN, ANDOR_NUM_MAX
          IF ((LV1_ANDOR_TERM_TYPE(TERM_NUM, AO_THRSH_TYPE_INDEX)
     &      .EQ. AO_THRSH_GSUM) .AND.
     &      (LV1_ANDOR_TERM_TYPE(TERM_NUM, AO_THRSH_SUB1_INDEX)
     &       .EQ. QUANT) .AND.
     &      (LV1_ANDOR_TERM_TYPE(TERM_NUM, AO_THRSH_SUB2_INDEX)
     &       .EQ. THRESH_NUM)) THEN
            GOTO 100
          ELSEIF ((LV1_ANDOR_TERM_TYPE(TERM_NUM,
     &       AO_THRSH_TYPE_INDEX) .EQ. AO_THRSH_MPT) .AND.
     &      (QUANT .EQ. AO_THRSH_MPT) .AND.
     &      (LV1_ANDOR_TERM_TYPE(TERM_NUM, AO_THRSH_SUB1_INDEX)
     &       .EQ. THRESH_NUM+1)) THEN
            GOTO 100
          ENDIF
        END DO
C       If execution reaches here, the threshold has not been defined
C       Move on to the next threshold number
        GOTO 200
C
C       Print the threshold information
  100   CONTINUE
C       Find the definition of the Andor Term name
        CALL L1DMP_GET_NEXT_ANDOR_KEY_INIT()
        OK = .TRUE.
        DO WHILE (OK .EQV. .TRUE.)
          CALL L1DMP_GET_NEXT_ANDOR_KEY(TERM_NAME, ANDOR_TERM_NUM, OK)
          IF (OK .EQV. .FALSE.) THEN
            TERM_NAME = 'unknown'
          ELSEIF (ANDOR_TERM_NUM .EQ. TERM_NUM) THEN
            OK = .FALSE.
          ENDIF
        END DO
C
        IF (QUANT .NE. AO_THRSH_MPT) THEN
          OFFSET = TREE_OFFSET(THRESH_TO_QUANT(QUANT))
          SCALE = GLOBAL_ENERGY_SCALE(THRESH_TO_QUANT(QUANT))
        ELSE
          OFFSET = 0
          SCALE = GLOBAL_ENERGY_SCALE(PX_QUANT)
        ENDIF
C
        IF (THRESH(THRESH_NUM) .GT. 0) THEN
          IF (QUANT .NE. AO_THRSH_MPT) THEN
            WRITE (LUN,110,IOSTAT=ISTAT) TERM_NUM, THRESH_NUM,
     &        THRESH(THRESH_NUM), (THRESH(THRESH_NUM) - OFFSET) * SCALE,
     &        TERM_NAME(1:TRULEN(TERM_NAME))
          ELSE
            INT_IN = THRESH(THRESH_NUM)
            WRITE (LUN,110,IOSTAT=ISTAT) TERM_NUM, THRESH_NUM,
     &        IFIX(REAL_OUT), REAL_OUT * SCALE,
     &        TERM_NAME(1:TRULEN(TERM_NAME))
          ENDIF
        ELSE
          WRITE (LUN,120,IOSTAT=ISTAT) TERM_NUM, THRESH_NUM,
     &      TERM_NAME(1:TRULEN(TERM_NAME))
        ENDIF
        FIRST_LINE = .FALSE.
C
  110   FORMAT('   Andor Term #', I3, ' : Energy Threshold #', I2,
     &    ' :', I7, ' ADC Count =', F6.2, ' GeV  (',A,')' )
  120   FORMAT('   Andor Term #', I3, ' : Energy Threshold #', I2,
     &    ' : unused  (', A, ')' )
  130   FORMAT('   Andor Term #', I3, ' : Energy Threshold #', I2,
     &    ' :', 7X, '            ', F6.2, ' GeV  (',A,')' )
C
  200   CONTINUE
      END DO
C
      IF (FIRST_LINE .EQV. .TRUE.) THEN
        WRITE (LUN,*) '   No existing thresholds'
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
