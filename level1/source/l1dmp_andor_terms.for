      SUBROUTINE L1DMP_ANDOR_TERMS(LUN, SIMULATION)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print the state of each Andor Term.
C-
C-   Inputs  : LUN      The unit number to write the output to.
C-             SIMULATION Whether simulation was performed on this event. 
C-                        If there was, then extra information is available.
C-   Outputs : none
C-   Controls: none
C-
C-   Created  30-JAN-1992   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$INC:LEVEL1_LOOKUP.INC'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1SIM_CONTROL.PARAMS'
      INCLUDE 'D0$INC:L1SIM_CONTROL.INC'
      INCLUDE 'D0$INC:L1FW_ANDOR_AND_MISC.INC'
      INCLUDE 'D0$INC:L1C_GLOBAL_ENERGY_THRESHOLDS.INC'
      INCLUDE 'D0$INC:L1C_REFSET_AND_COUNT_THRESHOLDS.INC'
C
      INTEGER LUN
      LOGICAL SIMULATION
C
      LOGICAL OK
      CHARACTER*32 RCPKEY
      INTEGER TERM_NUM
      INTEGER ISTAT
      INTEGER THRESH, QUANT, REF_NUM, CNT
      CHARACTER*5 TF
      INTEGER THRESH_VALUE
C
      CHARACTER*32 ANDOR_TERM_NAME(ANDOR_NUM_MIN:ANDOR_NUM_MAX)
C
      INTEGER THRESH_TO_QUANT(GL_EMET_THRTYP:GL_TOTL2_THRTYP)
      DATA THRESH_TO_QUANT / EM_ET_QUANT, EM_L2_QUANT, HD_ET_QUANT,
     &                       HD_L2_QUANT, TOT_ET_QUANT, TOT_L2_QUANT /
C
      WRITE(LUN,*)
      WRITE(LUN,*)
      WRITE(LUN,*) 'Andor Term Listing by Name'
      WRITE(LUN,*) '=========================='
      WRITE(LUN,*)
C
C       Put the term names in order by term number
C
      DO TERM_NUM = ANDOR_NUM_MIN, ANDOR_NUM_MAX
        ANDOR_TERM_NAME(TERM_NUM) = ' '
      END DO
C
      CALL L1DMP_GET_NEXT_ANDOR_KEY_INIT()
C
C       Go through each defined Andor Term
C
      OK = .TRUE.
      DO WHILE (OK .EQV. .TRUE.)
        RCPKEY = ' '
        CALL L1DMP_GET_NEXT_ANDOR_KEY(RCPKEY, TERM_NUM, OK)
        IF ((OK .EQV. .TRUE.) .AND. (TERM_NUM .GE. ANDOR_NUM_MIN)
     &      .AND. (TERM_NUM .LE. ANDOR_NUM_MAX)) THEN
          ANDOR_TERM_NAME(TERM_NUM) = RCPKEY
        END IF
      END DO
C
C       Print out the term names
C
  300 FORMAT(' ', A, T35, '#', I3, X, A5)
  310 FORMAT(' ', A, T35, '#', I3, X, A5, ' (.GE.', F8.2, ' GeV)')
  320 FORMAT(' ', A, T35, '#', I3, X, A5, ' (.GE.', I5, ' towers)')
C
C
      DO TERM_NUM = ANDOR_NUM_MIN, ANDOR_NUM_MAX
        RCPKEY = ANDOR_TERM_NAME(TERM_NUM)
        IF (RCPKEY .NE. ' ') THEN
C
          IF (ANDOR_TERM(TERM_NUM) .EQV. .TRUE.) THEN
            TF = 'TRUE '
          ELSE
            TF = 'FALSE'
          ENDIF
C
          IF ((SIMULATION .EQV. .FALSE.) .OR.
     &        (LV1_ANDOR_TERM_TYPE(TERM_NUM, AO_THRSH_TYPE_INDEX) 
     &         .EQ. 0)) THEN
            WRITE(LUN,300,IOSTAT=ISTAT) RCPKEY, TERM_NUM, TF
C
          ELSEIF (LV1_ANDOR_TERM_TYPE(TERM_NUM, AO_THRSH_TYPE_INDEX) 
     &      .EQ. AO_THRSH_MPT) THEN
            THRESH = LV1_ANDOR_TERM_TYPE(TERM_NUM, AO_THRSH_SUB1_INDEX)
            THRESH_VALUE = TOTAL_MPT_REF(THRESH)
            IF (TOTAL_MPT_REF(THRESH) .NE. 0) THEN
              WRITE(LUN,310,IOSTAT=ISTAT) RCPKEY, TERM_NUM, TF, 
     &          TOTAL_MPT_REF(THRESH) * GLOBAL_ENERGY_SCALE(PX_QUANT)
            ELSE
              WRITE(LUN,300,IOSTAT=ISTAT) RCPKEY, TERM_NUM, TF
            ENDIF
C
          ELSEIF (LV1_ANDOR_TERM_TYPE(TERM_NUM, AO_THRSH_TYPE_INDEX)
     &      .EQ. AO_THRSH_GSUM) THEN
            QUANT = LV1_ANDOR_TERM_TYPE(TERM_NUM, AO_THRSH_SUB1_INDEX)
            THRESH = LV1_ANDOR_TERM_TYPE(TERM_NUM, AO_THRSH_SUB2_INDEX)
            THRESH_VALUE = GLOBAL_ENERGY_REF(MOD(THRESH,4)+1, 
     &                                       THRESH/4+1, QUANT)
            IF (THRESH_VALUE .NE. 0) THEN
              WRITE(LUN,310,IOSTAT=ISTAT) RCPKEY, TERM_NUM, TF,
     &          FLOAT(THRESH_VALUE 
     &                - TREE_OFFSET(THRESH_TO_QUANT(QUANT)) )
     &          * GLOBAL_ENERGY_SCALE(THRESH_TO_QUANT(QUANT))
            ELSE
              WRITE(LUN,300,IOSTAT=ISTAT) RCPKEY, TERM_NUM, TF
            ENDIF
C
          ELSEIF (LV1_ANDOR_TERM_TYPE(TERM_NUM, AO_THRSH_TYPE_INDEX) 
     &            .EQ. AO_THRSH_CNT) THEN
            REF_NUM = LV1_ANDOR_TERM_TYPE(TERM_NUM, AO_THRSH_SUB1_INDEX)
            CNT = LV1_ANDOR_TERM_TYPE(TERM_NUM, AO_THRSH_SUB2_INDEX)
            THRESH_VALUE = HOT_COUNT_REF(CNT, REF_NUM)
            IF (THRESH_VALUE .NE. 0) THEN
              WRITE(LUN,320,IOSTAT=ISTAT) RCPKEY, TERM_NUM, TF,
     &          THRESH_VALUE
            ELSE
              WRITE(LUN,300,IOSTAT=ISTAT) RCPKEY, TERM_NUM, TF
            ENDIF
C
          ELSE
            WRITE(LUN,300,IOSTAT=ISTAT) RCPKEY, TERM_NUM, TF
          ENDIF
        ENDIF
      END DO
C
C----------------------------------------------------------------------
  999 RETURN
      END
