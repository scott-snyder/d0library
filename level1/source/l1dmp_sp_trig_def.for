      SUBROUTINE L1DMP_SP_TRIG_DEF (LUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prints out the Specific Trigger and threshold 
C-      definitions.
C-
C-   Inputs  : LUN : Logical Unit Number to be used as output.
C-
C-   Outputs : File output.
C-   Controls: None.
C-
C-   Created  23-APR-1990   Sylvain Tisserant (MSU)
C-   Updated  18-OCT-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                            - Changed order of delcaration statements to pass
C-                              D0FLAVOR
C-   Updated   5-NOV-1991   Philippe Laurens, Steven Klocek   
C-                      - Dumps programming of Prescalers
C-                      - No longer dumps Specific Trigger Exists and
C-                        Geographic Sections exist
C-   Updated  14-NOV-1991   Philippe Laurens, Steven Klocek  
C-                      - Rewritten to print out the information in a different
C-                        format. 
C-                      - The data in the output file is now organized by
C-                        specific trigger.
C-   Updated  19-NOV-1991   Philippe Laurens, Steven Klocek  
C-                      - Reordered the information in the output. 
C-   Updated   3-JAN-1992   Philippe Laurens, Steven Klocek  
C-                      - Fixed problem with .GE. being printed with
C-                        non-threshold Andor Terms.
C-   Updated   7-FEB-1992   Philippe Laurens, Steven Klocek  
C-                      - Added listing of Level 1.5 terms used by each
C-                          specific triggers. Added listing of all Level 1.5
C-                          terms.
C-   Updated  28-SEP-1992   Philippe Laurens, Steven Klocek  
C-                      - Format for 80 Character output (FORTRAN breaks the
C-                        line at 80 characters anyway, so it looks nicer).
C-   Updated   9-JUL-1993   Philippe Laurens - MSU L1 Trigger  
C-                      - Add Dump of Lareg Tile Reference Sets 
C-                      - and dump of Large Tile programming vs SpTrg 
C-   Updated   4-MAR-1994   Philippe Laurens - MSU L1 Trigger  
C-                      Add switch PRESCALER_USE_EVENT_DATA 
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
      INCLUDE 'D0$PARAMS:L15_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:L15_FRAMEWORK.INC'
      INCLUDE 'D0$INC:L1C_GLOBAL_ENERGY_THRESHOLDS.INC'
      INCLUDE 'D0$INC:L1C_REFSET_AND_COUNT_THRESHOLDS.INC'
C
      INTEGER TRULEN
      EXTERNAL TRULEN
C
      INTEGER LINE_WIDTH
      PARAMETER (LINE_WIDTH = 130)
      INTEGER RCPOK
      PARAMETER (RCPOK = 0)
      INTEGER  LUN
      INTEGER THRESH, REF_NUM, QUANT, SPEC_TRIG, ANDOR, GEO, CNT
      INTEGER TERM_NUM
      INTEGER L15_TERM_NUM, CURID, IER, ID, KEYLEN
      INTEGER ISTAT
      LOGICAL FIRST_LINE
      LOGICAL IS_THRESH
      INTEGER BASE_LENGTH
      LOGICAL OK
      INTEGER CUR_POS
      CHARACTER*80 BUF
      CHARACTER*132 LBUF
      CHARACTER*32 RCPKEY
      CHARACTER*32 L15_TERM_RCPKEY(L15_TERM_NUM_MIN:L15_TERM_NUM_MAX)
      INTEGER THRESH_TO_QUANT(GL_EMET_THRTYP:GL_TOTL2_THRTYP)
      DATA THRESH_TO_QUANT / EM_ET_QUANT, EM_L2_QUANT, HD_ET_QUANT,
     &  HD_L2_QUANT, TOT_ET_QUANT, TOT_L2_QUANT /
C
      WRITE (LUN,20)
   10 FORMAT( ' ', 130('-'))
   20 FORMAT( '1' )
      WRITE (LUN,10) 
      WRITE (LUN,*) 'CALORIMETER TRIGGER DEFINITION'
      WRITE (LUN,*) '------------------------------'
      WRITE (LUN,*)
      WRITE (LUN,*) 'Programming of each Specific Trigger'
      WRITE (LUN,*) '------------------------------------'
      WRITE (LUN,*)
C       
C       Is Prescaling enabled
      IF (APPLY_PRESCALER .EQV. .TRUE.) THEN
        IF ( PRESCALER_USE_EVENT_DATA .EQV. .TRUE. ) THEN
          WRITE (LUN,*) '  Prescaling mode was enabled,'
          WRITE (LUN,*) '   and used event data in final decision.'
        ELSE
          WRITE (LUN,*) '  Prescaling mode was enabled,'
          WRITE (LUN,*) '   and used programmed ratios (random 1 of n).'
        END IF
      ELSE
        WRITE (LUN,*) '  *** WARNING *** ' // 
     &    'Prescaling ratios were ignored.'
      ENDIF
      WRITE (LUN,*)
C
C       Store the names of the Level 1.5 Terms
C
      DO TERM_NUM = L15_TERM_NUM_MIN, L15_TERM_NUM_MAX
        L15_TERM_RCPKEY(TERM_NUM) = ' '
      END DO
C
      CALL L1UTIL_PICK_L15RESOURCE_RCP
      CURID = 1
      DO WHILE( .TRUE.) 
        ID = 0
        CALL EZGNXT(' ', CURID, ID)
        IF (ID .EQ. 0) GOTO 180 
C
        CALL EZGET1(ID, 1, 1, 1, TERM_NUM, IER)
        IF (IER .NE. RCPOK) THEN
          CALL EZGET_ERROR_TEXT(IER, BUF)
          CALL ERRMSG(' EZGET1', 'L1DMP_SP_TRIG_DEF', BUF, 'F')
          GOTO 180
        ENDIF
        IF ((TERM_NUM .GE. L15_TERM_NUM_MIN) 
     &    .AND. (TERM_NUM .LE. L15_TERM_NUM_MAX)) THEN
C
          CALL EZGETN(ID, RCPKEY, KEYLEN)
          CALL EZERR(IER)
          IF (IER .NE. RCPOK) THEN
            CALL EZGET_ERROR_TEXT(IER, BUF)
            CALL ERRMSG(' EZGETN', 'L1DMP_SP_TRIG_DEF', BUF, 'F')
            GOTO 180
          ENDIF
C
          L15_TERM_RCPKEY(TERM_NUM) = RCPKEY(1:KEYLEN)
C
        ENDIF
      END DO
      CALL EZRSET()
  180 CONTINUE
C
C       Print out the definition of each Specific Trigger
      DO SPEC_TRIG = TRG_NUM_MIN, TRG_NUM_MAX
        DO ANDOR = ANDOR_NUM_MIN, ANDOR_NUM_MAX
          IF (SPECTRIG_ANDOR_ALLOC(ANDOR, SPEC_TRIG) .EQV. .TRUE.) THEN
            GOTO 100
          ENDIF
        END DO
C       If no Andor Terms are allocated to this Specific Trigger, skip to the
C       next one
        WRITE (LUN,110,IOSTAT=ISTAT) SPEC_TRIG
        WRITE (LUN,*) '  Unused'
        WRITE (LUN,*)
        GOTO 170
C
  100   CONTINUE
        WRITE (LUN,110,IOSTAT=ISTAT) SPEC_TRIG
  110   FORMAT('   Specific Trigger #', I2)
        WRITE (LUN,120,IOSTAT=ISTAT) ST_PRESCALER(SPEC_TRIG)
  120   FORMAT('     Prescale Ratio =', I7)
C
C       Start digitize signals for each specific trigger
        OK = .FALSE.
        LBUF = '    Start Digitize Geographic Sections #'
        CUR_POS = TRULEN(LBUF) + 1
        DO GEO = GEO_NUM_MIN, GEO_NUM_MAX
          IF (ST_STARTDGT(GEO, SPEC_TRIG) .EQV. .TRUE.) THEN
            OK = .TRUE.
            WRITE (LBUF(CUR_POS:CUR_POS+2),130,IOSTAT=ISTAT) GEO
            CUR_POS = CUR_POS + 3
            IF (CUR_POS .GE. LINE_WIDTH -3) THEN
              WRITE(LUN,*) LBUF(1:TRULEN(LBUF))
              CUR_POS = 42
              LBUF = ' '
            ENDIF
          ENDIF
        END DO
        IF (OK .EQV. .FALSE.) THEN
          WRITE (LUN,*) '    Start digitize Geographic Sections : None'
        ELSEIF (BUF .NE. ' ') THEN
          WRITE (LUN,*) LBUF(1:TRULEN(LBUF))
        ENDIF
  130   FORMAT(' ', I2)
C
C       Andor Terms used by this Spec Trig
        WRITE(LUN, *) '    Andor Terms'
        DO ANDOR = ANDOR_NUM_MIN, ANDOR_NUM_MAX
          IF (SPECTRIG_ANDOR_ALLOC(ANDOR, SPEC_TRIG) .EQV. .TRUE.) THEN
            CALL L1DMP_GET_NEXT_ANDOR_KEY_INIT()
            OK = .TRUE.
            DO WHILE (OK .EQV. .TRUE.) 
              RCPKEY = ' '
              CALL L1DMP_GET_NEXT_ANDOR_KEY(RCPKEY, TERM_NUM, OK)
              IF (OK .EQV. .FALSE.) RCPKEY = 'UNKNOWN'
              IF (TERM_NUM .EQ. ANDOR) OK = .FALSE.
            END DO
C
C       If not a CalTrig Andor
            IS_THRESH = .FALSE.
            IF (LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_TYPE_INDEX) 
     &      .EQ. 0) THEN
              BUF = ' '
C
C       CalTrig Andor Term
            ELSE
  150         FORMAT(F6.2,' GeV')
  160         FORMAT(I4,' towers')
              IF (LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_TYPE_INDEX)
     &          .EQ. AO_THRSH_MPT) THEN
                THRESH = LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_SUB1_INDEX)
                WRITE (BUF,150,IOSTAT=ISTAT) TOTAL_MPT_REF(THRESH) 
     &            * GLOBAL_ENERGY_SCALE(PX_QUANT)
                IS_THRESH = .TRUE.
C
              ELSEIF (LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_TYPE_INDEX)
     &          .EQ. AO_THRSH_GSUM) THEN
                QUANT = LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_SUB1_INDEX)
                THRESH = LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_SUB2_INDEX)
                WRITE (BUF,150,IOSTAT=ISTAT) 
     &            FLOAT(GLOBAL_ENERGY_REF(MOD(THRESH,4)+1, 
     &              THRESH/4 +1, QUANT) 
     &             - TREE_OFFSET(THRESH_TO_QUANT(QUANT))) 
     &               * GLOBAL_ENERGY_SCALE(THRESH_TO_QUANT(QUANT))
                IS_THRESH = .TRUE.
C
              ELSEIF (LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_TYPE_INDEX)
     &          .EQ. AO_THRSH_CNT) THEN
                REF_NUM = LV1_ANDOR_TERM_TYPE(ANDOR, 
     &            AO_THRSH_SUB1_INDEX)
                CNT = LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_SUB2_INDEX)
                WRITE (BUF,160,IOSTAT=ISTAT) HOT_COUNT_REF(CNT, REF_NUM)
                IS_THRESH = .TRUE.
              ELSE
                BUF = 'Unknown threshold quantity'
              ENDIF
            ENDIF
C
            IF (SPECTRIG_ANDOR_POLARITY(ANDOR, SPEC_TRIG) 
     &        .EQV. .FALSE.) THEN
              IF (IS_THRESH .EQV. .TRUE.) THEN
                WRITE (LUN,140,IOSTAT=ISTAT) ANDOR, '   veto', RCPKEY, 
     &            '.LT.', BUF(1:TRULEN(BUF))
              ELSE
                WRITE (LUN,145,IOSTAT=ISTAT) ANDOR, '   veto', RCPKEY,
     &            BUF(1:TRULEN(BUF))
              ENDIF
C
            ELSE
              IF (IS_THRESH .EQV. .TRUE.) THEN
                WRITE (LUN,140,IOSTAT=ISTAT) ANDOR, 'require', RCPKEY, 
     &            '.GE.', BUF(1:TRULEN(BUF))
              ELSE
                WRITE (LUN,145,IOSTAT=ISTAT) ANDOR, 'require', RCPKEY,
     &            BUF(1:TRULEN(BUF))
              ENDIF
            ENDIF
  140       FORMAT('       #',I3,2X,A,' = ',A, 2X, A4, 1X, A)
  145       FORMAT('       #',I3,2X,A,' = ',A, 2X, A)
          END IF
C
        END DO
C
        IF (ST_IS_L15(SPEC_TRIG) .EQV. .TRUE.) THEN
          WRITE(LUN,*) '    This is a Level 1.5 Specific Trigger'
          WRITE(LUN,*) '    Level 1.5 Terms'
C
          DO L15_TERM_NUM = L15_TERM_NUM_MIN, L15_TERM_NUM_MAX
            IF (L15_TERM_ALLOCATE(L15_TERM_NUM, SPEC_TRIG) 
     &        .EQV. .TRUE.) THEN
              IF (L15_TERM_RCPKEY(L15_TERM_NUM) .EQ. ' ') THEN
                WRITE(LUN,185) L15_TERM_NUM, 'UNKNOWN TERM NAME'
              ELSE
                WRITE(LUN,185) L15_TERM_NUM, 
     &                         L15_TERM_RCPKEY(L15_TERM_NUM)
              ENDIF
            ENDIF
          END DO
        ENDIF
  185   FORMAT(7X, 'Term #', I2, ': ', A)
C
C
        WRITE(LUN,*)
  170   CONTINUE
      END DO
C
C       Print the Reference Set Definitions
C
      WRITE (LUN,*)
      WRITE (LUN,*) 'Definition of Trigger Tower Reference Sets'
      WRITE (LUN,*) '------------------------------------------'
      DO REF_NUM = RS_SET_MIN, RS_SET_MAX
        WRITE (LUN,*)
        WRITE (LUN,*)
        CALL L1DMP_REFSET(LUN, EM_ET_REF_MIN, REF_NUM)
      END DO
      DO REF_NUM = RS_SET_MIN, RS_SET_MAX
        WRITE (LUN,*)
        WRITE (LUN,*)
        CALL L1DMP_REFSET(LUN,TOT_ET_REF_MIN, REF_NUM)
      END DO
      WRITE (LUN,*)
      WRITE (LUN,*) 'Definition of Large Tile Reference Sets'
      WRITE (LUN,*) '---------------------------------------'
      DO REF_NUM = 0, LT_REF_MAX-LT_REF_MIN
        WRITE (LUN,*)
        WRITE (LUN,*)
        CALL L1DMP_REFSET(LUN, LT_REF_MIN, REF_NUM)
      END DO
C
C       
      WRITE (LUN,20)
      WRITE (LUN,10)
C
C       Which triggers pay attention to each reference set.
C
      WRITE (LUN,*)
     &  'Specific Triggers used in each Reference Set'
      WRITE (LUN,*)
     &  '--------------------------------------------'
C
  200 FORMAT(A16,I1,' used in Sp.Trig. #')
  210 FORMAT(' ',I2)
  220 FORMAT(' ',A16,I1,' unused')
C       Print out the EM Et Ref Sets
      DO REF_NUM = RS_SET_MIN, RS_SET_MAX
        WRITE (LBUF,200,IOSTAT=ISTAT) 'EM  Et Ref Set #', REF_NUM
        BASE_LENGTH = TRULEN(LBUF) + 1
        CUR_POS = BASE_LENGTH
        OK = .FALSE.
        DO SPEC_TRIG = TRG_NUM_MIN, TRG_NUM_MAX
          IF (ST_VS_RS(SPEC_TRIG, REF_NUM+EM_ET_REF_MIN)
     &      .EQV. .TRUE.) THEN
            OK = .TRUE.
            WRITE (LBUF(CUR_POS:CUR_POS+2),210,IOSTAT=ISTAT) SPEC_TRIG
            CUR_POS = CUR_POS + 3
            IF (CUR_POS .GE. LINE_WIDTH - 3) THEN
              WRITE(LUN,*) LBUF(1:TRULEN(LBUF))
              CUR_POS = BASE_LENGTH
              LBUF = ' '
            ENDIF
          ENDIF
        END DO
        IF (LBUF .NE. ' ') THEN
          IF (OK .EQV. .TRUE.) THEN
            WRITE (LUN,*) LBUF(1:TRULEN(LBUF))
          ELSE
            WRITE (LUN,220,IOSTAT=ISTAT) 'EM  Et Ref Set #', REF_NUM
          ENDIF
        ENDIF
      END DO
C
C       Print out the TOT Et Ref Sets
      DO REF_NUM = RS_SET_MIN, RS_SET_MAX
        WRITE (LBUF,200,IOSTAT=ISTAT) 'TOT Et Ref Set #', REF_NUM
        BASE_LENGTH = TRULEN(LBUF) + 1
        CUR_POS = BASE_LENGTH
        OK = .FALSE.
        DO SPEC_TRIG = TRG_NUM_MIN, TRG_NUM_MAX
          IF (ST_VS_RS(SPEC_TRIG, REF_NUM+TOT_ET_REF_MIN)
     &      .EQV. .TRUE.) THEN
            OK = .TRUE.
            WRITE (LBUF(CUR_POS:CUR_POS+2),210,IOSTAT=ISTAT) SPEC_TRIG
            CUR_POS = CUR_POS + 3
            IF (CUR_POS .GE. LINE_WIDTH - 3) THEN
              WRITE(LUN,*) LBUF(1:TRULEN(LBUF))
              CUR_POS = BASE_LENGTH
              LBUF = ' '
            ENDIF
          ENDIF
        END DO
        IF (LBUF .NE. ' ') THEN
          IF (OK .EQV. .TRUE.) THEN
            WRITE (LUN,*) LBUF(1:TRULEN(LBUF))
          ELSE
            WRITE (LUN,220,IOSTAT=ISTAT) 'TOT Et Ref Set #', REF_NUM
          ENDIF
        ENDIF
      END DO
C
C       Print out the Large Tile Ref Sets
      DO REF_NUM = 0, LT_REF_MAX-LT_REF_MIN
        WRITE (LBUF,200,IOSTAT=ISTAT) 'LT Ref Set #', REF_NUM
        BASE_LENGTH = TRULEN(LBUF) + 1
        CUR_POS = BASE_LENGTH
        OK = .FALSE.
        DO SPEC_TRIG = TRG_NUM_MIN, TRG_NUM_MAX
          IF (ST_VS_RS(SPEC_TRIG, REF_NUM+LT_REF_MIN)
     &      .EQV. .TRUE.) THEN
            OK = .TRUE.
            WRITE (LBUF(CUR_POS:CUR_POS+2),210,IOSTAT=ISTAT) SPEC_TRIG
            CUR_POS = CUR_POS + 3
            IF (CUR_POS .GE. LINE_WIDTH - 3) THEN
              WRITE(LUN,*) LBUF(1:TRULEN(LBUF))
              CUR_POS = BASE_LENGTH
              LBUF = ' '
            ENDIF
          ENDIF
        END DO
        IF (LBUF .NE. ' ') THEN
          IF (OK .EQV. .TRUE.) THEN
            WRITE (LUN,*) LBUF(1:TRULEN(LBUF))
          ELSE
            WRITE (LUN,220,IOSTAT=ISTAT) 'LT Ref Set #', REF_NUM
          ENDIF
        ENDIF
      END DO
C
C       Tree Offsets
C
      WRITE (LUN,*)
      WRITE (LUN,*) 'Energy Summing Tree Offsets'
      WRITE (LUN,*) '---------------------------'
      WRITE (LUN,250,IOSTAT=ISTAT) '  EM Et', TREE_OFFSET(EM_ET_QUANT),
     &  TREE_OFFSET(EM_ET_QUANT) * GLOBAL_ENERGY_SCALE(EM_ET_QUANT)
      WRITE (LUN,250,IOSTAT=ISTAT) '  HD Et', TREE_OFFSET(HD_ET_QUANT),
     &  TREE_OFFSET(HD_ET_QUANT) * GLOBAL_ENERGY_SCALE(HD_ET_QUANT)
      WRITE (LUN,250,IOSTAT=ISTAT) ' TOT Et', TREE_OFFSET(TOT_ET_QUANT),
     &  TREE_OFFSET(TOT_ET_QUANT) * GLOBAL_ENERGY_SCALE(TOT_ET_QUANT)
      WRITE (LUN,250,IOSTAT=ISTAT) ' EM 2nd', TREE_OFFSET(EM_L2_QUANT),
     &  TREE_OFFSET(EM_L2_QUANT) * GLOBAL_ENERGY_SCALE(EM_L2_QUANT)
      WRITE (LUN,250,IOSTAT=ISTAT) ' HD 2nd', TREE_OFFSET(HD_L2_QUANT),
     &  TREE_OFFSET(HD_L2_QUANT) * GLOBAL_ENERGY_SCALE(HD_L2_QUANT)
      WRITE (LUN,250,IOSTAT=ISTAT) 'TOT 2nd', TREE_OFFSET(TOT_L2_QUANT),
     &  TREE_OFFSET(TOT_L2_QUANT) * GLOBAL_ENERGY_SCALE(TOT_L2_QUANT)
  250 FORMAT (' ', A7, I7, ' ADC count = ', F8.2, ' GeV')
C
C       Sum Thresholds
C
      WRITE (LUN,*)
      WRITE (LUN,*) 'Global Sum Threshold Definitions'
      WRITE (LUN,*) '--------------------------------'
C
      CALL L1DMP_ENERGY_SUM_THRSH(LUN,GL_EMET_THRTYP,
     &  'EM Et Energy Sum', GLOBAL_ENERGY_REF(1,1,GL_EMET_THRTYP), 
     &  REF_MAX-1)
      CALL L1DMP_ENERGY_SUM_THRSH(LUN,GL_HDET_THRTYP, 
     &  'HD Et Energy Sum', GLOBAL_ENERGY_REF(1,1,GL_HDET_THRTYP), 
     &  REF_MAX-1)
      CALL L1DMP_ENERGY_SUM_THRSH(LUN,GL_TOTET_THRTYP, 
     &  'TOT Et Energy Sum', GLOBAL_ENERGY_REF(1,1,GL_TOTET_THRTYP), 
     &  REF_MAX-1)
      CALL L1DMP_ENERGY_SUM_THRSH(LUN,GL_EML2_THRTYP, 
     &  'EM 2nd Energy Sum', GLOBAL_ENERGY_REF(1,1,GL_EML2_THRTYP), 
     &  REF_MAX-1)
      CALL L1DMP_ENERGY_SUM_THRSH(LUN,GL_HDL2_THRTYP, 
     &  'HD 2nd Energy Sum', GLOBAL_ENERGY_REF(1,1,GL_HDL2_THRTYP), 
     &  REF_MAX-1)
      CALL L1DMP_ENERGY_SUM_THRSH(LUN,GL_TOTL2_THRTYP, 
     &  'TOT 2nd Energy Sum', GLOBAL_ENERGY_REF(1,1,GL_TOTL2_THRTYP), 
     &  REF_MAX-1)
      CALL L1DMP_ENERGY_SUM_THRSH(LUN,AO_THRSH_MPT, 
     &  'Mis Pt Energy Sum', TOTAL_MPT_REF, MPT_CMP_MAX-1)
C
C       Count Thresholds
  260 FORMAT(' ', A, I1)
      DO REF_NUM = EM_ET_REF_MIN, TOT_ET_REF_MAX
        WRITE (LUN,*)
        FIRST_LINE = .TRUE.
        IF (REF_NUM .LT. TOT_ET_REF_MIN) THEN
          WRITE (LUN,260,IOSTAT=ISTAT) 'EM  Et Ref Set # ', 
     &      REF_NUM - EM_ET_REF_MIN
        ELSE
          WRITE (LUN,260,IOSTAT=ISTAT) 'TOT Et Ref Set # ', 
     &      REF_NUM - TOT_ET_REF_MIN
        ENDIF
C
        DO CNT = TOWER_CNT_THRSH_MIN, TOWER_CNT_THRSH_MAX
          DO ANDOR = ANDOR_NUM_MIN, ANDOR_NUM_MAX
            IF ((LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_TYPE_INDEX) 
     &         .EQ. AO_THRSH_CNT) .AND. 
     &        (LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_SUB1_INDEX) 
     &         .EQ. REF_NUM) .AND.
     &        (LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_SUB2_INDEX)
     &         .EQ. CNT)) THEN
              GOTO 270
            ENDIF
          END DO
C
C       Move on to the next threshold number
          GOTO 280
C
C       Find the definition of the Andor Term name
  270     CONTINUE
          CALL L1DMP_GET_NEXT_ANDOR_KEY_INIT()
          OK = .TRUE.
          DO WHILE (OK .EQV. .TRUE.) 
            CALL L1DMP_GET_NEXT_ANDOR_KEY(RCPKEY, TERM_NUM, OK)
            IF (OK .EQV. .FALSE.) THEN
              RCPKEY = 'unknown'
            ELSEIF (TERM_NUM .EQ. ANDOR) THEN
              OK = .FALSE.
            ENDIF
          END DO
C
          FIRST_LINE = .FALSE.
          THRESH = HOT_COUNT_REF(CNT, REF_NUM)
          IF (THRESH .GT. 0) THEN
            WRITE (LUN, 290,IOSTAT=ISTAT) ANDOR, CNT, THRESH,
     &        RCPKEY(1:TRULEN(RCPKEY))
          ELSE
            WRITE (LUN, 295,IOSTAT=ISTAT) ANDOR, CNT, 
     &        RCPKEY(1:TRULEN(RCPKEY))
          ENDIF
  290     FORMAT('   Andor Term #', I3, ' : Tower Count Threshold #', 
     &      I2, ' :', I3, ' Trigger Towers  (', A, ')')
  295     FORMAT('   Andor Term #', I3, ' : Tower Count Threshold #', 
     &      I2, ' : Unused  (', A, ')')
C
  280     CONTINUE
        END DO
C
        IF (FIRST_LINE .EQV. .TRUE.) THEN
          WRITE (LUN,*) '  No available thresholds'
        ENDIF
      END DO
C
C       Other Andor Terms
C
      WRITE (LUN,*)
      WRITE (LUN,*) 'Other available Level 1 Andor Terms'
      WRITE (LUN,*) '-----------------------------------'
  300 FORMAT (' Andor Term #',I3,': ', A)
  310 FORMAT (' Andor Term #',I3,': ', A, ' (Not simulated by L1SIM)')
      CALL L1DMP_GET_NEXT_ANDOR_KEY_INIT()
      OK = .TRUE.
      DO WHILE (OK .EQV. .TRUE.) 
        RCPKEY = ' '
        CALL L1DMP_GET_NEXT_ANDOR_KEY(RCPKEY, TERM_NUM, OK)
        IF ((OK .EQV. .TRUE.) .AND. (TERM_NUM .GE. ANDOR_NUM_MIN) 
     &    .AND. (TERM_NUM .LE. ANDOR_NUM_MAX)) THEN 
          IF (LV1_ANDOR_TERM_TYPE(TERM_NUM, AO_THRSH_TYPE_INDEX) 
     &      .EQ. 0) THEN
            IF (ANDOR_TERM_ASSIGNED(TERM_NUM) .EQV. .TRUE.) THEN
              WRITE (LUN,300,IOSTAT=ISTAT) TERM_NUM,
     &          RCPKEY(1:TRULEN(RCPKEY))
            ELSE
              WRITE (LUN,310,IOSTAT=ISTAT) TERM_NUM,
     &          RCPKEY(1:TRULEN(RCPKEY))
            ENDIF
          ENDIF
        ENDIF
      END DO
C
C       Level 1.5 terms
C
      WRITE (LUN,*)
      WRITE (LUN,*) 'Available Level 1.5 Terms'
      WRITE (LUN,*) '-------------------------'
  400 FORMAT(' Term #',I3,': ', A)
      DO TERM_NUM = L15_TERM_NUM_MIN, L15_TERM_NUM_MAX
        IF (L15_TERM_RCPKEY(TERM_NUM) .NE. ' ') THEN
          WRITE(LUN,400) TERM_NUM, L15_TERM_RCPKEY(TERM_NUM)
        ENDIF
      END DO
C
      WRITE (LUN,*)
      WRITE (LUN,10)
C
  999 RETURN
      END
