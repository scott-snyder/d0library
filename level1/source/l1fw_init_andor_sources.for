      SUBROUTINE L1FW_INIT_ANDOR_SOURCES()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize the array which describes the sources of
C-     the AndOr terms associated with Calorimeter Trigger comparisons.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  12-AUG-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                          - Commented out code which reads from RCP bank
C-                            LV1_EXISTING_RESOURCE until this information is
C-                            available.
C-                          - Changed order of delcaration statements to pass
C-                            D0FLAVOR
C-   Updated  26-NOV-1991   Philippe Laurens, Steven Klocek   
C-                      - No longer complains when two non-CalTrig Andor Terms
C-                        have the same term number. 
C-                      - No longer complains when a non-CalTrig Andor Term has
C-                        the same term number as a CalTrig Andor.
C-                      - DOES complain when two CalTrig Andor Terms have the
C-                        same term number.
C-   Updated  16-DEC-1991   Philippe Laurens, Steven Klocek  
C-                      - Now recognizes the Andor Term names as actually
C-                        produced by COOR. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:L1FW_ANDOR_AND_MISC.INC'
      LOGICAL  L1UTIL_PICK_RESOURCE_RCP
      EXTERNAL L1UTIL_PICK_RESOURCE_RCP
      INTEGER  TRULEN
      EXTERNAL TRULEN
C
      INTEGER CURRENT, ID
      INTEGER ISTAT
      INTEGER NUM_WORDS, FIRST_CHAR, LAST_CHAR, KEY_LEN, LENW, WORDI
      INTEGER THRESH, IER, ANDOR, CMP_NUM, REF_NUM
      CHARACTER*72 STRING
      CHARACTER*32 RCPKEY
      INTEGER MAX_NUM_STR
      PARAMETER (MAX_NUM_STR = 7)
      INTEGER WORD_START(MAX_NUM_STR)
      LOGICAL ANDOR_USED(ANDOR_NUM_MIN:ANDOR_NUM_MAX)
C
      CHARACTER*(*) PREFIX
      PARAMETER (PREFIX = 'L1C_')
      INTEGER PREFIX_LEN
      PARAMETER (PREFIX_LEN = 4)
      CHARACTER*(*) EMETCNT, TOTETCNT, CMP, REF
      PARAMETER (EMETCNT = 'EMETCNT', TOTETCNT = 'TOTETCNT',
     &  CMP = 'CMP', REF = 'REF')
C
      CHARACTER*(*) MISPT
      PARAMETER (MISPT = 'MISPTSUM')
      INTEGER RCPOK
      PARAMETER (RCPOK = 0)
      INTEGER SPECTRIG, GEOSECT
      CHARACTER*8 THRESH_STR(GL_EMET_THRTYP:GL_TOTL2_THRTYP)
      INTEGER THRESH_STR_LEN(GL_EMET_THRTYP:GL_TOTL2_THRTYP)
      DATA THRESH_STR
     &  / 'EMETSUM', 'EML2SUM', 'HDETSUM', 'HDL2SUM', 
     &    'TOTETSUM', 'TOTL2SUM' /
      DATA THRESH_STR_LEN / 7, 7, 7, 7, 8, 8 /
C
C       Mark all Andor Terms as not assigned
      DO ANDOR = ANDOR_NUM_MIN, ANDOR_NUM_MAX
        ANDOR_USED(ANDOR) = .FALSE.
      END DO
C
C       Select the RCP bank
      IF (L1UTIL_PICK_RESOURCE_RCP() .EQV. .FALSE.) GOTO 9999
C
C       Go through each parameter beginning with L1C
      CURRENT = 1
      ID = 0
      CALL EZGNXT(' ', CURRENT, ID)
C
      DO WHILE (ID .NE. 0)
C
C       Get the Andor Term number of the current parameter
C
        CALL EZGET1(ID, 1, 1, 1, ANDOR, IER)
        IF (IER .NE. RCPOK) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG (' EZGET1',
     &      'L1FW_INIT_ANDOR_SOURCES',STRING,'F')
          GOTO 999
        ENDIF
C
C       If the Andor Term number is out of range, then ignore it.
        IF ((ANDOR .GT. ANDOR_NUM_MAX)
     &    .OR. (ANDOR .LT. ANDOR_NUM_MIN)) GOTO 500
C
        CALL EZGETN(ID, RCPKEY, KEY_LEN)
        CALL EZERR(IER)
        IF (IER .NE. RCPOK) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG( ' EZGETN','L1FW_INIT_ANDOR_SOURCES',STRING,'F')
          GOTO 999
        ENDIF
        IF (KEY_LEN .LT. 32) THEN
          RCPKEY(KEY_LEN+1:32) = ' '
        ENDIF
C
C       Mark the Andor Term number as assigned
C       It is an error to assign two names to the same Andor Term
        IF (ANDOR_USED(ANDOR) .EQV. .FALSE.) THEN
          ANDOR_USED(ANDOR) = .TRUE.
        ELSE
C
C       No longer complain about Andor Terms with the same term numbers
C
C          WRITE (STRING,50, IOSTAT=ISTAT) ANDOR
C   50     FORMAT( I3 )
C          CALL ERRMSG('ANDOR CONFILICT','L1FW_INIT_ANDOR_SOURCES',
C     &      'Andor term number assigned twice in resource file: ' 
C     &      // 'Term #' 
C     &      // STRING(1:TRULEN(STRING)), 'F' )
        ENDIF
C
C       Parse the Andor Term name
C
C       A valid Calorimeter Trigger Andor Term name will have 3 or 5
C         underscores.
C
C       Divide the string into words
        NUM_WORDS = 1
        WORD_START(NUM_WORDS) = 1
        DO LAST_CHAR = 1, KEY_LEN
          IF (RCPKEY(LAST_CHAR:LAST_CHAR) .EQ. '_') THEN
            NUM_WORDS = NUM_WORDS + 1
            IF (NUM_WORDS .GE. MAX_NUM_STR) GOTO 500
            WORD_START(NUM_WORDS) = LAST_CHAR +1
          ENDIF
        END DO
        WORD_START(NUM_WORDS+1) = KEY_LEN+2
C
        IF ((NUM_WORDS .NE. 4) .AND. (NUM_WORDS .NE. 6)) GOTO 500
C
        DO WORDI = 1, NUM_WORDS
          IF (WORD_START(WORDI) .GT. WORD_START(WORDI+1)-2) GOTO 500
        ENDDO
C
C       The word should start with L1C_
        IF (RCPKEY(WORD_START(1):WORD_START(2)-1)
     &    .NE. PREFIX) GOTO 500
C
C       It should be either a sum or a count
        IF ((RCPKEY(WORD_START(2):WORD_START(3)-2) .NE. EMETCNT) .AND.
     &    (RCPKEY(WORD_START(2):WORD_START(3)-2) .NE. TOTETCNT)) THEN
C
          DO THRESH = GL_EMET_THRTYP, GL_TOTL2_THRTYP
            IF (THRESH_STR(THRESH) 
     &        .EQ. RCPKEY(WORD_START(2):WORD_START(3)-2)) GOTO 300
          END DO
          IF (MISPT .EQ. RCPKEY(WORD_START(2):WORD_START(3)-2)) GOTO 300
C
C       If execution reaches here, this is not a Calorimeter Trigger message
C
          GOTO 500
C
C
C       Decode the comparator number
  300     CONTINUE
          IF (RCPKEY(WORD_START(3):WORD_START(4)-2) .NE. CMP) THEN
            CALL ERRMSG('INVALID ANDOR DEF', 'L1FW_INIT_ANDOR_SOURCES', 
     &        'Invalid Calorimeter Trigger ANDOR key:' // RCPKEY, 'F')
            GOTO 500
          ENDIF
C
          READ(RCPKEY(WORD_START(4):WORD_START(5)-2),460,
     &      IOSTAT=ISTAT) CMP_NUM
  460     FORMAT( I2 )
          IF (ISTAT .NE. 0) THEN
            CALL ERRMSG('INVALID ANDOR DEF', 'L1FW_INIT_ANDOR_SOURCES', 
     &        'Invalid comparator number in resource file: ' // RCPKEY,
     &        'F')
            GOTO 500
          ENDIF
C
          IF ((CMP_NUM .LT. SUM_MIN) .OR. (CMP_NUM .GT. SUM_MAX)) THEN
            CALL ERRMSG('INVALID ANDOR DEF','L1FW_INIT_ANDOR_SOURCES',
     &        'Comparator number out of range in resource file: ' 
     &        // RCPKEY, 'F')
            GOTO 500
          ENDIF
C
C       Check global sum definitions
          DO THRESH = GL_EMET_THRTYP, GL_TOTL2_THRTYP
            IF (RCPKEY(WORD_START(2):WORD_START(3)-2) 
     &        .EQ. THRESH_STR(THRESH)) GOTO 450
          END DO
C
C       Handle Missing Pt
          IF (RCPKEY(WORD_START(2):WORD_START(3)-2) .EQ. MISPT) THEN
            IF (LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_TYPE_INDEX)
     &        .NE. 0) THEN
              WRITE (STRING,465, IOSTAT=ISTAT) ANDOR
  465         FORMAT( I3 )
              CALL ERRMSG('ANDOR CONFILICT','L1FW_INIT_ANDOR_SOURCES',
     &          'Andor term number assigned twice in resource file: ' 
     &          // 'Term #' 
     &          // STRING(1:TRULEN(STRING)), 'F' )
            ENDIF
C
            LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_TYPE_INDEX) 
     &        = AO_THRSH_MPT
            LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_SUB1_INDEX) 
     &        = CMP_NUM + 1
            LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_SUB2_INDEX) = 0
          ENDIF
          GOTO 500
C
  450     CONTINUE
C
C       Store the Andor Term number for other sums
C
            IF (LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_TYPE_INDEX)
     &        .NE. 0) THEN
              WRITE (STRING,455, IOSTAT=ISTAT) ANDOR
  455         FORMAT( I3 )
              CALL ERRMSG('ANDOR CONFILICT','L1FW_INIT_ANDOR_SOURCES',
     &          'Andor term number assigned twice in resource file: ' 
     &          // 'Term #' 
     &          // STRING(1:TRULEN(STRING)), 'F' )
              GOTO 500
            ENDIF
C
          LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_TYPE_INDEX)
     &      = AO_THRSH_GSUM
          LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_SUB1_INDEX) = THRESH
          LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_SUB2_INDEX) = CMP_NUM 
          GOTO 500
C
C       Handle Tower Count Andor Terms
C
        ELSE
C
          IF ((RCPKEY(WORD_START(3):WORD_START(4)-2) .NE. REF) 
     &      .OR. (RCPKEY(WORD_START(5):WORD_START(6)-2) .NE. CMP)) THEN
            CALL ERRMSG('INVALID ANDOR DEF','L1FW_INIT_ANDOR_SOURCES', 
     &        'Invalid Calorimeter Trigger Andor Term definition: ' //
     &        RCPKEY, 'F')
            GOTO 500
          ENDIF
C
          READ(RCPKEY(WORD_START(4):WORD_START(5)-2),470,
     &      IOSTAT=ISTAT) REF_NUM
  470     FORMAT( I1 )
          IF (ISTAT .NE. 0) THEN
            CALL ERRMSG('INVALID ANDOR DEF','L1FW_INIT_ANDOR_SOURCES', 
     &        'Invalid Calorimeter Trigger Andor Term definition: ' //
     &        RCPKEY, 'F')
            GOTO 500
          ENDIF
C
          IF ((REF_NUM .LT. RS_SET_MIN) 
     &      .OR. (REF_NUM .GT. RS_SET_MAX)) THEN
            CALL ERRMSG('INVALID ANDOR DEF','L1FW_INIT_ANDOR_SOURCES',
     &        'Reference set number out of range in resource file: ' 
     &        // RCPKEY, 'F')
            GOTO 500
          ENDIF
C
          READ(RCPKEY(WORD_START(6):WORD_START(7)-2),480,
     &      IOSTAT=ISTAT) CMP_NUM
  480     FORMAT( I2 )
          IF (ISTAT .NE. 0) THEN
            CALL ERRMSG('INVALID ANDOR DEF','L1FW_INIT_ANDOR_SOURCES', 
     &        'Invalid Calorimeter Trigger Andor Term definition: ' //
     &        RCPKEY, 'F')
            GOTO 500
          ENDIF
C
          IF ((CMP_NUM .LT. TOWER_CNT_THRSH_MIN) 
     &      .OR. (CMP_NUM .GT. TOWER_CNT_THRSH_MAX)) THEN
            CALL ERRMSG('INVALID ANDOR DEF','L1FW_INIT_ANDOR_SOURCES',
     &        'Comparator number out of range in resource file: ' 
     &        // RCPKEY, 'F')
            GOTO 500
          ENDIF
C
          IF (RCPKEY(WORD_START(2):WORD_START(3)-2) .EQ. EMETCNT) THEN
            REF_NUM = REF_NUM + EM_ET_REF_MIN
          ELSEIF (RCPKEY(WORD_START(2):WORD_START(3)-2) 
     &      .EQ. TOTETCNT) THEN
            REF_NUM = REF_NUM + TOT_ET_REF_MIN
          ELSE
            CALL ERRMSG('INVALID ANDOR DEF','L1FW_INIT_ANDOR_SOURCES', 
     &        'Invalid Calorimeter Trigger Andor Term definition: ' //
     &        RCPKEY, 'F')
            GOTO 500
          ENDIF
C
          IF (LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_TYPE_INDEX)
     &      .NE. 0) THEN
            WRITE (STRING,485, IOSTAT=ISTAT) ANDOR
  485       FORMAT( I3 )
            CALL ERRMSG('ANDOR CONFILICT','L1FW_INIT_ANDOR_SOURCES',
     &        'Andor term number assigned twice in resource file: '
     &        // 'Term #' 
     &        // STRING(1:TRULEN(STRING)), 'F' )
            GOTO 500
          ENDIF
C
          LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_TYPE_INDEX)
     &      = AO_THRSH_CNT
          LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_SUB1_INDEX) = REF_NUM
          LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_SUB2_INDEX) = CMP_NUM
C          
        ENDIF
  500   CONTINUE
        ID = 0
        CALL EZGNXT(' ', CURRENT, ID)
      END DO
  999 CONTINUE
C
C       Deselect the RCP bank
      CALL EZRSET()
C
 9999 CONTINUE
C
C       Find which Specific Triggers and Geographic Sections should exist
C
C       25-OCT-1991 
C       Commented out until this information is available from COOR
C
C      CALL EZPICK('LV1_EXISTING_RESOURCE')
C      CALL EZERR(IER)
C      IF (IER .NE. RCPOK) THEN
C        CALL EZGET_ERROR_TEXT(IER, STRING)
C        CALL ERRMSG('EZPICK','L1FW_INIT_ANDOR_SOURCES', STRING, 'F')
C        GOTO 2000
C      ENDIF
C
C      DO SPECTRIG = TRG_NUM_MIN, TRG_NUM_MAX
C        WRITE (RCPKEY,800) SPECTRIG
C  800   FORMAT('SPEC_TRIG_', I2.2, '_EXIST')
C        CALL EZGET(RCPKEY, EXIST_SPECTRIG(SPECTRIG), IER)
C        IF (IER .NE. RCPOK) EXIST_SPECTRIG(SPECTRIG) = .FALSE.
C      END DO
C
C      DO GEOSECT = GEO_NUM_MIN, GEO_NUM_MAX
C        WRITE (RCPKEY,810) GEOSECT
C  810   FORMAT('GEO_SECT_', I2.2, '_EXIST')
C        CALL EZGET (RCPKEY, EXIST_GEOSECT(GEOSECT), IER)
C        IF (IER .NE. RCPOK) EXIST_GEOSECT(GEOSECT) = .FALSE.
C      END DO
C
C       Deselect the RCP bank
C 1999 CONTINUE
C      CALL EZRSET()
C
 2000 CONTINUE
      RETURN
      END
