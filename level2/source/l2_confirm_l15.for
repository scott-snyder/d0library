      SUBROUTINE L2_CONFIRM_L15(PARAM_SET_NUMBER,HARDWARE,RESULT_FLAG,
     &  EXTRA_FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : level 2 tool to confirm level 1.5 trigger on
C-                         those specific bits which make use of level
C-                         1.5. :
C-
C-   Inputs  : PARAM_SET_NUMBER : # of parameter set to use
C-             HARDWARE:          mask of set bits for LV1 trigger which started
C-                                  this filter.
C-   Outputs : RESULT_FLAG :      Flag set to TRUE when we want to pass tool
C-                                  under this PARAM_SET_NUMBER
C-             EXTRA_FLAG  :      Set to TRUE when we want to pass event and
C-                                  do no further filtering. (NOT IMPLEMENTED)
C-   Controls:
C-
C-   Created:  4-dec-1992   eric james   configure for level 1.5 confirmation
C-   Updated: 12-mar-1993   eric james   configure for use with new otcmgrs
C-            28-apr-1993   eric james   return to two parameters
C-   Updated   2-MAR-1994   sFahey       Added CAL level 1.5 confirmation
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:L15CALDBB_DATA_BLOCK.PARAMS'
      INCLUDE 'D0$PARAMS:L15_UNBIAS_INFO.PARAMS'
C
      INTEGER PARAM_SET_NUMBER,HARDWARE
      LOGICAL EXTRA_FLAG,RESULT_FLAG
      INCLUDE 'D0$INC:ZEBCOM.INC'               ! zebra main store
      CHARACTER*80 MSG
C
      CHARACTER*69 CONFIRM_TYPE
      CHARACTER*69 L15_REGIONS                  ! filter parameters
      INTEGER  NUM_L15_MUONS
C
      INTEGER IP,NPARIN,IER
      LOGICAL EZERROR,OK
      INTEGER I,J,NEWPLD(5),STRSUB
      INTEGER STR_LEN,NUM_LOW,NUM_HIGH
      LOGICAL CF,WN,WS,ON,OS,SN,SS
      CHARACTER*2 STRING
      CHARACTER*1 STRING2
      CHARACTER*4 PTCUT
C
      INTEGER IREG(6),CRATEWORDS(5),TRIG_NUM(5),CRATE_ID(5)
      INTEGER NOTC_CARDS(5),ACTOTC(5),VERSION(5),L15OTC(5)
      INTEGER LONGTO(5),TABTYP(5),TRGBITS(5),MERR(5),KERR(5)
      INTEGER CCTLAT(5),CCTLAT2(5),OTCNUM(5,16),OTCSTAT(5,16)
      INTEGER MGRSTAT(5),NOTCWD(5),KTABLE(5,130,2,4),IFTWORD(2)
      INTEGER TRAILWC(5),TRAILCR(5),TRAILTN(5)
C
      INTEGER MAX_L15BIT
      PARAMETER (MAX_L15BIT = 15)
      INTEGER L1BITS_PER_L15TERM(0:MAX_L15BIT)
      INTEGER L15TERMS_ON(0:MAX_L15BIT),L15TERM
      INTEGER NBITS_ON,NTERMS,L1BIT_ON
      INTEGER L15_FRAME_SECTION(L15CAL_FRAME_LENGTH)
      INTEGER L15_TERM_MASK
      INTEGER L15TERMS_TRIED,L15TERMS_PASSED
      INTEGER L15TERMS_INVAL,L15TERMS_RET
      LOGICAL PASSED_L15CAL
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      EXTRA_FLAG = .FALSE.
      RESULT_FLAG = .FALSE.
C
C...first, carefully retrieve cuts from RCP
      IP = PARAM_SET_NUMBER
      CALL EZPICK('L2_CONFIRM_L15')             ! configuration file (.FILTs)
      OK = .NOT.EZERROR(IER)
      IF (OK) THEN
C   Confirming muons or cal objects?
        IF (IER.EQ.0) 
     &    CALL EZGETS('L15_CONFIRM_TYPE',IP,CONFIRM_TYPE,STR_LEN,IER)
C
C   Check if parameter L15_CONFIRM_TYPE is there.  If not, assume this
C   is an old trigger list and the confirm type is muon.
C
        IF (IER.EQ.-2) THEN    ! IER = -2 means parameter not found 
          CONFIRM_TYPE = 'MUO'
          CALL ERRMSG('L2_CONFIRM_L15','L2_CONFIRM_L15_PARAMETERS',
     &      'L15_CONFIRM_TYPE not found.  Defaulting to MUON.','W')
          IER = 0
        ENDIF
C
      IF ((CONFIRM_TYPE.NE.'MUO').AND.(CONFIRM_TYPE.NE.'CAL'))
     &  OK = .FALSE.
C
C
        IF (CONFIRM_TYPE.EQ.'MUO') THEN
C
C...is IP consistent with the number of sets which exist?
          IF (IER.EQ.0) CALL EZGET('NUMBER_OF_SETS',NPARIN,IER)
          IF (IER.EQ.0) THEN
            IF ((IP.LE.0).OR.(IP.GT.NPARIN)) THEN
              WRITE(MSG,'(A,I5,A,I5)')' parameter set requested = ',
     &        PARAM_SET_NUMBER, ' but only had ',NPARIN
              CALL ERRMSG('L2_CONFIRM_L15','L2_CONFIRM_L15',MSG,'F')
              GO TO 999
            ENDIF
C
C...Get the parameters from the IPth set
            IF (IER.EQ.0)
     $         CALL EZGETA('NUM_L15_MUONS',IP,IP,1,NUM_L15_MUONS,IER)
            IF (IER.EQ.0)
     $         CALL EZGETS('L15_REGIONS',IP,L15_REGIONS,STR_LEN,IER)
          ENDIF 
C
        ENDIF   ! muon confirmation parameters
C
      ENDIF     ! if OK
C
      IF (.NOT.OK) THEN
        CALL ERRMSG('L2_CONFIRM_L15','L2_CONFIRM_L15', 'BANK BLANK','F')
      ELSEIF (IER .NE. 0) THEN      ! Error reading RCP
        CALL ERRMSG('L2_CONFIRM_L15','L2_CONFIRM_L15_PARAMETERS',
     &          'Couldn''t find parameter','F')
      ENDIF
C
C...now you can actually do some cutting
C
C------------------------------------------------------------------------------
C
      IF (CONFIRM_TYPE.EQ.'MUO') THEN
C
        CF = .FALSE.
        WN = .FALSE.
        WS = .FALSE.
        ON = .FALSE.
        OS = .FALSE.
        SN = .FALSE.
        SS = .FALSE.
C
        STRING2 = L15_REGIONS(STR_LEN:STR_LEN)
        IF (STRING2.EQ.')') THEN
          PTCUT = 'HIGH'
          STRSUB = 0
        ELSE IF (STRING2.EQ.'W') THEN
          PTCUT = 'LOW'
          STRSUB = 4
        ELSE IF (STRING2.EQ.'H') THEN
          PTCUT = 'HIGH'
          STRSUB = 5
        ELSE
          WRITE(MSG,'(A)') 'illegal input parameter (bad character   
     &      string for l15_regions)'
          CALL ERRMSG('L2_CONFIRM_L15','L2_CONFIRM_L15',MSG,'F')
          GO TO 999
        ENDIF
        STR_LEN = STR_LEN - STRSUB
C
        DO J = 1,(STR_LEN-1)/3
          STRING = L15_REGIONS(((J-1)*3)+2:((J-1)*3)+3)
          IF (STRING.EQ.'Y1') THEN
            CF = .TRUE.
          ELSE IF (STRING.EQ.'Y2') THEN
            CF = .TRUE.
            WN = .TRUE.
            WS = .TRUE.
          ELSE IF (STRING.EQ.'Y3') THEN
            CF = .TRUE.
            WN = .TRUE.
            WS = .TRUE.
            ON = .TRUE.
            OS = .TRUE.
          ELSE IF (STRING.EQ.'Y4') THEN
            CF = .TRUE.
            WN = .TRUE.
            WS = .TRUE.
            ON = .TRUE.
            OS = .TRUE.
            SN = .TRUE.
            SS = .TRUE.
          ELSE IF (STRING.EQ.'CF') THEN
            CF = .TRUE.
          ELSE IF (STRING.EQ.'WN') THEN
            WN = .TRUE.
          ELSE IF (STRING.EQ.'WS') THEN
            WS = .TRUE.
          ELSE IF (STRING.EQ.'ON') THEN
            ON = .TRUE.
          ELSE IF (STRING.EQ.'OS') THEN
            OS = .TRUE.
          ELSE IF (STRING.EQ.'SN') THEN
            SN = .TRUE.
          ELSE IF (STRING.EQ.'SS') THEN
            SS = .TRUE.
          ELSE
            WRITE(MSG,'(A,A)') 'non-allowed character set in parameter 
     &        string l15_regions = ',STRING
            CALL ERRMSG('L2_CONFIRM_L15','L2_CONFIRM_L15',MSG,'F')
            GO TO 999
          ENDIF
        ENDDO
C
        NUM_HIGH = 0
        NUM_LOW = 0
C
        CALL GTTRGR2(IREG,CRATEWORDS,TRIG_NUM,CRATE_ID,NOTC_CARDS,
     &    ACTOTC,VERSION,L15OTC,LONGTO,TABTYP,TRGBITS,MERR,KERR,
     &    CCTLAT,CCTLAT2,OTCNUM,OTCSTAT,MGRSTAT,NOTCWD,KTABLE,IFTWORD,
     &    TRAILWC,TRAILCR,TRAILTN)
C
        DO I = 1,5
          NEWPLD(I) = 0
          IF (VERSION(I).GT.12549) NEWPLD(I) = 1
          IF (VERSION(I).GT.15382) NEWPLD(I) = 2
        ENDDO
C
        IF (CF) THEN
          IF (NEWPLD(1).EQ.2) THEN
            IF (BTEST(TRGBITS(1),0)) NUM_LOW = NUM_LOW + 1
            IF (BTEST(TRGBITS(1),1)) NUM_LOW = NUM_LOW + 2
            IF (BTEST(TRGBITS(1),2)) NUM_HIGH = NUM_HIGH + 1
          ENDIF
          IF (NEWPLD(1).EQ.1) THEN
            IF (BTEST(TRGBITS(1),0)) NUM_LOW = NUM_LOW + 1
            IF (BTEST(TRGBITS(1),1)) NUM_HIGH = NUM_HIGH + 1
          ENDIF
          IF (NEWPLD(1).EQ.0) THEN
            IF (BTEST(TRGBITS(1),0)) NUM_HIGH = NUM_HIGH + 1
            IF (BTEST(TRGBITS(1),1)) NUM_HIGH = NUM_HIGH + 2
          ENDIF
        ENDIF
        IF (WN) THEN
          IF (NEWPLD(2).EQ.2) THEN
            IF (BTEST(TRGBITS(2),2)) NUM_HIGH = NUM_HIGH + 1
          ENDIF
          IF (NEWPLD(2).EQ.1) THEN
            IF (BTEST(TRGBITS(2),1)) NUM_LOW = NUM_LOW + 1
            IF (BTEST(TRGBITS(2),2)) NUM_HIGH = NUM_HIGH + 1
          ENDIF
          IF (NEWPLD(2).EQ.0) THEN
            IF (BTEST(TRGBITS(2),2)) NUM_HIGH = NUM_HIGH + 1
          ENDIF
        ENDIF
        IF (WS) THEN
          IF (NEWPLD(3).EQ.2) THEN
            IF (BTEST(TRGBITS(3),2)) NUM_HIGH = NUM_HIGH + 1
          ENDIF
          IF (NEWPLD(3).EQ.1) THEN
            IF (BTEST(TRGBITS(3),1)) NUM_LOW = NUM_LOW + 1
            IF (BTEST(TRGBITS(3),2)) NUM_HIGH = NUM_HIGH + 1
          ENDIF
          IF (NEWPLD(3).EQ.0) THEN
            IF (BTEST(TRGBITS(3),2)) NUM_HIGH = NUM_HIGH + 1
          ENDIF
        ENDIF
        IF (ON) THEN
          IF (NEWPLD(2).EQ.2) THEN
            IF (BTEST(TRGBITS(2),0)) NUM_LOW = NUM_LOW + 1
            IF (BTEST(TRGBITS(2),1)) NUM_LOW = NUM_LOW + 2
          ENDIF
          IF (NEWPLD(2).EQ.1) THEN
            IF (BTEST(TRGBITS(2),0)) NUM_LOW = NUM_LOW + 1
          ENDIF
          IF (NEWPLD(2).EQ.0) THEN
            IF (BTEST(TRGBITS(2),0)) NUM_LOW = NUM_LOW + 1
            IF (BTEST(TRGBITS(2),1)) NUM_LOW = NUM_LOW + 2
          ENDIF
        ENDIF
        IF (OS) THEN
          IF (NEWPLD(3).EQ.2) THEN
            IF (BTEST(TRGBITS(3),0)) NUM_LOW = NUM_LOW + 1
            IF (BTEST(TRGBITS(3),1)) NUM_LOW = NUM_LOW + 2
          ENDIF
          IF (NEWPLD(3).EQ.1) THEN
            IF (BTEST(TRGBITS(3),0)) NUM_LOW = NUM_LOW + 1
          ENDIF
          IF (NEWPLD(3).EQ.0) THEN
            IF (BTEST(TRGBITS(3),0)) NUM_LOW = NUM_LOW + 1
            IF (BTEST(TRGBITS(3),1)) NUM_LOW = NUM_LOW + 2
          ENDIF
        ENDIF
        IF (SN) THEN
          IF (BTEST(TRGBITS(4),0)) NUM_LOW = NUM_LOW + 1
          IF (BTEST(TRGBITS(4),1)) NUM_LOW = NUM_LOW + 2
        ENDIF
        IF (SS) THEN
          IF (BTEST(TRGBITS(5),0)) NUM_LOW = NUM_LOW + 1
          IF (BTEST(TRGBITS(5),1)) NUM_LOW = NUM_LOW + 2
        ENDIF
C
        IF (PTCUT.EQ.'LOW') THEN
          IF (NUM_LOW.GE.NUM_L15_MUONS) THEN
            RESULT_FLAG = .TRUE.
          ENDIF
        ENDIF
C
        IF (PTCUT.EQ.'HIGH') THEN
          IF (NUM_HIGH.GE.NUM_L15_MUONS) THEN
            RESULT_FLAG = .TRUE.
          ENDIF
        ENDIF
C
      ENDIF  ! MUON PART
C
C
C*******************   L15 CALORIMETER CONFIRM
C
      IF (CONFIRM_TYPE.EQ.'CAL') THEN
C
C   Get L1 bit mask for each term and put into array L15_TERM_MASK
C
        IF (FIRST) THEN
          FIRST = .FALSE.
          CALL EZPICK('AND_OR')
          CALL EZGETA('AND_OR_USED',I_AND_OR_USED_SKIP+L15_TERM_BEG,
     &      I_AND_OR_USED_SKIP+L15_TERM_END,1,L1BITS_PER_L15TERM,IER)
          IF (IER.NE.0) THEN
            CALL ERRMSG('L2_CONFIRM_L15CAL','L2_CONFIRM_L15',
     &                'UNABLE TO READ L15 ANDOR TERMS','F')
            GOTO 999
          ENDIF
          CALL EZRSET
        ENDIF
C
C   Which L1 bit correspondes to this tool call?
C
        NBITS_ON = 0
        DO I = 0,MAX_L15BIT
          IF (BTEST(HARDWARE,I)) THEN
            NBITS_ON = NBITS_ON + 1
            L1BIT_ON = I
          ENDIF
        ENDDO
C
C   Should be one and only one L1 bit...
C
        IF (NBITS_ON.EQ.0) THEN
          CALL ERRMSG('L2_CONFIRM_L15CAL','L2_CONFIRM_L15',
     &                'NO LEVEL 1 BITS TO CONFIRM','E')
          GOTO 999
        ENDIF
        IF (NBITS_ON.GT.1) THEN
          CALL ERRMSG('L2_CONFIRM_L15CAL','L2_CONFIRM_L15',
     &                'TOO MANY LEVEL 1 BITS TO CONFIRM','E')
          GOTO 999
        ENDIF
C
C   Which terms should be on for this Level 1 bit?
C
        NTERMS = 0
        DO L15TERM = L15_TERM_BEG, L15_TERM_END
          IF (BTEST(L1BITS_PER_L15TERM(L15TERM),L1BIT_ON)) THEN
            NTERMS = NTERMS + 1
            L15TERMS_ON(NTERMS) = L15TERM
          ENDIF
        ENDDO
C
        IF (NTERMS.EQ.0) THEN
          CALL ERRMSG('L2_CONFIRM_L15CAL','L2_CONFIRM_L15',
     &                'NO L15 TERMS TO CONFIRM','E')
          GOTO 999
        ENDIF
C
C   Get L15 term mask from Frame part of l15cal crate
C
        CALL L15EXTRACT_L15CAL_FRAME(1,L15_FRAME_SECTION)
        L15_TERM_MASK = L15_FRAME_SECTION(6)
C
        CALL MVBITS(L15_TERM_MASK,0,9,L15TERMS_TRIED,0)
        CALL MVBITS(L15_TERM_MASK,9,7,L15TERMS_PASSED,0)
        CALL MVBITS(L15_TERM_MASK,16,8,L15TERMS_INVAL,0)
        CALL MVBITS(L15_TERM_MASK,24,8,L15TERMS_RET,0)
C
C   Did the L15 machine try to evaluate the L15 terms (array L15TERMS_ON)?
C
        DO I = 1, NTERMS
          IF (.NOT.BTEST(L15TERMS_TRIED,L15TERMS_ON(I))) THEN
            CALL ERRMSG('L2_CONFIRM_L15CAL','L2_CONFIRM_L15',
     &         'MISMATCH OF TERMS BETWEEN ANDOR AND L15 BLOCKS','E')
            GOTO 999
          ENDIF
        ENDDO
C
C   Did the terms Pass L15?
C
        PASSED_L15CAL = .TRUE.
        I = 1
        DO WHILE ((PASSED_L15CAL).AND.(I.LE.NTERMS))
          PASSED_L15CAL = .TRUE.
          IF (.NOT.BTEST(L15TERMS_PASSED,L15TERMS_ON(I))) THEN
            PASSED_L15CAL = .FALSE.
C
            IF (BTEST(L15TERMS_INVAL,L15TERMS_ON(I))) THEN
              CALL ERRMSG('L2_CONFIRM_L15CAL','L2_CONFIRM_L15',
     &             'L15 DECISION NOT COMPLETE FOR THIS TRIGGER','E')
              PASSED_L15CAL = .TRUE.
            ENDIF
C
          ENDIF
          I = I + 1
        ENDDO
C
        RESULT_FLAG = PASSED_L15CAL
C
      ENDIF  ! CAL PART
C
C
C
  999 CONTINUE
      IF (OK) CALL EZRSET
      RETURN
      END
