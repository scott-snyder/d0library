      SUBROUTINE CAEPFL_PEDGNS(RCP_BANK,CRATE,ADDR,SCALE,
     &  ENERGY,IER)
C--------------------------------------------------------------
C-
C-   Purpose and Methods : Returns  ENERGY with PEDESTAL subtracted and
C-           electronics GAIN corrected. RCP_BANK controls pedestals and gains
C-            
C-
C-   Inputs  : RCP_BANK [C] = RCP CONTROL BANK
C-             CRATE    [I] = CRATE NUMBER
C-                            For CAD1=07,17,27,37,47,57;CAD2=08,18,28,38,48,58
C-                            5000CH TEST=67; QUADRANT TEST=77; TB91 LOAD 2 =87
C-             ADDR     [I] = 0,6144 - CAD WORD ADDRESS
C-             SCALE    [I] = CAD WORD SCALE 0=X8,1=X1
C-             ENERGY   [R] = RAW ADC COUNTS from CAD bank 
C-                          = Gain corrected & pedestal subtracted on OUTPUT
C-   Ouput:    IER      [I] = 0 :  OK
C-                          = -1 :  BAD PEDESTAL /GAIN (SKIP THIS CHANNEL)
C-                          = -2 :  EVEN WORSE PEDESTAL /GAIN (SKIP THIS EVENT)
C-   Controls: RCP_BANK,TASK
C-
C-   Created   3-JAN-1991   Chip Stewart
C-   Updated  17-Mar-1992   Herbert Greenlee Removed machine blocks
C-   Updated  15-APR-1992   Chip Stewart  - removed limit checks - 
C-                                now done by CPB and CGB bits
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCP_BANK
      INTEGER CRATE,ADDR,IER
      REAL    ENERGY
      INCLUDE 'D0$INC:CUNFLG.INC'               ! CAD bank flags
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'     ! CAD bank params
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'    ! CAL UNPACKING params
      INCLUDE 'D0$INC:ZEBSTP.INC'
C CHARACTER STRINGS
      CHARACTER*80 MSG_STRING,GAIN_FILE,PED_FILE
      CHARACTER*40 MSG,ERR_ID
C INTEGERS
      INTEGER I,J,K,M,N
      INTEGER ICABLE, ICRATE, DEPTH, SCALE,ADC_CARD  ! CAD addresses
      INTEGER HEADER_LEN,SYNCH,CONTROL_WORD,VERSION,STATUS,PULSER
      INTEGER SEQ, TASK, PED_TYPE, JCRATE, CRATES(25), NCRATE,ICAD
      INTEGER LCPD1,LCPD8,GZCPD8,GZCPD1, GZCPDH ! ped bank addresses
      INTEGER LCGN1,LCGN8,GZCGN8,GZCGN1, GZCGNH ! GAIN bank addresses
      INTEGER IOR,ISHFT,IAND,IBITS                   ! VAX functions
      INTEGER HIGH_PED_1,HIGH_PED_8
      INTEGER LOW_PED_1,LOW_PED_8,ERRMSG_LIMIT,NERR
C LOGICALS
      LOGICAL FIRST,DO_PEDSUB,DO_GNSCOR,DO_CPZ,DO_ZSUPR
      LOGICAL EZERR
C REALS
      REAL PEDESTAL,SIGMA,SIGMA_CUT
      REAL GAIN,GAIN_NORM,LOW_GAIN,HIGH_GAIN
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK(RCP_BANK)
        IF( .NOT. EZERR(IER) ) THEN
          CALL EZGET_l('DO_GNSCOR',DO_GNSCOR,IER)
          CALL EZGET_i('ERRMSG_LIMIT_PDGN',ERRMSG_LIMIT,IER)
          IF(DO_GNSCOR)THEN
            CALL EZ_GET_CHARS('GAINS_FILE',N,GAIN_FILE,IER)
            IF (IER.NE.0) THEN
              GAIN_FILE = 'DBL3'
              CALL ERRMSG('NO GAIN FILE in RCP ','CAEPFL_PEDGNS',
     &        ' USE NOMINAL DBL3 ','W')
            END IF
            CALL EZGET('GAIN_NORMALIZATION',GAIN_NORM,IER)
            IF (IER.NE.0) THEN
              GAIN_NORM = 1900.
              CALL ERRMSG('NO GAIN NORMALIZATION in RCP ',
     &          'CAEPFL_PEDGNS', ' USE NOMINAL 1900 ','W')
            END IF
            CALL EZGET('LOW_GAIN_CUT',LOW_GAIN,IER)
            IF (IER.NE.0) LOW_GAIN = 0
            CALL EZGET('HIGH_GAIN_CUT',HIGH_GAIN,IER)
            IF (IER.NE.0) HIGH_GAIN = 50000
          END IF
          CALL EZGET_l('DO_PEDSUB',DO_PEDSUB,IER)
          IF(DO_PEDSUB)THEN
            CALL EZ_GET_CHARS('PEDESTAL_FILE',N,PED_FILE,IER)
            IF (IER.NE.0) THEN
              PED_FILE = 'DBL3'
              CALL ERRMSG('NO PED FILES in RCP ','CAEPFL_PEDGNS',
     &        ' USE NOMINAL DBL3 ','W')
            END IF
            CALL EZGET_l('USE_CPZ_BANK',DO_CPZ,IER)
            IF (IER.NE.0) THEN
              DO_CPZ = .FALSE.
            END IF
            CALL EZGET_i('LOW_PED_CUT*1',LOW_PED_1,IER)
            IF (IER.NE.0) LOW_PED_1 = 0
            CALL EZGET_i('HIGH_PED_CUT*1',HIGH_PED_1,IER)
            IF (IER.NE.0) HIGH_PED_1 = 100000
            CALL EZGET_i('LOW_PED_CUT*8',LOW_PED_8,IER)
            IF (IER.NE.0) LOW_PED_8 = 0
            CALL EZGET_i('HIGH_PED_CUT*8',HIGH_PED_8,IER)
            IF (IER.NE.0) HIGH_PED_8 = 100000
            CALL EZGET_l('DO_ZERO_SUPRESS',DO_ZSUPR,IER)
            IF (IER.NE.0) THEN
              DO_ZSUPR = .TRUE.
              CALL ERRMSG('NO ZERO_SUPRESS FLAG in RCP ',
     &          'CAEPFL_PEDGNS', ' DO ZERO SUPRESS ','W')
            END IF
            CALL EZGET('SIGMA_CUTOFF',SIGMA_CUT,IER)
            IF (IER.NE.0) THEN
              SIGMA_CUT =1.0
              CALL ERRMSG('NO SIGMA_CUTOFF in RCP ',
     &          'CAEPFL_PEDGNS', ' SIGMA_CUT = 1.0 ','W')
            END IF
          END IF
          CALL EZRSET
        ELSE
          CALL ERRMSG('NO RCP BANK','CAEPFL_PEDGNS',
     &        ' USE NOMINAL VALUES ','W')
          DO_PEDSUB = .TRUE.
          DO_GNSCOR = .TRUE.
          DO_ZSUPR  = .TRUE.
          GAIN_FILE= 'DBL3'
          PED_FILE= 'DBL3'
          LOW_GAIN = 0
          HIGH_GAIN = 20000
          GAIN_NORM = 1900.
          HIGH_PED_1 = 10000
          LOW_PED_1 = 0
          HIGH_PED_8 = 10000
          LOW_PED_8 = 0
          DO_CPZ = .FALSE.
          SIGMA_CUT = 1.0
        END IF
C
C ****  SET TASK
C
        TASK = -1
        IF( DO_GNSCOR) THEN
          TASK = 3
          IF ( DO_PEDSUB ) TASK =0
        ELSE IF ( DO_PEDSUB ) THEN
          TASK =1
        ELSE IF ( PEDSUB .AND. DO_PEDSUB ) THEN
          CALL ERRMSG(' CAD DATA ALREADY PED SUBTRACTED ',
     &        'CAEPFL_PEDGNS',' NO CPDH/CPZH PEDS USED ','W')
          DO_PEDSUB = .FALSE.
        END IF
C
C ****  Get PEDS and GAINS
C
        IF ( TASK .NE.-1 ) THEN ! FIND OUT IF PED/GNS NEED TO BE READ IN
          LCPD8 = GZCPD8 ()
          LCPD1 = GZCPD1 ()
          LCGN8 = GZCGN8 ()
          LCGN1 = GZCGN1 ()
          IF ( ( DO_PEDSUB.AND.(LCPD8.LE.0.OR.LCPD1.LE.0 ) )
     &      .OR. ( DO_GNSCOR.AND.(LCGN8.LE.0.OR.LCGN1.LE.0 ) ) ) THEN
C
C ****  LOOP OVER CAD BANKS LOOKING FOR CRATES TO FILL
C
          NCRATE = 0
          ICAD =1
          JCRATE = 0
CWORRY ABOUT 5000 CH ,TB, QUAD TEST .... CRATES
   22     JCRATE = JCRATE -1
          CALL GTCAD_HEADER(ICAD,JCRATE, HEADER_LEN,SYNCH,
     &                  CONTROL_WORD,VERSION,STATUS,PULSER,IER)
          IF (IER.EQ.0) THEN
            NCRATE = NCRATE + 1
            CRATES(NCRATE) = CRT
            GOTO 22
          ELSE IF(ICAD.EQ.1) THEN
            ICAD = 2
            JCRATE = 0
            GOTO 22
          END IF
          IER = 0
C
C ****  READ IN PEDESTALS AND/OR GAINS
C
          IF ( DO_PEDSUB.AND.(LCPD8.GT.0.OR.LCPD1.GT.0 ) )
     &      CALL ERRMSG(' EXISTING PED BANKS ',
     &      'CAEPFL_PEDGNS',' CPD1 & CPD8 OVERWRITEN ','W')
          END IF
          IF ( DO_GNSCOR.AND.(LCGN8.GT.0.OR.LCGN1.GT.0 ) )
     &      CALL ERRMSG(' EXISTING GNS BANKS ',
     &      'CAEPFL_PEDGNS',' CGN1 & CGN8 OVERWRITEN ','W')
C------------------------------------------------------------------------
C
          CALL CALOR_READ_PEDGNS(PED_FILE,GAIN_FILE,TASK,
     &          NCRATE,CRATES,IER)
C
C------------------------------------------------------------------------
          IF(TASK.EQ.0 .OR. TASK.EQ.1) THEN   ! PEDESTALS
            LCPD8 = GZCPD8 ()
            LCPD1 = GZCPD1 ()
            IF ( LCPD8.LE.0 .OR. LCPD1.LE.0) THEN
              DO_PEDSUB = .FALSE.
              PEDESTAL = 0
              CALL ERRMSG('NO CPD1/CPD8 BANKS READ',
     &            'CALOR_PEDGNS','NO PED SUBTRACTION','W')
              CALL EZPICK(RCP_BANK)
              CALL EZSET('DO_PEDSUB',DO_PEDSUB,IER)
              CALL EZRSET
            END IF
C
C ****  SETUP CPZ PEDESTALS
C
            PED_TYPE = 1
            IF (DO_CPZ) THEN
              PED_TYPE = 2
              SIGMA_CUT = 1.0
              CALL INTMSG(' USE_CPZ_BANK - SETTING SIGMA_CUT TO 1.0')
            END IF
          END IF
        END IF
        IF (DO_GNSCOR) THEN
          LCGN8 = GZCGN8 ()
          LCGN1 = GZCGN1 ()
          IF ( LCGN8.LE.0 .OR. LCGN1.LE.0) THEN
            DO_GNSCOR = .FALSE.
            GAIN = 1.0
            CALL ERRMSG(' NO CGN1/CGN8 BANKS READ',
     &            'CALOR_PEDGNS',' NO GNS CORRECTIONS ','W')
            
            CALL EZPICK(RCP_BANK)
            CALL EZSET('DO_GNSCOR',DO_GNSCOR,IER)
            CALL EZRSET
          END IF
        END IF
      END IF
C
C ****  EACH CHANNEL
C
      ADC_CARD = ISHFT (ADDR,-9)
      DEPTH    = IAND (ADDR,15)            ! JBYT(ADR,3,4)
      SEQ      = IBITS (ADDR,4,5)  *NDEPTC + DEPTH
C
      IF (.NOT.PEDSUB .AND. DO_PEDSUB ) THEN
        CALL GT_PED_GNS_ADDR(PED_TYPE,CRATE,ADC_CARD,SEQ,SCALE,
     &          PEDESTAL,SIGMA,IER)
        IF (SIGMA.LE.0) THEN
          IER = -1
          WRITE(msg, 17)ADDR
          CALL ERRMSG('BAD SIGMA','CAEPFL_PEDGNS',MSG,'W')
          GOTO 999
        END IF
        IF (IER.EQ.0) THEN
          IF  ( ( (SCALE.EQ.0).AND.
     &       (PEDESTAL.GT.HIGH_PED_8.OR.PEDESTAL.LT.LOW_PED_8) )
     &     .OR. ( (SCALE.EQ.1).AND.
     &       (PEDESTAL.GT.HIGH_PED_1.OR.PEDESTAL.LT.LOW_PED_1) ) ) THEN
            IF (ERRMSG_LIMIT.GE.0) THEN
              ERR_ID = 'BAD-PEDESTAL'
              WRITE(MSG,1006)PEDESTAL
 1006         FORMAT(' PEDESTAL=',E10.3)
              WRITE(ERR_ID,17)CRATE,ADDR,SCALE
   17         FORMAT(' BAD PED ',I4,Z5.4,I3)
              CALL ERRMAX(ERR_ID,1,ERRMSG_LIMIT)
              CALL ERRMSG(ERR_ID,'CAEPFL_PEDGNS',MSG,'W')
            END IF
            IER = -1
            GOTO 999
          END IF
C------------------------------------------------------------------------
C
          ENERGY = ENERGY - PEDESTAL
C
C------------------------------------------------------------------------
          IF(DO_ZSUPR .AND. ABS(ENERGY/SIGMA).LT.SIGMA_CUT) THEN
            IER = -1
            GOTO 999
          END IF
        ELSE
          GOTO 999
        END IF
      END IF
      IF (.NOT.GNCORR .and. DO_GNSCOR ) THEN
        CALL GT_PED_GNS_ADDR(3,CRATE,ADC_CARD,SEQ,SCALE,
     &          GAIN,SIGMA,IER)
        IF (IER.EQ.0) THEN
          IF ( GAIN.LT.LOW_GAIN .or. GAIN.GT.HIGH_GAIN ) THEN
            IF (ERRMSG_LIMIT.GE.0) THEN
              ERR_ID = 'BAD-GAIN'
              WRITE(ERR_ID,18)CRATE,ADDR,SCALE
   18         FORMAT(' BAD GN ',I4,Z5.4,I3)
              CALL ERRGET(ERR_ID,NERR)
C
C ****  This part of code is slow - risk missing error messages on 
C ****  subsequent events by SKIPPING after NERR.GT.ERRMSG_LIMIT
C
              IF (NERR.GT.(ERRMSG_LIMIT+1)) THEN
                ERRMSG_LIMIT = -1
                IER = -1
                GOTO 999
              END IF
              WRITE(MSG,1007) GAIN
 1007         FORMAT(' GAIN=',E9.2)
              CALL ERRMAX(ERR_ID,1,ERRMSG_LIMIT)
              CALL ERRMSG(ERR_ID,'CAEPFL_PEDGNS',MSG,'W')
	    END IF
            IER = -1
            GOTO 999
          END IF
C------------------------------------------------------------------------
C
          ENERGY = GAIN_NORM * ENERGY / GAIN
C
C------------------------------------------------------------------------
        ELSE
          GOTO 999
        END IF
      END IF
  999 RETURN
C#######################################################################
      ENTRY RESET_CAEPFL_PEDGNS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Forces CAEPFL_PEDGNS to read in new
C-                         pedestal/gains banks from DBL3 on subsequent 
C-                         call. To be used at end/begining of runs to 
C-                         allow for different calib files for different
C-                         runs.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   7-JAN-1992   Chip Stewart
C-
C----------------------------------------------------------------------
      FIRST= .TRUE.
C
C ****  drop CALIB banks
C
      LCPDH = GZCPDH ()
      IF (LCPDH.GT.0) CALL MZDROP (IXSTP,LCPDH,' ')   ! Drop pedestal bank(s)
      LCGNH = GZCGNH ()
      IF (LCGNH.GT.0) CALL MZDROP (IXSTP,LCGNH,' ')   ! Drop SRCP bank(s)
      END
