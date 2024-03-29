      FUNCTION TB90_STATUS_PAGE ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : prints out status page at EXM_END_ANALYSIS hook
C-
C-   Returned value  : true if OK
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   2-AUG-1990   Chip Stewart
C-   Updated  22-MAR-2004   sss - compile with g77
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL TB90_STATUS_PAGE
      LOGICAL TB90_STATUS_PAGE_EVENT
      LOGICAL TB90_STATUS_PAGE_READ_EVENT
      LOGICAL TB90_STATUS_PAGE_BEGIN
      LOGICAL TB90_STATUS_PAGE_INI
      LOGICAL EZERR,DAQ_FLAG,FILE_FLAG, FLGVAL, OK
      LOGICAL FIRST, DO_COUNT,DO_CUTS,DO_RESULTS,DO_UNPACKING
      LOGICAL DO_EPIC,DO_HVMON,DO_PREA_TEMP,DO_BLS_TEMP
      LOGICAL DO_LAR_TEMP,DO_LAR_PUR
      INCLUDE 'D0$ERRMSG_UTIL$PARAMS:ERRMSG.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$INC:ERRCTL.INC'
      INCLUDE 'D0$INC:ERRCNT.INC'
      INCLUDE 'D0$INC:ERRIDS.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      CHARACTER DAYTIME*30,FILENAME*50
      CHARACTER DET_CODE*80,OPERS*80,OWNER*80,KEYWORD*80,COMMENT*80
      INTEGER I,J,K,L,N,NUNIT,IER, RUN,EVENT,SIZE
      INTEGER CAL_BEAM, CAL_BEAM_PEDS
      INTEGER INFO_ID,LOG_BOOK,LOG_PAGE
C
C
      CHARACTER CBADDR*4, CCUSED*10,CPEDGAIN*12
      INTEGER IKADDR,ISEQ
      INTEGER CONINT,CONHEX,ICUSED
      INTEGER ISCL,IDEP,ITOW,IBLS,IADC
      INTEGER NUMG,ETAV(20),PHIV(20),LAYERV(20)
C
C
C CALOR_HIST
      REAL    Gain_10pf(4),SFRAC_CORR(17)
      REAL    UPSTR_CUT,LOW_CRAZY,HIGH_CRAZY
      REAL    ETA_BEAM,ETA_RANGE,PHI_BEAM,PHI_RANGE
      REAL    TIGHT_ETA_RANGE,TIGHT_PHI_RANGE
      REAL    ETA_IH_RANGE,PHI_IH_RANGE
C ACCOUNTING
      INTEGER PWC_COUNT,IOS
      REAL    MOM_AVE,CERENA,CEREN2,CEREN9
C CALOR_UNPACK
      INTEGER HIGH_PEDESTAL1,LOW_PEDESTAL1
      INTEGER HIGH_PEDESTAL8,STATUS_MASK,LOW_PEDESTAL8
      INTEGER NCHAR,GAIN_RUN,PED_RUN
      REAL ECUTOF,SCUTOF,ADCGEV
      REAL GAIN_NORM,LOW_GAIN,HIGH_GAIN
      LOGICAL CONGEV,ZSUP
      LOGICAL DO_PEDSUB,DO_GNSCOR,CPZ,DUMP,DO_CONNECTED
      CHARACTER*50 PED_FILE,GAIN_FILE
C EPICS
      REAL BEAMOM,NW4W, NW8Q1, NW8Q2,NW9E,NW9V, NW9EVCC, NWAEVCC
      REAL NW4CON,NW6CON, NW7CON,NWASC2
C HVMON
      INTEGER VOLTAGE(36),CURRENT(36)
C PREA_TEMP
      REAL TEMPERATURE(60)
C PWC
      INTEGER  NTY,NTXD,NTXU,NHY,NHXD,CHY,CHXD
      INTEGER  NTR, NWTRK,NP
      REAL     YT(5),DXT(5),UXT(4),MOM(4)
C TAG
      INTEGER   TAGWORD,HALL
      INTEGER   TDC(50),ADC(50),LDTDC(50)
      INTEGER   NADC,NTDC,NSCALER,NLDTDC
      INTEGER   SCALER(50)

C----------------------------------------------------------------------
C                        Status Page Format
C
C       Date time
C       Run Number
C       Begin Run Comment (logger$brd:RCP_BINFO_*.DAT)
C       Number of Beam events - Pedestal events
C
C       Analysis Cuts:
C           Beam eta, phi
C           Sampling fractions (10pf corr)
C           eta, phi region cuts
C           Crazy gains,peds,energy cuts
C
C       Analysis Results:
C           PWC efficiency
C           Cernekov Tag efficiency
C           Average Beam momemtum
C
C       Unpacking Status:
C           Zero Suppression (on/off CAD,ECUT,SCUT,CPZ values)
C           GAIN correction (on/off, GAIN file DBL3/run number)
C           Pedestal Subtraction (on/off, PED file DBL3/run number)
C
C       Error Summary:
C           Crazy Energy count
C           Unpacking/Electronics error count
C
C       Hardware  Status:
C           NW9  current   (logger$brd:EPIC.rcp)
C           Converter wheel position
C           Cerenkov Pressures(2)
C           HV  readout    (say  3)   each from EM,IH,MH (logger$brd:HVMON.rcP)
C           Temperatures (BLS,  Preamps -  logger$brd:prea_temp.rcp last 6 words)
C
C
C ****  GET RCP FILES DEPENDING ON FILE OR DAQ
C
      TB90_STATUS_PAGE = .FALSE.

      DAYTIME= ' '
C&IF LINUX
C&      call fdate (daytime)
C&ELSE
      CALL DATE (DAYTIME(1:15))
      CALL TIME (DAYTIME(17:26))
C&ENDIF
      CALL STRINT('RUN',RUN,FILENAME,I)
      FILENAME='USR$OUT:'//FILENAME(1:I)//'_STATUS.SUM'
      CALL GTUNIT(77,NUNIT,IER)
      CALL D0OPEN(NUNIT,FILENAME,'OF',OK)
      CALL INTMSG(' STATUS PAGE WRITING TO '//FILENAME)
C
      WRITE (NUNIT,200) RUN,DAYTIME
  200 FORMAT(1X,'  EXAMINE RUN ',I8,20X,A30)
C
      CALL EZPICK('BINF')
      IF(EZERR(IER)) THEN
        WRITE (NUNIT,2240)
 2240   FORMAT(5X,'NO BINF RCP BANK')
        COMMENT = '                NO COMMENT'
      ELSE
        CALL EZGET_i('INFO_ID',         INFO_ID,   IER)
        CALL EZGETS('DETECTOR_CODE',N,DET_CODE,L,IER)
        CALL EZGETS('OPERATORS',    N,OPERS,   L,IER)
        CALL EZGETS('RUN_OWNER',    N,OWNER,   L,IER) 
        CALL EZGET_i('LOG_BOOK',        LOG_BOOK,  IER)
        CALL EZGET_i('PAGE_NUMBER',     LOG_PAGE,  IER)
        CALL EZGETS('RUN_KEYWORD',  N,KEYWORD, L,IER)
        CALL EZGETS('BEGIN_COMMENT',N,COMMENT, L,IER)
      ENDIF
      CALL EZRSET
      WRITE (NUNIT,210) COMMENT
  210 FORMAT(1X,A79)
C
      IF(DO_COUNT) THEN
        WRITE (NUNIT,220) CAL_BEAM, CAL_BEAM_PEDS
  220   FORMAT(1X,'EVENTS  CAL_BEAM ',I7,5X,' CAL_BEAM_PEDS ',I7)
      END IF
C
      IF(DO_CUTS) THEN
        WRITE (NUNIT,230)
  230   FORMAT(/1X,'ANALYSIS CUTS:')
C
        CALL EZPICK('TB90_CALOR_HIST_RCP')
        IF(EZERR(IER)) THEN
          WRITE (NUNIT,240)
  240     FORMAT(5X,'NO TB90_CALOR_HIST_RCP BANK')
        ELSE
          CALL EZGET('UPSTREAM_ENERGY_CUT',UPSTR_CUT,IER)
          CALL EZGET_rarr('CAPACITANCE_GAIN_CORR',GAIN_10PF,IER)
          CALL EZGET_rarr('SAMPLING_FRACTION_CORR',SFRAC_CORR,IER)
          CALL EZGET('LOW_ENERGY_CRAZY_LIMIT',LOW_CRAZY,IER)
          CALL EZGET('HIGH_ENERGY_CRAZY_LIMIT',HIGH_CRAZY,IER)
          CALL EZGET('ETA_OF_BEAM',ETA_BEAM,IER)
          CALL EZGET('ETA_RANGE_EM_SHOWER',ETA_RANGE,IER)
          CALL EZGET('TIGHT_ETA_RANGE_EM_SHOWER',TIGHT_ETA_RANGE,IER)
          CALL EZGET('ETA_RANGE_HAD_SHOWER',ETA_IH_RANGE,IER)
          CALL EZGET('PHI_OF_BEAM',PHI_BEAM,IER)
          CALL EZGET('PHI_RANGE_EM_SHOWER',PHI_RANGE,IER)
          CALL EZGET('TIGHT_PHI_RANGE_EM_SHOWER',TIGHT_PHI_RANGE,IER)
          CALL EZGET('PHI_RANGE_HAD_SHOWER',PHI_IH_RANGE,IER)

          WRITE (NUNIT,250) ETA_BEAM,PHI_BEAM
  250     FORMAT(5X,' ETA ',F10.3,' PHI ',F10.3)

          WRITE (NUNIT,270) ETA_RANGE,TIGHT_ETA_RANGE,ETA_IH_RANGE
  270     FORMAT(5X,' ETA RANGE (EM) ',F8.3,5X,' (TIGHT) ',F8.3,5X,
     &      ' (IH) ',F8.3)

          WRITE (NUNIT,280) PHI_RANGE,TIGHT_PHI_RANGE,PHI_IH_RANGE
  280     FORMAT(5X,' PHI RANGE (EM) ',F8.3,5X,' (TIGHT) ',F8.3,5X,
     &      ' (IH) ',F8.3)

          WRITE (NUNIT,290) LOW_CRAZY,HIGH_CRAZY
  290     FORMAT(5X,' CELL ENERGY CUT LOW ',F10.3,10X,' HIGH ',F10.3)

          WRITE (NUNIT,260) SFRAC_CORR,GAIN_10PF
  260     FORMAT(5X,' SAMPLING FRACTIONS: ',/7X,9F8.2,/7X,8F8.2,
     &      /5X,' CAPACITANCE FACTORS ',/7X,5F8.2)

        END IF
      END IF
      CALL EZRSET
      IF (DO_RESULTS) THEN
        WRITE (NUNIT,500)
  500   FORMAT(/1X,'ANALYSIS RESULTS:')

        WRITE (NUNIT,510) CAL_BEAM
  510   FORMAT(5X,' EVENTS ANALYSED ',I7)

        IF( CAL_BEAM.GT. 0) THEN

          WRITE (NUNIT,520) (PWC_COUNT +0.0) / CAL_BEAM
  520     FORMAT(5X,' PWC EFFICIENCY ',F12.3,' (TRACKS FOUND /EVENTS) ')

          IF(PWC_COUNT.GT.0)WRITE (NUNIT,540) MOM_AVE / PWC_COUNT
  540     FORMAT(5X,' PWC MOMENTUM AVERAGE (GEV)',F12.3)

          WRITE (NUNIT,550) (CERENA+0.0)/CAL_BEAM,
     &    (CEREN2+0.0)/CAL_BEAM ,(CEREN9+0.0)/CAL_BEAM
  550     FORMAT(5X,' CERENKOV EFFICIENCIES  A =',
     &    F10.3,'  2 =',F10.3,'  9 =',F10.3)

        END IF
      END IF

      IF( DO_UNPACKING ) THEN
        WRITE (NUNIT,295)
  295   FORMAT(/1X,'UNPACKING STATUS:')
        CALL EZPICK('TB90_CALOR_UNPACK_RCP')
        IF (EZERR(IER)) THEN
          IF(CPZ) WRITE (NUNIT,296)
  296     FORMAT(5X,' NO TB90_CALOR_UNPACK_RCP BANK')
        ELSE
          CALL EZGET('ENERGY_CUTOFF',ECUTOF,IER)
          CALL EZGET('SIGMA_CUTOFF',SCUTOF,IER)
          CALL EZGET_l('CONVERT_TO_GEV',CONGEV,IER)
          CALL EZGET_l('ZERO_SUPRESS',ZSUP,IER)
          CALL EZGET_l('USE_CPZ_BANK',CPZ,IER)
          CALL EZGET_l('DO_PEDSUB',DO_PEDSUB,IER)
          CALL EZGET_l('DO_GNSCOR',DO_GNSCOR,IER)
          CALL EZGET('GAIN_NORMALIZATION',GAIN_NORM,IER)
          CALL EZGET('LOW_GAIN_CUT',LOW_GAIN,IER)
          CALL EZGET('HIGH_GAIN_CUT',HIGH_GAIN,IER)
          CALL EZGET_i('LOW_PED_CUT*1',LOW_PEDESTAL1,IER)
          CALL EZGET_i('HIGH_PED_CUT*1',HIGH_PEDESTAL1,IER)
          CALL EZGET_i('LOW_PED_CUT*8',LOW_PEDESTAL8,IER)
          CALL EZGET_i('HIGH_PED_CUT*8',HIGH_PEDESTAL8,IER)
          CALL EZGET_i('STATUS_MASK',STATUS_MASK,IER)
          CALL EZ_GET_CHARS('PEDESTAL_FILE',NCHAR,PED_FILE,IER)
          CALL EZ_GET_CHARS('GAINS_FILE',NCHAR,GAIN_FILE,IER)
          CALL EZGET('ADC_TO_GEV',ADCGEV,IER)
          CALL EZGET_rarr('CAPACITANCE_GAIN_CORR',GAIN_10PF,IER)
          CALL EZGET_rarr('SAMPLING_FRACTION_CORR',SFRAC_CORR,IER)

          IF(CPZ) WRITE (NUNIT,298)
  298     FORMAT(5X,' USE CPZ ZERO SUPPRESSION BANKS',
     &       ' FOR PEDS AND/OR ZERO SUPPRESION')

          WRITE (NUNIT,300)DO_PEDSUB
  300     FORMAT(5X,' DO PEDESTAL SUBTRACTION ',L1)
          IF(DO_PEDSUB) THEN
            CALL PEDRNO(PED_RUN)
            WRITE (NUNIT,310)PED_FILE(1:20),PED_RUN
  310       FORMAT(7X,' PEDESTAL FILE ',A20,' PED RUN ',I7)
            WRITE (NUNIT,320)'x1',LOW_PEDESTAL1,HIGH_PEDESTAL1
  320       FORMAT(7X,' ',A2,' PEDESTAL CUTS LOW ',I10,10X,' HIGH ',I10)
            WRITE (NUNIT,320)'x8',LOW_PEDESTAL8,HIGH_PEDESTAL8
          END IF
          WRITE (NUNIT,350)DO_GNSCOR
  350     FORMAT(5X,' DO GAIN CORRECTION ',L1)
          IF(DO_PEDSUB) THEN
            CALL GNSRNO(GAIN_RUN)
            WRITE (NUNIT,360)GAIN_FILE(1:20),GAIN_RUN
  360       FORMAT(7X,' GAIN FILE ',A20,' GAIN RUN ',I7)
            WRITE (NUNIT,370)LOW_GAIN,HIGH_GAIN
  370       FORMAT(7X,' GAIN CUTS LOW ',E10.3,10X,' HIGH ',E10.3)
            WRITE (NUNIT,380)GAIN_NORM
  380       FORMAT(7X,' GAIN NORMALIZATION ',E15.3)
          END IF

          WRITE (NUNIT,400)ZSUP
  400     FORMAT(5X,' DO ZERO SUPRESSION ',L1)
          IF(ZSUP .AND. (.NOT. CPZ) ) THEN
            WRITE (NUNIT,410)SCUTOF,ECUTOF
  410       FORMAT(7X,' SIGMA (PEDESTAL) FACTOR CUT ',F12.3,
     &      ' ENERGY (GEV) CUT ',F12.4)
          END IF

          WRITE (NUNIT,420)CONGEV
  420     FORMAT(5X,' CONVERT FROM ADC COUNTS TO GEV ',L1)
          IF(CONGEV) THEN
            WRITE (NUNIT,430)ADCGEV
  430       FORMAT(7X,' ADC TO GEV CONVERSION FACTOR ', F12.4)
            WRITE (NUNIT,440) SFRAC_CORR,GAIN_10PF
  440       FORMAT(7X,' SAMPLING FRACTIONS: ',/7X,9F8.2,/7X,8F8.2,
     &      /7X,' CAPACITANCE FACTORS ',/7X,5F8.2)
          END IF

        END IF
        CALL EZRSET
C
        WRITE (NUNIT,890)
  890   FORMAT(5X,' UNPACKING ERRORS:')
        IF ( NENTRY .GT. 0 ) THEN
          DO 10 I = 1, NENTRY
            DUMP  = ID(I).EQ.'GTCAD_TOTAL'
            DUMP = DUMP .OR. ID(I).EQ.'BAD-CAFLG'
            DUMP = DUMP .OR. ID(I).EQ.'CAD1-STATUS-BAD'
            DUMP = DUMP .OR. ID(I).EQ.'CAD1-WORDS-BAD'
            DUMP = DUMP .OR. ID(I).EQ.'BAD-CAD1-WORD'
            DUMP = DUMP .OR. ID(I).EQ.'CAD-OVERFLOW'
            IF(DUMP)WRITE(UNIT=NUNIT,FMT=100)  ID(I), COUNT(I)
   10     CONTINUE
        ENDIF
  100   FORMAT(8X,A32,1X,I6,' TIMES')
C
        IF ( NENTRY .GT. 0 ) THEN
          CPEDGAIN = 'BAD-PEDESTAL'
          WRITE (NUNIT,891)'BAD PEDS: ', 'ADC','BLS','TOW','DEP','SC',
     &      'GANG','ETA', 'PHI','LYR','TIMES'
  891     FORMAT(6X,A10,4A4,A3,A10,3A5,4X,A8)
   12     CONTINUE
          DO 11 I = 1, NENTRY
            DUMP  = ID(I)(1:12).EQ.CPEDGAIN
            IF (DUMP)THEN
               CBADDR=ID(I)(14:17)
               IKADDR=CONHEX(CBADDR)
               ISCL=IBITS(IKADDR,1,1)
               IDEP=IBITS(IKADDR,2,4)
               ITOW=IBITS(IKADDR,6,2)
               IBLS=IBITS(IKADDR,8,3)
               IADC=IBITS(IKADDR,11,5)
               ISEQ=IADC*NDEPTC*NEFC*NBLSC + IBLS*NDEPTC*NEFC + 
     X              ITOW*NDEPTC + IDEP + 1
               CALL TB90_SEQPHY(ISEQ,NUMG,ETAV,PHIV,LAYERV)
               ICUSED=1
               WRITE(CCUSED,'(I10)')NUMG
               IF (NUMG.EQ.0)THEN
                  IF (DO_CONNECTED) GOTO 11
                  ICUSED=0
                  CCUSED='NOT USED'
               ENDIF
               IF (NUMG.EQ.0) THEN
                 WRITE(UNIT=NUNIT,FMT=102) 
     &             IADC,IBLS,ITOW,IDEP,ISCL,CCUSED,0,0,0,COUNT(I)
               ELSE
                 WRITE(UNIT=NUNIT,FMT=102) 
     &             IADC,IBLS,ITOW,IDEP,ISCL,CCUSED,ETAV(1),PHIV(1),
     &             LAYERV(1),COUNT(I)
  102              FORMAT(16X,4I4,I3,A10,3I5,4X,I8)
               END IF
            ENDIF
   11     CONTINUE
          IF (CPEDGAIN.EQ.'BAD-PEDESTAL') THEN
            CPEDGAIN = 'BAD-GAIN'
            WRITE (NUNIT,892)
  892       FORMAT(6X,'BAD GAINS:')
            GOTO 12
          END IF
        ENDIF
C
      END IF
C
      WRITE (NUNIT,600)
  600 FORMAT(/1X,'HARDWARE STATUS:')
      IF(DO_EPIC) THEN
        CALL EZPICK('EPIC_RCP')
        IF(EZERR(IER)) THEN
          WRITE (NUNIT,610)
  610     FORMAT(5X,'NO EPIC_RCP BANK')
        ELSE
          CALL EZGET( 'BEAMOM',BEAMOM,IER)
          CALL EZGET( 'NW4W',NW4W,IER)
          CALL EZGET( 'NW8Q1',NW8Q1,IER)
          CALL EZGET( 'NW8Q2',NW8Q2,IER)
          CALL EZGET( 'NW9E',NW9E,IER)
          CALL EZGET( 'NW9V',NW9V,IER)
          CALL EZGET( 'NW9EVCC',NW9EVCC,IER)
          CALL EZGET( 'NWAEVCC',NWAEVCC,IER)
          CALL EZGET( 'NW4CON',NW4CON,IER)
          CALL EZGET( 'NW6CON',NW6CON,IER)
          CALL EZGET( 'NW7CON',NW7CON,IER)
          CALL EZGET( 'NWASC2',NWASC2,IER)
          WRITE (NUNIT,620) BEAMOM
  620     FORMAT(5X,' BEAMLINE MOMENTUM  (GEV): ',F10.2)
          WRITE (NUNIT,630) NW9E,NW9V
  630     FORMAT(5X,' NW9 CURRENTS  E ',E10.2,10X,' V ',E10.2)
          WRITE (NUNIT,640) NW4CON,NW6CON,NW7CON
  640     FORMAT(5X,' CONVERTER WHEEL READBACK NW4 '
     &    ,E8.2,5X,' NW6 ',E8.2,5X,' NW7 ',E8.2)
          WRITE (NUNIT,645) NW9EVCC,NWAEVCC
  645     FORMAT(5X,' CERENKOV PRESSURES NW9 '
     &    ,E10.2,5X,' NWA ',E10.2,5X,' NW2 ',E10.2)
        END IF
        CALL EZRSET
      END IF
      IF(DO_HVMON) THEN
        CALL EZPICK('HVMON_RCP')
        IF(EZERR(IER)) THEN
          WRITE (NUNIT,650)
  650     FORMAT(5X,'NO HVMON_RCP BANK')
        ELSE
          CALL EZGET_iarr( 'VOLTAGE',VOLTAGE,IER)
          CALL EZGET_iarr( 'CURRENT',CURRENT,IER)
          WRITE (NUNIT,660) (I,I=5,35,5),
     &    (VOLTAGE(I),I=5,35,5),(CURRENT(I),I=5,35,5)
  660     FORMAT(5X,' CAL   ',7I7,/7X,' HV  ',7I7,/7X,' CUR ',7I7 )
        END IF
        CALL EZRSET
      END IF
      IF(DO_PREA_TEMP) THEN
        CALL EZPICK('PREA_TEMP_RCP')
        IF(EZERR(IER)) THEN
          WRITE (NUNIT,670)
  670     FORMAT(5X,' NO PREA_TEMP_RCP BANK')
        ELSE
          CALL EZGET_rarr( 'TEMPERATURE',TEMPERATURE,IER)
          WRITE (NUNIT,680) (I,I=14,19),(TEMPERATURE(I),I=14,19)
  680     FORMAT(5X,' PREAMP      ',6I7,/5X,' TEMPERATURE ',6F7.2)
        END IF
        CALL EZRSET
      END IF
      IF(DO_BLS_TEMP) THEN
        CALL EZPICK('BLS_TEMP_RCP')
        IF(EZERR(IER)) THEN
          WRITE (NUNIT,700)
  700     FORMAT(5X,'NO BLS_TEMP_RCP BANK')
        ELSE
          CALL EZGET_rarr( 'TEMPERATURE',TEMPERATURE,IER)
          WRITE (NUNIT,710) (I,I=1,51,10),(TEMPERATURE(I),I=1,51,10)
  710     FORMAT(5X,' BLS         ',6I7,/5X,' TEMPERATURE ',6F7.2)
        END IF
        CALL EZRSET
      END IF
      CALL RLUNIT(77,NUNIT,IER)
      CLOSE(UNIT=NUNIT)
      TB90_STATUS_PAGE = .TRUE.
  999 RETURN
C
      ENTRY TB90_STATUS_PAGE_READ_EVENT ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO BE CALLED FROM EXM_READ_EVENT
C-                         TO COUNT PED & BEAM EVENTS
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created   3-AUG-1990   Chip Stewart
C-   Updated  12-NOV-1990   Chip Stewart   - DO CAL_BEAM IN EVENT HOOK
C-
C----------------------------------------------------------------------
C      IF ( (2.AND.IQ(LHEAD+11) ) .GT.0) CAL_BEAM= CAL_BEAM + 1
C      IF ( (4.AND.IQ(LHEAD+11) ) .GT.0) CAL_BEAM_PEDS= CAL_BEAM_PEDS + 1
C----------------------------------------------------------------------
      TB90_STATUS_PAGE_READ_EVENT = .TRUE.
 1999 RETURN
C
      ENTRY TB90_STATUS_PAGE_EVENT ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO BE CALLED FROM EXM_POST_ANALYSIS
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created   3-AUG-1990   Chip Stewart
C-
C----------------------------------------------------------------------
      RUN = IQ(LHEAD+6)
      EVENT = IQ(LHEAD+9)
      IF ( iand(4,IQ(LHEAD+11) ) .GT.0) CAL_BEAM_PEDS= CAL_BEAM_PEDS + 1
      IF ( iand(2,IQ(LHEAD+11) ) .GT.0) THEN
        CAL_BEAM= CAL_BEAM + 1
      ELSE
        GOTO 2999
      END IF
      CALL GTPWCT(YT,DXT,UXT,NTY,NTXD,NTXU,NHY,NHXD,CHY,CHXD,
     &                  NP,MOM,IER)
      IF(NP.GT.0) THEN
        PWC_COUNT = PWC_COUNT + 1
        MOM_AVE = MOM_AVE + MOM(1)
      END IF

      CALL GTTCMC(TAGWORD,HALL,NADC,ADC,NTDC,TDC,NSCALER,SCALER,
     &                  NLDTDC,LDTDC,IER)

      IF ( iand(2**5, TAGWORD  ) .GT.0) CEREN9 = CEREN9 + 1
      IF ( iand(2**6, TAGWORD  ) .GT.0) CERENA = CERENA + 1
      IF ( iand(2**8, TAGWORD  ) .GT.0) CEREN2 = CEREN2 + 1

C----------------------------------------------------------------------
      TB90_STATUS_PAGE_EVENT = .TRUE.
C----------------------------------------------------------------------
 2999 RETURN

      ENTRY TB90_STATUS_PAGE_BEGIN ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO BE CALLED FROM EXM_BEGIN_RUN
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created   13-AUG-1990   Chip Stewart
C-
C----------------------------------------------------------------------
      TB90_STATUS_PAGE_BEGIN = .TRUE.
      GOTO 4900
 3900 CAL_BEAM = 0
      PWC_COUNT = 0
      MOM_AVE = 0
      CEREN2 = 0
      CERENA = 0
      CEREN9 = 0
      CAL_BEAM = 0
      CAL_BEAM_PEDS = 0
      FILE_FLAG = FLGVAL('SEL_DATA_FILE')
      DAQ_FLAG = FLGVAL('DAQ_DATA')
C
C ****  DROP EXISTING RCP BANKS
C
      CALL EZDROP('HVMON_RCP')
      CALL EZDROP('EPIC_RCP')
      CALL EZDROP('TB_LAR_TEM_RCP')
      CALL EZDROP('BLS_TEMP_RCP')
      CALL EZDROP('TB_LAR_PUR_RCP')
      CALL EZDROP('PREA_TEMP_RCP')
      CALL EZDROP('BINF')
      CALL EZDROP('FILTERS')


      IF(DAQ_FLAG) THEN

C
C ****  GET RCP FILES FROM LOGGER$BRD
C
        IF (DO_HVMON)     CALL INRCP('LOGGER$BRD:HVMON.RCP',IER)
        IF (DO_EPIC)      CALL INRCP('LOGGER$BRD:EPIC.RCP',IER)
        IF (DO_LAR_TEMP)  CALL INRCP('LOGGER$BRD:TB_LAR_TEM.RCP',IER)
        IF (DO_BLS_TEMP)  CALL INRCP('LOGGER$BRD:BLS_TEMP.RCP',IER)
        IF (DO_LAR_PUR)   CALL INRCP('LOGGER$BRD:TB_LAR_PUR.RCP',IER)
        IF (DO_PREA_TEMP) CALL INRCP('LOGGER$BRD:PREA_TEMP.RCP',IER)

      ELSE IF(FILE_FLAG) THEN

        CALL BERD_SRCP

      ELSE

        CALL ERRMSG('NOT FILE OR DAQ','TB90_STATUS_PAGE',
     &    ' NO STATUS PAGE PRINT ','W')
        GOTO 999
      END IF

 3999 RETURN
C----------------------------------------------------------------------
C
      ENTRY TB90_STATUS_PAGE_INI ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO BE CALLED FROM INITIALIZATION
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created   26-NOV-1990   Chip Stewart
C-
C----------------------------------------------------------------------
 4900 TB90_STATUS_PAGE_INI = .TRUE.
      DATA FIRST/ .TRUE. /
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL INRCP('TB90_STATUS_PAGE_RCP',IER)
        IF(EZERR(IER)) THEN
          CALL ERRMSG('NO TB90_STATUS_PAGE_RCP FILE',
     &      'TB90_STATUS_PAGE','USE DEFAULTS ','W')
          GOTO 3999
        END IF
        CALL EZPICK('TB90_STATUS_PAGE_RCP')
        IF(EZERR(IER)) THEN
          CALL ERRMSG('NO TB90_STATUS_PAGE_RCP BANK',
     &      'TB90_STATUS_PAGE','USE DEFAULTS ','W')
          GOTO 3999
        END IF
        CALL EZGET_l('SUM_COUNTS',DO_COUNT,IER)
        IF(IER.NE.0) DO_COUNT = .TRUE.
        CALL EZGET_l('SUM_CUTS',DO_CUTS,IER)
        IF(IER.NE.0) DO_CUTS = .TRUE.
        CALL EZGET_l('SUM_RESULTS',DO_RESULTS,IER)
        IF(IER.NE.0) DO_RESULTS = .TRUE.
        CALL EZGET_l('SUM_UNPACKING',DO_UNPACKING,IER)
        IF(IER.NE.0) DO_UNPACKING = .TRUE.
        CALL EZGET_l('SUM_ONLY_CONNECTED_CHANNELS',DO_CONNECTED,IER)
        IF(IER.NE.0) DO_CONNECTED = .TRUE.
        CALL EZGET_l('SUM_EPIC',DO_EPIC,IER)
        CALL EZGET_l('SUM_HVMON',DO_HVMON,IER)
        CALL EZGET_l('SUM_PREAMP_TEMP',DO_PREA_TEMP,IER)
        CALL EZGET_l('SUM_BLS_TEMP',DO_BLS_TEMP,IER)
        CALL EZGET_l('SUM_LAR_TEMP',DO_LAR_TEMP,IER)
        CALL EZGET_l('SUM_LAR_PUR',DO_LAR_PUR,IER)
        CALL EZRSET
      END IF
      IF(TB90_STATUS_PAGE_BEGIN) GOTO 3900
 4999 RETURN
      END
