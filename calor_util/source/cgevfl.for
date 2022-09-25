      SUBROUTINE CGEVFL(RCP_FILE,LCGEV1)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank CGEV.
C-            CGEVFL 
C-
C-   Inputs  :RCP_FILE [C]  RCP_FILE for control parameters
C-            LCGEV1   [I]  link to first (*8) CGEV bank 
C-                          <=0, routine will check for existing CGEV bank 
C-                               if CGEV exists CGEVFL does nothing
C-                               if no CGEV exists then book and fill CGEV bank 
C-                          > 0, routine will overwrite any existing CGEV bank
C-
C-   Outputs : LCGEV
C-   Controls: RCP_FILE
C-
C-   Created  6-APR-1992   Chip Stewart   
C-   Updated  9-FEB-1993   Joan Guida     Added CGEVFL_HOT
C-   Updated 25-JUN-1993   Chip Stewart/Jan Guida     - VALIDITY RANGE USED
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCP_FILE
      INTEGER LCGEV1
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INCLUDE 'D0$INC:CUNFLG.INC'
      INTEGER IER
      INTEGER MAX_LIST,NBAD,NR,LOC,IBAD(5)
      PARAMETER (MAX_LIST=50)
      INTEGER BAD_LIST(3,MAX_LIST),IETA1,IPHI1,ILYR1,IETA2,IPHI2,ILYR2
      LOGICAL MC,BTEST,EZERR,TB
      CHARACTER CSF_STPFILE*132,ERR_ID*80,CBAD(5)*20,IBM_HAPPY*5
      LOGICAL DO_GNSCOR,DO_PEDSUB,DO_ADC_TO_GEV,BUILD_CSF,TCOR,DO_HOTSUP
      LOGICAL DO_ZERO_SUPRESS,LDD,PSIGMA,DO_PEDSUBL,CEXIST,LGN,LPD
      REAL    EM_GAIN,SIGMA_CUTOF,AWC,ICD_GAIN
      INTEGER LCGEV, GZCGEV,LCSFW,GZCSFW,LCSFC,GZCSFC,LCSFC1,LZFIND
      INTEGER LENF,ICSFW,ICSFC,ICGEV,I,K,LCGEV2,OLD_CAEP
      INTEGER HEADER_LEN,SYNCH,CONTROL_WORD,VERSION,STATUS,PULSER
      INTEGER IETA,IPHI,ILYR,NH,NV,NS,ND,CELL_FBKCAP
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
C ****  READ RCP_FILE
C
        CALL EZLOC(RCP_FILE,LOC)
        IF(LOC.EQ.0) CALL INRCP(RCP_FILE,IER)
        CALL EZPICK(RCP_FILE)
        IF(EZERR(IER)) THEN
          CALL ERRMSG('NO_RCP_FILE_CGEV','CGEVFL',RCP_FILE,'W')
        ELSE
          CALL EZGETS ('CSF_STPFILE',1,CSF_STPFILE,LENF,IER)
          CALL EZGET_l ('DO_GNSCOR',         DO_GNSCOR,IER)
          CALL EZGET ('EM_GAIN',           EM_GAIN,IER)  
          CALL EZGET ('ICD_GAIN',          ICD_GAIN,IER)  
          CALL EZGET_l ('DO_PEDSUB',         DO_PEDSUB,IER)
          CALL EZGET_l ('DO_ADC_TO_GEV',     DO_ADC_TO_GEV,IER)
          CALL EZGET_l ('DO_HOTSUP',         DO_HOTSUP,IER)
          CALL EZGET_l ('CSF_CORRECTIONS',   TCOR,IER)    
          CALL EZGET_l ('BUILD_CSF',         BUILD_CSF,IER)
          CALL EZGET_i ('OLD_CAEP',          OLD_CAEP,IER)
          CALL EZGET_l ('DO_ZERO_SUPRESS',   DO_ZERO_SUPRESS,IER)
          CALL EZGET ('SIGMA_CUTOFF',      SIGMA_CUTOF,IER)
          CALL EZGET_l ('DATA_DRIVE_CSF',    LDD,IER)      
          CALL EZGET_l ('PEDESTAL_SIGMAS',   PSIGMA,IER)
          PSIGMA = PSIGMA.OR.(DO_ZERO_SUPRESS.AND.SIGMA_CUTOF.GT.0)
          CALL EZGET_iarr ('BAD_CHANNELS',      BAD_LIST,IER) 
          CALL EZGET_SIZE ('BAD_CHANNELS', NBAD,IER)     
          IF(MOD(NBAD,3).NE.0) THEN
            CALL ERRMSG('BAD_CHANNELS_SET_WRONG','CGEVFL_PED',
     &        'LIST IN RCP: ETA,PHI,LYR','W')
            NBAD = 0
          END IF
          NBAD = NBAD / 3
        END IF
C
C **** Check CAD bank head 
C
        CALL GTCAD_HEADER (0,0,HEADER_LEN,SYNCH,CONTROL_WORD,
     &  VERSION,STATUS,PULSER,IER)
        CALL CAFLGS(CONTROL_WORD)   ! FILL CUNFLG COMMON WITH FLAGS/SWITCHES
        MC = BTEST(D0VSN,5)         ! Check for Monte Carlo generated CAD bank 
        TB = BTEST(D0VSN,6)         ! Check for NWA data CAD1 bank 
        IF (MC.OR.(OLD_CAEP.EQ.2)) THEN                ! 
          IF((DO_PEDSUB.OR.DO_GNSCOR.OR.PSIGMA).AND.MC) THEN
            CALL ERRMSG('NO_PED_GNS_IN_MC','CGEVFL',
     &      'NO CALIB PEDS/GNS for MC data','I') 
          ELSE IF(DO_PEDSUB.OR.DO_GNSCOR.OR.PSIGMA) THEN
            CALL ERRMSG('NO_PED_GNS_OLD_CAEP','CGEVFL',
     &      'NO CALIB PEDS/GNS for OLD_CAEP=2','W') 
          IF(DO_HOTSUP.AND.MC) THEN
            CALL ERRMSG('NO_HOT_IN_MC','CGEVFL',
     &      'NO HOT channel suppression for MC data','I') 
          END IF
          END IF
          DO_PEDSUB = .FALSE. ! no peds in MC
          DO_GNSCOR = .FALSE. ! no electronics gains in MC
          DO_HOTSUP = .FALSE. ! no "hot" channels in MC
          SIGMA_CUTOF = 0.    ! no pedestal sigma in MC (yet)
          PSIGMA    = .FALSE. ! no pedestal sigma in MC (yet)
          CALL EZPICK(RCP_FILE)
          CALL EZSET_l('DO_PEDSUB',DO_PEDSUB,IER)  ! update RCP_BANK 
          CALL EZSET_l('DO_GNSCOR',DO_GNSCOR,IER)
          CALL EZSET_l('DO_HOTSUP',DO_HOTSUP,IER)
          CALL EZSET('SIGMA_CUTOFF',SIGMA_CUTOF,IER)
          CALL EZSET_l('PEDESTAL_SIGMA',PSIGMA,IER)
          CALL EZRSET
        ELSE IF (TB) THEN
          CALL EZPICK(RCP_FILE)
          IF((.NOT.DO_PEDSUB).AND.(.NOT.PEDSUB))  THEN
            CALL ERRMSG('TB DATA NOT PEDESTAL SUBTRACTED','CGEVFL',
     &      'DO_PEDSUB TURNED ON IN CAHITS_RCP','W') 
            DO_PEDSUB = .TRUE. ! get peds in TB dbl3
            CALL EZSET_l('DO_PEDSUB',DO_PEDSUB,IER)  ! update RCP_BANK 
          END IF
          CALL EZSET_l('NEED_CAD2',.FALSE.,IER) ! update RCP_BANK 
          CALL EZRSET
        END IF
C
        CALL EZRSET
C
C ****  Set repeat words in CGEV
C
        NR = 1
        IF (PSIGMA) NR = 2
        IF (DO_PEDSUB) NR = 3
      ENDIF
C
C **** Book CGEV bank
C
      LGN = .FALSE.
      LCGEV = GZCGEV()              
      IF(LCGEV.GT.0)THEN
        IF (LCGEV1.LE.0) GOTO 999    ! do not overwrite existing CGEV bank
      ELSE
        CALL BKCGEV(NR,LCGEV)        ! book *1 CGEV bank 
        IC(LCGEV+3) = 1              ! *1 scale 
        IF ((LDD.OR.BUILD_CSF).AND.DO_ADC_TO_GEV) THEN
          CALL CSF_FIX(RCP_FILE)     ! check/set CSF TYPE 
          CALL CSF_HV                ! check/set CSF HV
          CALL CSF_ICD_1A            ! check if run 1a ICD corrections needed
        END IF
      END IF
      NH = IC(LCGEV+1)
C
C ****  loop over all IETA,IPHI,ILYR fill CGEV with sampling weights
C
      LCSFW = GZCSFW ()
      LCSFC = GZCSFC ()
      DO ILYR = 1, NLYRL
        IF((LCSFC.GT.0).AND.TCOR) LCSFC1 = LZFIND(IDVSTP,LCSFC,ILYR,2)
        DO IETA = -NETAL,NETAL
          ICSFW =ILYR+(IABS(IETA)-1)*NLYRL  ! 1 to 629
          DO 86,IPHI = 1,NPHIL
            IF(IETA.EQ.0) GOTO 86
C
C ****  Check crate validity range here
C
            IF(.NOT.MC) CALL CGEVFL_VALIDITY(IETA,IPHI,ILYR,LPD,LGN)
            IF (LGN) GOTO 86
            ICSFC = IPHI+(IETA+NETAL)*NPHIL  
            IF(DO_ADC_TO_GEV) THEN
              IF((LCSFC1.GT.0).AND.TCOR) THEN
                AWC = C(LCSFW+1+ICSFW) * C(LCSFC1+2+ICSFC) !sampling weights 
              ELSE
                AWC = C(LCSFW+1+ICSFW)  !sampling weights 
              END IF
            ELSE
              AWC = 1.0                                  ! ADC count E scale
            END IF
            IF(.NOT.DO_GNSCOR) THEN  ! Simulate average CALIB gain effect
              IF (CELL_FBKCAP(IETA,IPHI,ILYR).EQ.10) THEN
                IF((.NOT.MC).AND.(.NOT.TB)) THEN
                  AWC = AWC*EM_GAIN ! 10.5pf 
                ELSE IF((ILYR.GE.3).AND.(ILYR.LE.7))  THEN
                  AWC = AWC*EM_GAIN ! 10.5pf for EM FLOOR 3,4 in MC,TB
                END IF
              END IF
            ELSE                     ! take out ICD average CALIB gain
              IF((.NOT.MC).AND.(.NOT.TB).AND.(ILYR.EQ.9)) 
     &          AWC = AWC*ICD_GAIN  ! ICD 20.5 pf 
            END IF
            ICGEV = NR*(ILYR-1+(IPHI-1)*NLYRL+(IETA+NETAL)*NPHIL*NLYRL)
            C(LCGEV+ICGEV+NH+1)             = AWC ! x8
   86     CONTINUE
        END DO
      END DO
C
C ****  KILL channels CGEV gain set to zero
C
      DO K = 1, NBAD
        IETA = BAD_LIST(1,K)
        IPHI = BAD_LIST(2,K)
        ILYR = BAD_LIST(3,K)
        CALL CAHITS_ERRMSG(4,IETA,IPHI,ILYR,1,0,0,
     &    'IN BAD_CHANNELS LIST IN CAHITS_RCP',IER)
        IF(IETA.EQ.0) THEN
          IETA1 = -NETAL
          IETA2 =  NETAL
        ELSE
          IETA1 = IETA
          IETA2 = IETA
        END IF
        IF(IPHI.EQ.0) THEN
          IPHI1 =  1
          IPHI2 =  NPHIL
        ELSE
          IPHI1 =  IPHI
          IPHI2 =  IPHI
        END IF
        IF(ILYR.EQ.0) THEN
          ILYR1 =  1
          ILYR2 =  NLYRL
        ELSE
          ILYR1 =  ILYR
          ILYR2 =  ILYR
        END IF
        DO 10, ILYR = ILYR1,ILYR2
          DO 10, IPHI = IPHI1,IPHI2
             DO 10, IETA = IETA1,IETA2
               IF(.NOT.CEXIST(IETA,IPHI,ILYR) ) GOTO 10
               ICGEV = ILYR-1+(IPHI-1)*NLYRL+(IETA+NETAL)*NPHIL*NLYRL
               IF((ICGEV.LT.NR*NLYRL*NPHIL*2*NETAL).AND.(ICGEV.GE.0))
     &         C(LCGEV+(NR*ICGEV)+NH+1) = 0.
   10   CONTINUE
      END DO
C
C ****  Book *8 scale CGEV bank, copy *8 into *1 
C
      IF ( (.NOT.DO_PEDSUB) .AND.(.NOT.DO_GNSCOR) ) GOTO 888
      CALL GTCGEV_HEAD(NH,NV,NS,NR,ND,IER)
      LCGEV = GZCGEV()
      LCGEV = LC(LCGEV)
      IF(LCGEV.EQ.0) CALL BKCGEV(NR,LCGEV)
      LCGEV2 = LCGEV
      LCGEV = GZCGEV()
C
      CALL UCOPY(C(LCGEV+1),C(LCGEV2+1),ND)
      IC(LCGEV+3)  = 0   ! first CGEV is *8 bank
      IC(LCGEV2+3) = 1   ! second *1 bank
C
C ****  Correct CGEV with CALIB gains (pedestals,sigmas..)
C
      DO_PEDSUBL = DO_PEDSUB    ! Turn on PED_SUB just for CGEVFL_PEDGNS
      IF(.NOT.DO_PEDSUB.AND.PSIGMA) THEN
        DO_PEDSUB = PSIGMA
        CALL EZPICK(RCP_FILE)
        CALL EZSET_l('DO_PEDSUB',DO_PEDSUB,IER)  ! update RCP_BANK 
        CALL EZRSET
      ENDIF
      CALL CGEVFL_PEDGNS(RCP_FILE,IER)
      IF(.NOT.DO_PEDSUBL.AND.PSIGMA) THEN
        DO_PEDSUB = DO_PEDSUBL
        CALL EZPICK(RCP_FILE)
        CALL EZSET_l('DO_PEDSUB',DO_PEDSUB,IER)  ! update RCP_BANK 
        CALL EZRSET
      ENDIF
  888 CONTINUE
      IF(DO_HOTSUP)CALL CGEVFL_HOT(RCP_FILE,IER)
      CALL CAHITS_ERRMSG_SUM(IBAD,CBAD,IER)
      DO I = 1,6
        IF (IBAD(I).GT.0) THEN
          WRITE(IBM_HAPPY,'(I5)')IBAD(I)
          ERR_ID = 'CAL '//IBM_HAPPY//' CHANNELS '//CBAD(I)
          CALL ERRMSG(ERR_ID,'CGEVFL',' GAINS IN CGEV BANK ','W') 
        END IF
      END DO
C----------------------------------------------------------------------
  999 RETURN
      ENTRY CGEVFL_RESET
      FIRST = .TRUE.
      END


