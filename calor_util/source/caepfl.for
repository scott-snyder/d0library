      SUBROUTINE CAEPFL(OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Book and fill CAEP bank starting from CAD1 and CAD2
C-
C-   Inputs  : NONE
C-   Outputs : OK - TRUE if CAEP filled
C-   Controls: CAHITS_RCP 
C-
C-   Created   2-DEC-1988   Serban D. Protopopescu
C-   Updated   1-MAR-1990   Chip Stewart  - new CAD format
C-   Updated  15-JAN-1991   Chip Stewart  - use CAD_STPFILE lookup table
C-                          Pedestal subtraction, Gain correction
C-   Updated  15-APR-1992   Chip Stewart  - use CGEV bank for gains,peds 
C-   Updated  24-APR-1992   Serban Protopopescu  - drop CAEP if
C-                                                 CAD1,2 corrupted 
C-   Updated   9-FEB-1993   Joan Guida    - Fix zero suppression,
C-                                          Add hot channel suppression bit
C-   Updated  18-MAR-1993   Joan Guida    - Put zero suppression bug back in 
C-   Updated  31-JUL-1993   Joan Guida    - Remove zero suppression bug 
C-   Updated   3-DEC-1993   Ian Adam, Chip- Fixed a CAD2-only data problem
C-   Updated  24-FEB-1994   Jan Guida     - Change definition of bit 0 of
C-                                          PAKADR, now pulser correction flag
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:CUNFLG.INC'
      INCLUDE 'D0$LINKS:IZCAEP.LINK'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$INC:PTCAEP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
C
      INTEGER I,J,IER,NCH
      INTEGER GZCAHT,GZCAEP,GZCADT,LCADT,CADT_LINK(2,0:5)
      INTEGER NCH1,NCH2,NONZCH,NCAD,ICH,ICAD,NCRATE,ADDR,SCALE,PCRATE
      INTEGER HEADER_LEN,ICRATE,SYNCH,CONTROL_WORD,VERSION,STATUS,PULSER
      INTEGER POINT,NR,ND,IWCONT,PAKADR,IWORD,IER1,IER2,IETA,IPHI,ILYR
      INTEGER GZCPDH,IPH,OLD_CAEP,MAX_ADDR,ADDR_PREV
      INTEGER GZCAD1,GZCAD2,NCGEV,LCGEV
      LOGICAL OK,FIRST,FIRST_CELL,DO_ADDR_CHECK,CEXIST,DO_CAEPFL
      LOGICAL DO_CONGEV,DO_ZSUPR,DO_PTCAEP,DO_PEDSUB,DO_GNSCOR,LHOT
      LOGICAL NEED_CAD1,NEED_CAD2,RESET
      CHARACTER MSG*40
      REAL ENERGY,ENERGY_CUT,SIGMA_CUT,CGEV(3),K,S,P
      EQUIVALENCE(K,CGEV(1))
      EQUIVALENCE(S,CGEV(2))
      EQUIVALENCE(P,CGEV(3))
      BYTE CONTRW(4)
      EQUIVALENCE(IWCONT,CONTRW)
      BYTE BYTES(4)
      EQUIVALENCE (PAKADR,BYTES)
      INTEGER IBITS,IOR
      LOGICAL BTEST
      SAVE FIRST,ENERGY_CUT,DO_CONGEV,DO_ZSUPR,SIGMA_CUT
      DATA FIRST /.TRUE./,RESET/.TRUE./
C----------------------------------------------------------------------
      OK=.TRUE.
      IF(FIRST.OR.RESET) THEN
        RESET = .FALSE.
C
C ****  FETCH CONSTANTS FROM RCP
C
        CALL EZPICK('CAHITS_RCP')
        CALL EZERR(IER)
        IF ( IER.EQ.0) THEN
          CALL EZGET('DO_CAEPFL',DO_CAEPFL,IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG(' WRONG_CAHITS_RCP','CAEPFL',
     &        ' PROCESSING SKIPS CAEPFL','W')
            GOTO 999
          END IF
          CALL EZGET('DO_ZERO_SUPRESS',DO_ZSUPR,IER)
          CALL EZGET('ENERGY_CUTOFF',ENERGY_CUT,IER)
          CALL EZGET('FILL_PTCAEP',DO_PTCAEP,IER)
          CALL EZGET('DO_ADDR_CHECK',DO_ADDR_CHECK,IER)
          CALL EZGET('OLD_CAEP',OLD_CAEP,IER)
          CALL EZGET('NEED_CAD1',NEED_CAD1,IER)
          CALL EZRSET
        ELSE
          CALL ERRMSG(' NO_CAHITS_RCP','CAEPFL',
     &        ' PROCESSING SKIPS CAEPFL','W')
          DO_CAEPFL   =  .FALSE.
        ENDIF
        IF( .NOT.DO_CAEPFL) THEN
          CALL INTMSG(' PROCESSING SKIPS CAEPFL')
          GOTO 999
        END IF
C
C ****  SETUP CADT LOOK-UP TABLE
C
        IF(FIRST) THEN
          FIRST = .FALSE.
          PLSCOR = .FALSE.      ! Initialize here, set in CMODGN
          CALL CZLINI
          CALL STP_INZLNK
          LCADT = GZCADT ()
          DO NCAD = 1, 2
            DO NCRATE = 0, 5
C
C ****  RESERVE LINK IN STP_ZLINKA FOR LINK TO NCAD,NCRATE
C
              CALL STP_GSLINK('CUNPAK',CADT_LINK(NCAD,NCRATE) )
              STP_LSLINK(CADT_LINK(NCAD,NCRATE)) = LCADT
              IF (LCADT.LE.0) THEN
                CALL ERRMSG('CAL ADDRESSING TABLE','CAEPFL',
     &            'CADT CHAIN BAD','W')
                GOTO 999
              END IF
              LCADT = LC(LCADT)
              IF( LCADT.LE.0) GOTO 77
            END DO
          END DO
   77     LCADT = GZCADT ()
          CALL CADPAK(NADCC-1,NBLSC-1,NEFC-1, NDEPTC-1,0,0,MAX_ADDR)
          MAX_ADDR = IBITS(MAX_ADDR,2,13)
        ENDIF
C
C ****  CREATE CGEV BANK FROM CALIB GAIN,PED and CSF_STPFILE
C
        LCAEP=GZCAEP()
        IF((OLD_CAEP.EQ.1).OR.(LCAEP.LE.0)) THEN
          IER1 = 0
          IER2 = 0
          IF(GZCAD1().GT.0) CALL GTCAD_TOTAL(1,NCH1,IER1)
          IF(GZCAD2().GT.0) CALL GTCAD_TOTAL(2,NCH1,IER2)
          IF ((IER1.NE.0).OR.(IER2.NE.0)) THEN
            RESET = .TRUE.
            CALL ERRMSG('CAD HEADER ERROR ON FIRST EVENT','CAEPFL',
     &      'BUILD CGEV ON NEXT EVENT','W')
            GOTO 999
          ELSE IF((GZCAD2().EQ.0).AND.(GZCAD1().EQ.0)) THEN
            RESET = .TRUE.
            CALL ERRMSG('NO CAD BANKS ON FIRST EVENT','CAEPFL',
     &      'BUILD CGEV ON FIRST EVENT WITH CAD1 or CAD2','W')
            GOTO 999
          END IF
C
C ****  CHECK CAD VERSION TO FIX CADT TABLE IF NEEDED
C
          CALL CADT_FIX(CADT_LINK)
C
C ****  Fill CGEV
C
          LCGEV = 1
          CALL CGEVFL('CAHITS_RCP',LCGEV)
          CALL EZPICK('CAHITS_RCP')
          CALL EZGET('DO_PEDSUB',DO_PEDSUB,IER)
          CALL EZGET('DO_GNSCOR',DO_GNSCOR,IER)
          CALL EZGET('SIGMA_CUTOFF',SIGMA_CUT,IER)
          CALL EZGET('NEED_CAD2',NEED_CAD2,IER)
          CALL EZGET('DO_ADC_TO_GEV',DO_CONGEV,IER)
          CALL EZRSET
        END IF
C
      ENDIF
C
      IF (.NOT.DO_CAEPFL) GOTO 999
C
      OK=.FALSE.
      LCAEP=GZCAEP()
      IF (LCADT.LE.0) GOTO 999    ! NEED CADT TABLE TO UNPACK CAD DATA
      IF(LCAEP.EQ.0.AND.GZCAD1().EQ.0.AND.GZCAD2().EQ.0)GOTO 999 !nothing THERE
      LCAHT=GZCAHT()
      IF(LCAHT.EQ.0) THEN         ! CONSTRUCT CAHT BANK
        CALL CAHTFL
        LCAHT=GZCAHT()
      ENDIF
      IF(LCAEP.GT.0) THEN
        IF (OLD_CAEP.EQ.1) THEN
          LQ(LCAHT-IZCAEP) = 0
          CALL ERRMSG(' OVERWRITE EXISTING CAEP BANK','CAEPFL',
     &      'CREATE NEW CAEP FROM CAD1 & CAD2','I')
        ELSE IF (OLD_CAEP.EQ.2) THEN
          CALL CAEPFL_RESCALE('CAHITS_RCP',IER)
          OK=IER.EQ.0
          GOTO 999
        ELSE
          OK=.TRUE.
          GOTO 999     ! CAEP ALREADY DONE
        END IF
      ENDIF
C
C ****  CAD CHANNEL TOTALS
C
      CALL GTCAD_TOTAL(1,NCH1,IER1)
      CALL GTCAD_TOTAL(2,NCH2,IER2)
      NCH=NCH1+NCH2               ! Total no. of channels
      IER = IER1*IER2
      IF(IER.NE.0 .OR. NCH.EQ.0) THEN
        CALL ERRMSG('GTCAD_TOTAL','CAEPFL',
     &    'CAD1 AND/OR CAD2 BAD','W')
        GOTO 999
      ELSE IF(IER1.NE.0 .AND. NEED_CAD1) THEN
        CALL ERRMSG('GTCAD_TOTAL','CAEPFL',
     &    'NEED CAD1 BUT CAD1 BAD ','W')
        GOTO 999
      ELSE IF(IER2.NE.0 .AND. NEED_CAD2) THEN
        CALL ERRMSG('GTCAD_TOTAL','CAEPFL',
     &    'NEED CAD2 BUT CAD2 BAD ','W')
        GOTO 999
      END IF
C
C ****  BOOK CAEP BANK
C
      CALL BKCAEP(NCH,LCAEP)
      NR=IQ(LCAEP+2)
      NCAD=1                      ! UNPACK CAD1 FIRST
      IF(IER1.NE.0) NCAD = 2      ! UNLESS CAD2 HAS ONLY DATA
      POINT=3
      NONZCH=0
      NCH=NCH1
      IF(NCAD.EQ.2) NCH=NCH2
C
C ****  LOOP OVER CAD BANKS
C
   20 CALL GTCAD_HEADER (NCAD,0,HEADER_LEN,SYNCH,CONTROL_WORD,
     &  VERSION,STATUS,PULSER,IER)
      CALL CAFLGS(CONTROL_WORD)   ! FILL CUNFLG COMMON WITH CAD FLAGS/SWITCHES
C
C **** CAD BANK OVERALL CHECK - HEADER,NCH... RCP CONTROLLED
C **** THIS ROUTINE ONLY CHECKS THE FIRST CRATE HEADER IN EACH CAD BANK
C
      CALL CAD_HEADER_CHECK('CAHITS_RCP',NCAD,IER)
      IF(IER.LE.-2) THEN
        CALL ERRMSG('CAD HEADER ERROR','CAEPFL',
     &    'CAD HEADER ERROR','W')
        CALL MZDROP(IXCOM,LCAEP,' ')
        GOTO 999
      ELSE IF(IER.EQ.0) THEN            !PRESCALE CAD ADDRESS CHECKING
        ADDR_PREV = -1
        DO_ADDR_CHECK = .TRUE.         !CHECK THIS EVENT
      ELSE IF(IER.EQ.1) THEN
        DO_ADDR_CHECK = .FALSE.        !SKIP CHECK ON THIS EVENT
      END IF
C
C ****  LOOP OVER CHANNELS
C
      FIRST_CELL = .TRUE.
      IF (DO_PTCAEP) PTZFLG = .FALSE.   ! PTCAEP flag
      DO 40, ICH = 1, NCH
        CALL GTCAD(NCAD,FIRST_CELL,ICRATE,IWORD,IER)
        FIRST_CELL = .FALSE.
        ADDR =  IBITS(IWORD,18,13)
        SCALE = IBITS(IWORD,17,1)
        IF (ADDR.GT.MAX_ADDR) THEN
          WRITE(MSG,1004) IWORD
          CALL ERRMSG('BAD CAD WORD','CAEPFL',MSG,'W')
          CALL MZDROP(IXCOM,LCAEP,' ')
          GOTO 999
        END IF
        CALL CALPH(IWORD,IPH)
        ENERGY= IPH
C
C ****  ICRATE CAD1=07,17,27,37,47,57; CAD2=08,18,28,38,48,58;
C ****  5000CH TEST=67; QUADRANT TEST = 77; TB91 LOAD 2 =87
C
        IF(ICRATE.NE.PCRATE) THEN
          ADDR_PREV = -1
          PCRATE = ICRATE
          NCRATE = ICRATE / 10
          ICAD = NCAD
          IF (D0VSN.EQ.0 .AND. CALVSN.EQ.3) ICAD = MOD(ICRATE,10) - 6  !CR1
          IF (D0VSN.EQ.64 .AND. CALVSN.EQ.2) NCRATE = 0  !TB90L2 ICRATE=87
        END IF
        IF( DO_ADDR_CHECK) THEN
          IF (DATYPE.EQ.0 .AND. ADDR.LE.ADDR_PREV) THEN
            CALL ERRMSG('CAD_ORDER','CAEPFL',
     &        'CAD WORDS OUT OF ORDER','W')
            CALL MZDROP(IXCOM,LCAEP,' ')
            GOTO 999
          END IF
          ADDR_PREV = ADDR
          CALL CAD_ADDRESS_CHECK(IWORD,IER)
          IF(IER.LT.-1) THEN
            WRITE(MSG,1004) IWORD
            CALL ERRMSG('BAD CAD WORD','CAEPFL',MSG,'W')
            CALL MZDROP(IXCOM,LCAEP,' ')
            GOTO 999
          END IF
          IF(IER.EQ.-1) GOTO 40
        END IF
        PAKADR = IC(STP_LSLINK(CADT_LINK(ICAD,NCRATE)) + ADDR + 3)
        IETA  = BYTES(BYTE4)
        ILYR  = BYTES(BYTE2)
        IPHI  = BYTES(BYTE3)
        IF(ILYR.LE.0 ) GOTO 40
C
C ****  CAEP ENERGY SCALE, PEDESTAL SUBTRACTION & ELECTRONICS GAIN CORRECTION
C
        CALL GTCGEV(ILYR,IPHI,IETA,SCALE,NCGEV,CGEV,IER)
        IF ((.NOT.PEDSUB).AND.DO_PEDSUB) ENERGY = ENERGY - P
C
C ****  ZERO SUPPRESSION (simulating hardware)
C
        IF(DO_ZSUPR .AND.
     &    ABS(NINT(ENERGY)).LE.MAX0(NINT(S*SIGMA_CUT),1))GOTO40
        ENERGY = ENERGY*K
C
C ****  SOFTWARE ZERO SUPPRESSION EXCEPT FOR ICD AND MASSLESS GAPS
C
        IF((DO_ZSUPR.AND.(ILYR.LT.8.OR.ILYR.GT.10))
     &   .AND.(ABS(ENERGY).LT.ENERGY_CUT)) GOTO 40
C
C ****  SET TAG BITS
C
C    Change definition of bit 0.  2/24/93  
C    bit 0 of PAKADR is then filled in CMODGN if pulser corrections 
C    were performed.
C        PAKADR=IOR(PAKADR,IBITS(IWORD,16,1) )      ! NEGLIM
        IF(PLSCOR) PAKADR=IOR(PAKADR,1)          ! Pulser corrections performed
        PAKADR=IOR(PAKADR,2*(IBITS(IWORD,17,1)))   ! SCALE
        IF(.NOT.ZSUPRS.AND..NOT.DO_ZSUPR) PAKADR=IOR(PAKADR,16)
        IF(.NOT.DO_CONGEV) PAKADR=IOR(PAKADR,32)  ! not converted to GeV
        IF(.NOT.DO_GNSCOR.AND..NOT.GNCORR) PAKADR=IOR(PAKADR,8) !no gns crrctd
        IF(.NOT.DO_PEDSUB.AND..NOT.PEDSUB) PAKADR=IOR(PAKADR,4) !no ped sbtrct
        CALL CGEVFL_HOT_CHAN(IETA,IPHI,ILYR,LHOT)
        IF(LHOT) PAKADR=IOR(PAKADR,128)            !hot channel (suppressed)
C
C ****  FILL CAEP
C
        POINT=POINT+1
        IQ(LCAEP+POINT)=PAKADR         ! packed physics address
        POINT=POINT+1
        Q(LCAEP+POINT)=ENERGY          ! energy
        NONZCH=NONZCH+1
C
C ****  FILL PTCAEP ARRAY
C
        IF (DO_PTCAEP) PTCAEP(IETA,IPHI,ILYR) = NONZCH
   40 CONTINUE
      IF(NCAD.EQ.1 .AND. (CALVSN.EQ.0 .OR. CALVSN.GE.4)) THEN
C
C ****  UNPACK CAD2 BANK (IF CALVSN SET TO FULL D0 DATA)
C
        IF(GZCAD2().EQ.0) THEN
          CALL ERRMSG('NO_CAD2','CAEPFL',
     &      ' NO CAD2 FOUND','W')
        ELSE
          NCAD = 2
          NCH = NCH2
          GOTO 20
        END IF
      END IF
C
C ****  REDUCE BANK SIZE IF WARRANTED
C
      IF(NONZCH .LT. (NCH1+NCH2 ) ) THEN
        NCH = NR*(NONZCH-NCH1-NCH2)
        CALL MZPUSH(IXCOM,LCAEP,0,NCH,'I')
      ENDIF
      IQ(LCAEP+3)=NONZCH                ! no. of channels
      OK=.TRUE.                         ! succesful unpacking
  999 RETURN
 1004 FORMAT(1X,' CAD WORD ',Z12.8)
C------------------------------------------------------------------------
      ENTRY CAEPFL_RESET
      CALL CAD_HEADER_CHECK_RESET
      CALL CGEVFL_RESET
      CALL CGEVFL_PEDGNS_RESET
      CALL CSF_FIX_RESET
      CALL CADT_FIX_RESET
      CALL GT_PED_GNS_ADDR_RESET
      CALL CAHITS_ERRMSG_RESET
      RESET = .TRUE.
      END
