      SUBROUTINE TB90_CAEPFL (IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Book and fill CAEP bank starting from CAD1 and CAD2 from TB90 data.
C-
C-   Inputs  : NONE
C-   Outputs : IER =  0 IF CAEP FILLED OK
C-                 = -1 IF CAD BANK IS NOT TEST BEAM
C-                 = -2 PROBLEM UNPACKING CAD
C-                 = -3 PROBLEM IN ZEBRA
C-   Controls: NONE
C-
C-   Created   1-FEB-1990   WELATHANTRI G. DHARMARATNA, CHIP STEWART
C-   Updated   5-JUN-1990   W. G. DHARMARATNA, CHIP STEWART
C-                          TRY TO MAKE FASTER
C-   Updated  29-JUN-1990   Chip Stewart, Marcel Demarteau
C-                          pulser GAINS implemented
C----------------------------------------------------------------------
      IMPLICIT NONE
C common blocks included
      INCLUDE 'D0$INC:ZEBCOM.INC'               ! zebra common
      INCLUDE 'D0$INC:ZLINKC.INC'       ! calorimeter address link area
      INCLUDE 'D0$INC:CUNFLG.INC'       ! CAD bank flags
      INCLUDE 'D0$LINKS:IZCAEP.LINK'    ! link pointer to CAEP
      INCLUDE 'D0$INC:TB90_PHYS_ADDR.INC'       ! tb90 look up table
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'     ! CAD bank params
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'    ! CAL UNPACKING params
      INCLUDE 'D0$INC:PTCAEP.INC'
C input
      INTEGER IER
C character strings
      CHARACTER*80 MSG_STRING
      CHARACTER*40 MSG,ERR_ID
C integers
      INTEGER I,J,K,M
      INTEGER IWORD,ICRATE,CARD,BLS,TOWER,DEPTH,SCALE,NEGLIM  ! CAD addresses
      INTEGER IWCONT,HEADER_LEN,SYNCH,SEQ,SEQ_POINT,TEST,SEQ_PED
      INTEGER VERSION,PULSER,ICAD,IPH,CONTROL_WORD,STATUS
      INTEGER LCPD8, LCPD1
      INTEGER LCGN8, LCGN1
      INTEGER IETA,IPHI,LYR,PAKADR,POINT                     ! CAEP addresses
      INTEGER NUMG,ETAV(20),PHIV(20),LAYERV(20)
      INTEGER NCOUNT,NONZCH,NWCAEP
      INTEGER NR,ND,NCH
      INTEGER IOR,ISHFT                              ! VAX functions
      INTEGER GZCPD8,GZCPD1,GZCAHT,GZCAEP            ! bank address functions
      INTEGER GZCGN8,GZCGN1,TASK                     ! bank address functions
      INTEGER STATUS_MASK
      INTEGER HIGH_PED_1,HIGH_PED_8
      INTEGER LOW_PED_1,LOW_PED_8
C logicals
      LOGICAL FIRST_CELL,TB,CAL_TB_CHK,CONGEV,FIRST,ZSUP
      LOGICAL DO_PEDSUB,DO_GNSCOR,CPZ,OVERWRITE_CAEP
      LOGICAL FIRST_PED,FIRST_GNS,LPTCAEP
C reals
      REAL ENERGY,FACTOR,PULSE_HEIGHT,PEDESTAL,SIGMA,ECUTOF,SCUTOF
      REAL GAIN,GAIN_NORM,LOW_GAIN,HIGH_GAIN
      REAL TB90_CAD_GAIN                     ! GAIN FUNCTION
C&IF VAXVMS
      BYTE CONTRW(4)
      EQUIVALENCE(IWCONT,CONTRW)        ! unpacking CAD control word
      BYTE BYTES(4)
      EQUIVALENCE (PAKADR,BYTES)        ! unpacking CAEP address
      INTEGER*2 HLFWRD(2)
      EQUIVALENCE (HLFWRD,IWORD)        ! unpacking pulse_height
C&ENDIF
      SAVE FIRST_CELL
      DATA CONGEV,FIRST,FIRST_PED,FIRST_GNS/.FALSE.,3*.TRUE./
      DATA GAIN/1./
C----------------------------------------------------------------------

      IF(FIRST) THEN
C                 get constants out of TB90_CALOR_UNPACK_RCP
        CALL EZPICK('TB90_CALOR_UNPACK_RCP')
        CALL EZERR(IER)
        IF ( IER.EQ.0 ) THEN
          CALL EZGET('ENERGY_CUTOFF',ECUTOF,IER)
          CALL EZGET('SIGMA_CUTOFF',SCUTOF,IER)
          CALL EZGET('CONVERT_TO_GEV',CONGEV,IER)
          CALL EZGET('ZERO_SUPRESS',ZSUP,IER)
          CALL EZGET('USE_CPZ_BANK',CPZ,IER)
          CALL EZGET('FILL_PTCAEP',LPTCAEP,IER)
          CALL EZGET('DO_PEDSUB',DO_PEDSUB,IER)
          CALL EZGET('DO_GNSCOR',DO_GNSCOR,IER)
          CALL EZGET('GAIN_NORMALIZATION',GAIN_NORM,IER)
          IF (IER.NE.0) THEN
            GAIN_NORM = 1900. 
            CALL ERRMSG('NO GAIN NORMALIZATION in RCP ','TB90_CAEPFL',
     &        ' USE NOMINAL 1900 ','W')
          END IF
          CALL EZGET('LOW_GAIN_CUT',LOW_GAIN,IER)
          IF (IER.NE.0) THEN
            LOW_GAIN = 500
            CALL ERRMSG('NO LOW GAIN CUT in RCP ','TB90_CAEPFL',
     &        ' USE NOMINAL 500 ','W')
          END IF
          CALL EZGET('HIGH_GAIN_CUT',HIGH_GAIN,IER)
          IF (IER.NE.0) THEN
            HIGH_GAIN = 5000 
            CALL ERRMSG('NO HIGH GAIN CUT in RCP ','TB90_CAEPFL',
     &        ' USE NOMINAL 5000 ','W')
          END IF
          CALL EZGET('LOW_PED_CUT*1',LOW_PED_1,IER)
          IF (IER.NE.0) THEN
            LOW_PED_1 = 2025
            CALL ERRMSG('NO LOW PEDESTAL CUT in RCP ','TB90_CAEPFL',
     &        ' USE NOMINAL 2025 ','W')
          END IF
          CALL EZGET('HIGH_PED_CUT*1',HIGH_PED_1,IER)
          IF (IER.NE.0) THEN
            HIGH_PED_1 = 2825 
            CALL ERRMSG('NO HIGH PEDESTAL CUT in RCP ','TB90_CAEPFL',
     &        ' USE NOMINAL 2825 ','W')
          END IF
          CALL EZGET('LOW_PED_CUT*8',LOW_PED_8,IER)
          IF (IER.NE.0) THEN
            LOW_PED_8 = 150
            CALL ERRMSG('NO LOW PEDESTAL CUT in RCP ','TB90_CAEPFL',
     &        ' USE NOMINAL 150 ','W')
          END IF
          CALL EZGET('HIGH_PED_CUT*8',HIGH_PED_8,IER)
          IF (IER.NE.0) THEN
            HIGH_PED_8 = 450
            CALL ERRMSG('NO HIGH PEDESTAL CUT in RCP ','TB90_CAEPFL',
     &        ' USE NOMINAL 450 ','W')
          END IF
          CALL EZGET('STATUS_MASK',STATUS_MASK,IER)
          IF (IER.NE.0) THEN
            STATUS_MASK = 196607 ! STATUS_VERTEX = 0002FFFF in hex 
            CALL ERRMSG('NO STATUS MASK in RCP ','TB90_CAEPFL',
     &        ' USE NOMINAL 0002FFFF ','W')
          END IF
          CALL EZGET('OVERWRITE_CAEP',OVERWRITE_CAEP,IER)
          IF (IER.NE.0) THEN
            OVERWRITE_CAEP = .FALSE. ! Do not overwrite CAEP
            CALL ERRMSG('NO OVERWRITE_CAEP in RCP ','TB90_CAEPFL',
     &        ' USE NOMINAL .FALSE. ','W')
          END IF

        ELSE
C            Defaults if no file is used
          CALL INTMSG( ' TB90_CALOR_UNPACK_RCP file was not read ')
          ECUTOF=0.00
          SCUTOF=0.01
          LOW_GAIN = 20
          HIGH_GAIN = 2000
          HIGH_PED_1 = 2825
          LOW_PED_1 = 2075
          HIGH_PED_8 = 450
          LOW_PED_8 = 150
          CONGEV=.FALSE.
          ZSUP=.TRUE.
          STATUS_MASK = 196607 ! STATUS_VERTEX = 0002FFFF in hex 
          OVERWRITE_CAEP = .FALSE. ! Do not overwrite CAEP
        ENDIF
        IF(CPZ.AND.(SCUTOF.NE.1.0) ) THEN
          SCUTOF = 1.0
          CALL ERRMSG('USE_CPZ_BANK','TB90_CAEPFL',
     &      ' SETTING SIGMA_CUTOFF TO 1.0','W')
        END IF
        CALL EZRSET
        FIRST=.FALSE.
      ENDIF
C
      IER = 0
      LCAEP=GZCAEP()
      IF(LCAEP.GT.0) THEN
        IF( OVERWRITE_CAEP ) THEN
          LCAHT=GZCAHT()
          LQ(LCAHT-IZCAEP) = 0
        ELSE 
          GOTO 999   ! already done
        END IF
      END IF
      LCAHT=GZCAHT()
      IF(LCAHT.EQ.0) THEN     ! construct CAHT bank
        CALL CAHTFL
        LCAHT=GZCAHT()
      ENDIF
      CALL GTCAD_TOTAL(1,NCH,IER)
      IF(IER.NE.0) THEN
        CALL ERRMSG('GTCAD_TOTAL','TB90_CAEPFL','CAD1 TOTAL BAD','W' )
        GOTO 999
      END IF
      NONZCH=0
C  Book CAEP bank
      NWCAEP = 5658
      CALL BKCAEP(NWCAEP,LCAEP)  ! FACTOR OF 2 TO ALLOW FOR <GANGING> <= 2
      NR=IQ(LCAEP+2)
      POINT = 3
      NCOUNT = 0
      FACTOR = 1.0
C  Get CAD1 header info
      CALL GTCAD_HEADER (1,ICRATE,HEADER_LEN,SYNCH,
     &  CONTROL_WORD,VERSION,STATUS,PULSER,IER)
C
      FIRST_CELL = .TRUE.
      CALL CAFLGS(CONTROL_WORD)
C&IF VAXVMS
      IF(DATYPE.LE.-1) THEN
        WRITE(MSG,1001) CONTROL_WORD
        CALL ERRMSG('BAD-CAFLG','TB90_CAEPFL',MSG,'W')
        IER = - 999
        GOTO 999
      ENDIF
C
      IF (.NOT.ZSUPRS .AND. STATUS .NE. STATUS_MASK ) THEN
        WRITE(MSG,1003) STATUS
        CALL ERRMSG('CAD1-STATUS-BAD','TB90_CAEPFL',MSG,'W')
        IER = - 998
        GOTO 999
      ELSEIF (ZSUPRS .AND. STATUS .LT. 0)THEN
        WRITE(MSG,1003) STATUS
        CALL ERRMSG('CAD1-STATUS-BAD','TB90_CAEPFL',MSG,'W')
        IER = - 998
        GOTO 999
      ENDIF
C
      IF(.NOT.ZSUPRS .AND. (MOD(NCH,384).NE.0)) THEN
        WRITE(MSG,1002) CONTROL_WORD,NCH
        CALL ERRMSG('CAD1-WORDS-BAD','TB90_CAEPFL',MSG,'W')
        IER = - 997
        GOTO 999
      ENDIF
C&ENDIF 
      PAKADR=0
C
      TB = CAL_TB_CHK ()
      IF ( .NOT. TB ) THEN
        IER = -1
        GOTO 999
      END IF
C
C ***** loop over ADCs per crate
C
      DO 40, ICAD = 1, NCH
        CALL GTCAD(1,FIRST_CELL,ICRATE,IWORD,IER)
        FIRST_CELL = .FALSE.
        NCOUNT=NCOUNT+1
c
c code from CALL TB90_ADCPHY(IWORD,NUMG,ETAV,PHIV,LAYERV,ICOND)
c code from CALL CADUPK(ICRATE,IWORD,CRATE,CARD,BLS,TOWER,DEPTH,SCALE, NEGLIM)
C ADC data word address part
C                  27           24     22               18  17   16
C |---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
C |        ADC CARD   |    BLS    | ROTOW |     DEPTH     |SCL|NEG|
C |                   |           |       |               |   |LIM|
C |---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
C&IF VAXVMS
        NEGLIM = ( ISHFT(IWORD,-16) .AND. 1)          ! JBIT(ADR,1)
        SCALE =  ( ISHFT(IWORD,-17) .AND. 1)          ! JBIT(ADR,2)
        DEPTH =  ( ISHFT(IWORD,-18) .AND. 15)         ! JBYT(ADR,3,4)
        TOWER =  ( ISHFT(IWORD,-22) .AND. 3)          ! JBYT(ADR,7,2)
        BLS   =  ( ISHFT(IWORD,-24) .AND. 7)          ! JBYT(ADR,9,3)
        CARD  =  ( ISHFT(IWORD,-27) .AND. 31)         ! JBYT(ADR,12,5)
C
        IF ( DEPTH.GT.11) THEN
          WRITE(MSG,1004) IWORD 
          CALL ERRMSG('BAD-CAD1-WORD','TB90_CAEPFL',MSG,'W')
          GOTO 40
        END IF
C
C--- Now get the SEQUENTIAL ADC address & pedestal address
C
        IF(.NOT.ZSUPRS ) THEN
          SEQ = ICAD   ! ALREADY IN SEQ ORDER IF CAD IS NOT ZERO SUPPRESSED
        ELSE
          SEQ =  ISHFT(IWORD,-22)*NDEPTC + DEPTH   + 1
        END IF

C        SEQ= CARD*NDEPTC*NEFC*NBLSC + BLS*NDEPTC*NEFC + TOWER*NDEPTC
C     X                                                     +DEPTH +1
        SEQ_PED = ( ISHFT(IWORD,-22) .AND. 31 )*NDEPTC + DEPTH
C
C ****  Do pedestal subtraction if needed
C
        IF(PEDSUB .AND. DO_PEDSUB ) THEN
          DO_PEDSUB = .FALSE.
          CALL INTMSG(' CAD1 data ALREADY pedestal subtracted')
        ENDIF
        IF(GNCORR.AND. DO_GNSCOR ) THEN
          DO_GNSCOR = .FALSE.
          CALL INTMSG(' CAD1 data ALREADY gain corrected')
        ENDIF
C
        IF (FIRST_PED .AND. .NOT.PEDSUB .and. DO_PEDSUB ) THEN
C code from CALL TB90_PEDSUB(ICRATE,CARD,BLS,TOWER,DEPTH,SCALE,PULSE_HEIGHT,IER)
          FIRST_PED = .FALSE.
          LCPD8 = GZCPD8 ()
          LCPD1 = GZCPD1 ()
          IF ( LCPD8.LE.0 .OR. LCPD1.LE.0 ) THEN
C ****  READ IN PEDESTALS
            CALL INTMSG(' SUBTRACT PEDS')
C CALL TB90_CALOR_READ_PEDS replaces CALL SELECT_INPUT_FILES 11-JUN-1990 CS/WGD
            CALL TB90_CALOR_READ_PEDGNS('TB90_CALOR_UNPACK_RCP')
            LCPD8 = GZCPD8 ()
            LCPD1 = GZCPD1 ()
            IF ( LCPD8.LE.0 .OR. LCPD1.LE.0) THEN
              DO_PEDSUB = .FALSE.
              PEDESTAL = 0
              CALL INTMSG('No PEDESTAL FILE selected - NO SUBTRACTION')
              GOTO 997
            END IF
C
C ****  SETUP CPZ PEDESTALS
C
            IF (CPZ) THEN
              TASK=2
            ELSE
              TASK=1
            END IF
            IF (DO_GNSCOR) THEN
              LCGN8 = GZCGN8 ()
              LCGN1 = GZCGN1 ()
              FIRST_GNS = .FALSE.
              IF ( LCGN8.LE.0 .OR. LCGN1.LE.0) THEN
                DO_GNSCOR = .FALSE.
                GAIN = 1.0
                CALL INTMSG(
     &             'No GAINS FILE selected - NO GAINS CORRECTION')
              END IF
            END IF
          END IF
        END IF
        IF (.NOT.PEDSUB .and. DO_PEDSUB ) 
     &    CALL GT_PED_GNS_ADDR(TASK,ICRATE,CARD,SEQ_PED,SCALE,
     &    PEDESTAL,SIGMA,IER)
  997   IPH=HLFWRD(1)
        IF ( (IPH.EQ.32760 .AND. SCALE.EQ.1 )    !32760= 2FF8 HEX
     &  .OR. (IPH.EQ.4095 .AND. SCALE.EQ.0 ) ) THEN   !4095 = FFF HEX
          WRITE(MSG,1004)IWORD
          CALL ERRMSG('CAD-OVERFLOW','TB90_CAEPFL',MSG,'W')
          IER = -996
          GOTO 999
        END IF
        IF (DO_PEDSUB .AND. ( PEDESTAL.GT.HIGH_PED_8
     &    .OR. PEDESTAL.LT.LOW_PED_8 )) THEN
          IF(SCALE.EQ.1) THEN
C
C ****  CHECK FOR X1 PEDS
C
            IF ( PEDESTAL.LE.HIGH_PED_1
     &        .AND. PEDESTAL.GE.LOW_PED_1 ) THEN            
              GOTO 15
            END IF
          END IF
          WRITE(MSG,1006)PEDESTAL,IWORD
   11     FORMAT(' PEDESTAL=',E7.2,' WORD=',Z9.8)
          WRITE(ERR_ID,17)HLFWRD(2)
   17     FORMAT('BAD-PEDESTAL-',Z4)
          CALL ERRMAX(ERR_ID,1,0)
          CALL ERRMSG(ERR_ID,'TB90_CAEPFL',MSG,'W')
          GOTO 40
        END IF
   15   PULSE_HEIGHT = IPH - PEDESTAL
C
C ****  QUICK ZERO SUPPRESSION
C
        IF (ZSUP.AND. (ABS(PULSE_HEIGHT).LT.(SCUTOF*SIGMA))) GOTO 40
C
C ****  Do gains correction if needed
C
        IF (FIRST_GNS .AND. .NOT.GNCORR .AND. DO_GNSCOR ) THEN
          FIRST_GNS = .FALSE.
          LCGN8 = GZCGN8 ()
          LCGN1 = GZCGN1 ()
          IF ( LCGN8.LE.0 .OR. LCGN1.LE.0 ) THEN
C ****  READ IN GAINS
            CALL INTMSG(' Do GAINS correction ')
            CALL TB90_CALOR_READ_PEDGNS('TB90_CALOR_UNPACK_RCP')
            LCGN8 = GZCGN8 ()
            LCGN1 = GZCGN1 ()
            IF ( LCGN8.LE.0 .OR. LCGN1.LE.0) THEN
              DO_GNSCOR = .FALSE.
              GAIN = 1.0
              CALL INTMSG(
     &             'No GAINS FILE selected - NO GAINS CORRECTION')
              GOTO 998
            END IF
          END IF
        END IF
        IF (.NOT.GNCORR .and. DO_GNSCOR ) THEN
          CALL GT_PED_GNS_ADDR(3,ICRATE,CARD,SEQ_PED,SCALE,
     &    GAIN,SIGMA,IER)
          IF ( GAIN.LT.LOW_GAIN .or. GAIN.GT.HIGH_GAIN ) THEN
            WRITE(MSG,1007) CARD,SEQ_PED, SEQ,GAIN
            WRITE(ERR_ID,18)HLFWRD(2)
   18       FORMAT('BAD-GAIN-',Z4)
            CALL ERRMAX(ERR_ID,1,0)
            CALL ERRMSG(ERR_ID,'TB90_CAEPFL',MSG,'W')
            GOTO 40
          ELSE
            PULSE_HEIGHT = GAIN_NORM * PULSE_HEIGHT / GAIN
          END IF
        ENDIF
C
C **** --- Finally convert to a PHYSICS address.
C
C code from CALL TB90_SEQPHY(SEQ,NUMG,ETAV,PHIV,LAYERV)
  998   NUMG = 1
        SEQ_POINT = SEQ
        PAKADR = TB90_PHYS_ADDR(2,SEQ)
   30   SEQ_POINT = TB90_PHYS_ADDR(1,SEQ_POINT)
        IF(SEQ_POINT.GT.10000 ) THEN
C ****  TB90_PHYS_ADDR table corrupted
          CALL ERRMSG('BAD-TABLE','TB90_CAEPFL',' BAD POINT ','W')
          NUMG = 0
        ELSE IF (SEQ_POINT.EQ.0) THEN
C ****  Some ADC channels are not used.
          NUMG = 0
          ETAV(1) = 0
          PHIV(1) = 0
          LAYERV(1) = 0
        ELSE
C ****  Good addresses
          ETAV(NUMG)  = BYTES(3)
          PHIV(NUMG)  = BYTES(2)
          LAYERV(NUMG)= BYTES(1)
          IF (SEQ_POINT.GT.SEQ) THEN
C
C ****  Loop back to pick up next ganging
C
            PAKADR = TB90_PHYS_ADDR(2,SEQ_POINT)
            NUMG = NUMG + 1
            GOTO 30
          END IF
        END IF
c
        IF (NUMG.EQ.0) GOTO 40

C
C ****  Loop over ganging (NUMG) and fill CAEP
C
        DO 50 M = 1, NUMG
C
C ****  Set tag bits - (originally based on CUNPAK routine)
C 
          PAKADR=NEGLIM                 ! bit 0= 1 if limit test overridden
          PAKADR=IOR(PAKADR,2*SCALE)    ! bit 1= 0(x8), 1(x1)
          IF(.NOT.DO_PEDSUB .AND. .NOT.PEDSUB) 
     &      PAKADR=IOR(PAKADR,4)        ! bit 2= 0(ped sub),1(no ped sub)
          IF(.NOT.DO_GNSCOR .AND. .NOT.GNCORR) 
     &      PAKADR=IOR(PAKADR,8)        ! bit 3= 0(gains cor), 1(no gains cor)
          IF(.NOT.ZSUPRS .AND. .NOT.ZSUP) 
     &      PAKADR=IOR(PAKADR,16)       ! bit 4= 0(zero sup.), 1(not zero sup.)
          IF(.NOT.CONGEV) 
     &      PAKADR=IOR(PAKADR,32)       ! bit 5= 0(E in Gev), 1(E in ADC counts)
C
C ****  Pack addresses 
C
          BYTES(4)=ETAV(M)
          BYTES(3)=PHIV(M)
          BYTES(2)=LAYERV(M)
C
C ****  ASSUME THAT ALL CELLS GANGED HAVE THE SAME SAMPLING FRACTION
C       Depending on the flag in the RCP file, ENERGY is in GeV or ADC counts
C
          IF (CONGEV) THEN
            PULSE_HEIGHT = 
     &        PULSE_HEIGHT*TB90_CAD_GAIN(ETAV(M),PHIV(M),LAYERV(M))
            IF(ZSUP .AND. (ABS(PULSE_HEIGHT).LT.ECUTOF) ) GOTO 50
          END IF

C
C ****  fill CAEP
C
          POINT=POINT+1
          IQ(LCAEP+POINT)=PAKADR         ! packed physics address
          POINT=POINT+1
C
          Q(LCAEP+POINT)= PULSE_HEIGHT/NUMG       ! PH DIVIDED BY GANGED CELLS
          NONZCH=NONZCH+1
          IF (LPTCAEP) PTCAEP(ETAV(M),PHIV(M),LAYERV(M))= NONZCH ! CAEP POINTER
          IF ( (POINT+2) .GE.IQ(LCAEP-1) ) THEN
            FACTOR = FACTOR + 1.0
            CALL MZPUSH(IXCOM,LCAEP,0,NWCAEP,'I')
          END IF
   50   CONTINUE
C&ENDIF
   40 CONTINUE
      IF (LPTCAEP) PTZFLG=.FALSE.       ! set flag indicating PTCAEP is not 0
C
      IQ(LCAEP+3)=NONZCH  ! no. of channels
      NWCAEP = IQ(LCAEP-1) - 3
      IF(2*NONZCH .LT. NWCAEP ) THEN        ! reduce bank size if warranted
        NCH=NR*NONZCH-IQ(LCAEP-1) + 3
        CALL MZPUSH(IXCOM,LCAEP,0,NCH,'I')
      ENDIF
  999 RETURN
C
C ****  ERRMSG FORMATS
C
C&IF VAXVMS
 1001 FORMAT(' CONTROLLER WORD BAD =',Z9.8)
 1002 FORMAT(' CONTROL=',Z9.8,' NCH=',I5)
 1003 FORMAT(' STAT/VTX =',Z9.8)
 1004 FORMAT(' WORD=',Z9.8)
 1006 FORMAT(' PED=',E8.2,' WORD=',Z9.8)
C&ENDIF
 1007 FORMAT(' CARD SEQP SEQ=',I2,I3,1X,I4,' GAIN ',E8.2)
      END

