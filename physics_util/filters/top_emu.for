      LOGICAL FUNCTION TOP_EMU
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Filter ttbar-->emu candidate events from ELF stream 
C-                          according to the following criteria 
C-     - PELC  with pt > ELEC_MIN_ET 
C-     - PPHO  with pt > PHOT_MIN_ET 
C-     - GOOD MUON based on on the following selection...
C-        -    crossing octant cut
C-        -    ifw4<=IFW4CUT (typically 1)
C-        -    pt>PTCUT
C-        -    |eta|<ETACUT (typically 1.7)
C-        -    cdmatch OR Ecal_1NN>CAL_OR GeV OR Iso_1NN>CISO_OR
C-        -    impact (vertex) <  CIMP_RZ cm (typically 30 cm)
C-        -    muctag
C-         
C-   Controls: TOP_EMU_RCP 
C-   
C-   Returned value  : true for events that pass 
C-
C-   Created  12-JAN-1993   Meenakshi Narain
C-   Modified 18-JUL-1993   Pushpa C. Bhat   
C-   Modified  7-APR-1994   Jim Cochran - update to 1B selection   
C-                          Change 2NN to 1NN, reject A-layer stubs
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER I,IER
      INTEGER LPELC,LPPHO,LHMTR
      INTEGER GZPELC,GZPPHO 
      INTEGER EVTNUM,RUNNUM,RUNNO
      INTEGER NELEC,NPHOT,NELPH
      REAL    ELETMIN, PHETMIN
      REAL    ET,ECLUS,ELC_ET,PHOT_ET,ISOL
      LOGICAL FIRST,FLAG_ELEC,FLAG_PHOT
      LOGICAL PASSED,LOK
      DATA    FIRST/.TRUE./
C
      LOGICAL COSREJ,XOCT,PASSED_MU
      INTEGER IFW4CUT,NCD_OR,LPARH,NPMUO,IPMUO,LPMUO,NS,LMUOT
      REAL    ETACUT,PTCUT,CIMP_RZ,CAL_OR,CISO_OR
      REAL    ETAMU,PTMU,ECAL_1NN,CALISO_1NN,RZ_IMPACT
      REAL    COS_TRAN
      INTEGER IFW4,IFW2,NCDMATCH
      INTEGER GZPMUO,GZPARH
      LOGICAL EZERR
      EXTERNAL GZPMUO,GZPARH,EZERR

C----------------------------------------------------------------------
      TOP_EMU   = .FALSE.
      PASSED    = .FALSE.
      PASSED_MU = .FALSE.
      FLAG_ELEC = .FALSE.
      FLAG_PHOT = .FALSE.
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL INRCP('TOP_EMU_RCP',IER)       
        IF(IER.EQ.0) THEN
          CALL EZPICK('TOP_EMU_RCP')  
          CALL EZGET('ELEC_MIN_ET',ELETMIN,IER)
          CALL EZGET('PHOT_MIN_ET',PHETMIN,IER)
          CALL EZGET('MU_PT', PTCUT, IER)
          CALL EZGET('MU_ETA', ETACUT, IER)
          CALL EZGET_i('MU_IFW4', IFW4CUT, IER)
          CALL EZGET_l('MU_MUCTAG', COSREJ, IER)
          CALL EZGET_l('MU_XOCT', XOCT, IER)
          CALL EZGET('MU_IMP_RZ', CIMP_RZ,IER)
          CALL EZGET_i('MU_CD_OR', NCD_OR, IER)
          CALL EZGET('MU_CAL_OR', CAL_OR, IER)
          CALL EZGET('MU_ISO_OR', CISO_OR, IER)
          IF (IER.NE.0) THEN
            CALL ERRMSG('TOP_EMU','TOP_EMU_RCP',
     &        'ERROR GETTING TOP_EMU RCP VALUES','W')
          ENDIF
          CALL EZRSET
        ELSE
          CALL ERRMSG('TOP_EMU','TOP_EMU_RCP',
     &      'ERROR READING TOP_EMU RCP FILE','W')
        ENDIF
      ENDIF
C
C------------------------------------------------------------------------------
C
      IF(LHEAD.NE.0) EVTNUM = IQ(LHEAD+9) 
      RUNNUM = RUNNO()
C
C
C ****  electrons
C
      NELEC=0
      LPELC=GZPELC()
      ELC_ET=ELETMIN
      IF(LPELC.NE.0) THEN
        DO WHILE (LPELC.GT.0)
          ET=Q(LPELC+7)
          IF(ET.LT.ELC_ET) GOTO 60 
C
          NELEC=NELEC+1
          FLAG_ELEC = .TRUE. 
C
   60     CONTINUE
          LPELC=LQ(LPELC)          
        ENDDO
      ENDIF
C
C
C ****  photons
C
      NPHOT=0
      LPPHO=GZPPHO()
      PHOT_ET=PHETMIN
      IF(LPPHO.NE.0) THEN
        DO WHILE (LPPHO.GT.0)
          ET=Q(LPPHO+7)
          IF(ET.LT.PHOT_ET) GOTO 80 
C
          NPHOT=NPHOT+1
          FLAG_PHOT = .TRUE. 
C
   80     CONTINUE
          LPPHO=LQ(LPPHO)          ! pointer to next photon
        ENDDO
      ENDIF
C
C
C ****  muons
C

C
C check pmuo bank
C
      LPARH=GZPARH()
      NPMUO = 0
      IF (LPARH.GT.0) THEN
        NPMUO=IQ(LPARH+2)
      ENDIF
C
C find number of good muons in the event
C
      IF(NPMUO.GT.0) THEN
        DO IPMUO=1,NPMUO
          LPMUO = GZPMUO(IPMUO)
          IF(LPMUO.GT.0) THEN
C get muon information from PMUO bank
            LMUOT=LQ(LPMUO-2)
            ETAMU = Q(LPMUO+16)
            PTMU = Q(LPMUO+14)
            IFW4 = IQ(LPMUO+9)
            IFW2 = IQ(LPMUO+44)
            NCDMATCH = IQ(LPMUO+6)
            ECAL_1NN = Q(LPMUO+84)
            CALISO_1NN = Q(LPMUO+29)
            RZ_IMPACT = Q(LPMUO+41)
C do cuts...
            IF(IQ(LMUOT+4).EQ.5) GOTO 1000    ! reject A-layer stubs
            IF(PTMU.LT.PTCUT) GOTO 1000
            IF(ABS(ETAMU).GT.ETACUT) GOTO 1000
            IF(IFW4.GT.IFW4CUT) GOTO 1000
            IF(COSREJ) THEN
              IF(BTEST(IFW2,6)) GOTO 1000
              IF(BTEST(IFW2,7)) GOTO 1000
            ENDIF
            IF(XOCT) THEN
              IF(BTEST(IFW2,8)) GOTO 1000
            ENDIF
            IF(RZ_IMPACT.GT.CIMP_RZ) GOTO 1000
C or of three conditions
            IF(NCDMATCH.LT.NCD_OR .AND. ECAL_1NN.LT.CAL_OR .AND.
     &        CALISO_1NN.LT.CISO_OR) GOTO 1000
C
            PASSED_MU = .TRUE.
C
 1000       CONTINUE    
          ENDIF
        ENDDO
      ENDIF
C
C
C ****  Write out event?
C
      NELPH = NELEC+NPHOT
      IF (NELPH.GE.1 .AND. PASSED_MU) PASSED = .TRUE. 
C
      TOP_EMU = PASSED 
      
  999 RETURN
      END
