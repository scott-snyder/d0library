      LOGICAL FUNCTION TOP_TOP_EMU
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
C-   Controls: TOP_TOP_EMU_RCP 
C-   
C-   Returned value  : true for events that pass 
C-
C-   Created  12-JAN-1993   Meenakshi Narain
C-   Modified 18-JUL-1993   Pushpa C. Bhat   
C-   Modified  7-APR-1994   Jim Cochran - update to 1B selection   
C-                          Change 2NN to 1NN, reject A-layer stubs
C-            15-NOV-1995   jc - update for d0fix streaming
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER K,IER
      INTEGER LPELC,LPPHO,LVERH,LVERT,NPVERT,GZVERH
      EXTERNAL GZVERH
      INTEGER GZPELC,GZPPHO,ISTAT,NVAR
      INTEGER EVTNUM,RUNNUM,RUNNO,NV,NVERT
      INTEGER NGELEC,NGPHOT,NELEC,NPHOT,NGELPH
      REAL    ELETMIN,PHETMIN,ZVTX_INFO(3, 1)
      REAL    CQUAN(50),CHISQR,EMF,EISOL,ET_NEW(10)
      REAL    ET,ELC_ET,PHOT_ET
      LOGICAL FIRST,OKZ,ETPASS
      LOGICAL PASSED,STATUS
      DATA    FIRST/.TRUE./
C
      LOGICAL COSREJ,XOCT,PASSED_MU
      INTEGER IFW4CUT,NCD_OR,LPARH,NPMUO,IPMUO,LPMUO,LMUOT
      REAL    ETACUT,PTCUT,CIMP_RZ,CAL_OR,CISO_OR
      REAL    ETAMU,PTMU,ECAL_1NN,CALISO_1NN,RZ_IMPACT,PTV
      REAL    PTFIX
      INTEGER IFW4,IFW2,NCDMATCH
      INTEGER GZPMUO,GZPARH
      LOGICAL EZERR,MU_RECOV
      EXTERNAL GZPMUO,GZPARH,EZERR

C----------------------------------------------------------------------
      TOP_TOP_EMU   = .FALSE.
      PASSED    = .FALSE.
      PASSED_MU = .FALSE.
      MU_RECOV  = .FALSE.
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL INRCP('TOP_TOP_EMU_RCP',IER)       
        IF(IER.EQ.0) THEN
          CALL EZPICK('TOP_TOP_EMU_RCP')  
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
            CALL ERRMSG('TOP_TOP_EMU','TOP_TOP_EMU_RCP',
     &        'ERROR GETTING TOP_TOP_EMU RCP VALUES','W')
          ENDIF
          CALL EZRSET
        ELSE
          CALL ERRMSG('TOP_TOP_EMU','TOP_TOP_EMU_RCP',
     &      'ERROR READING TOP_TOP_EMU RCP FILE','W')
        ENDIF
      ENDIF
C
C------------------------------------------------------------------------------
C
      IF(LHEAD.NE.0) EVTNUM = IQ(LHEAD+9) 
      RUNNUM = RUNNO()
C
C ****  Get Zvertex
C
      CALL VERTEX_INFO(1,NV,ZVTX_INFO,OKZ)
C            Only consider the main primary vertex

C
C ****  electrons
C
      NELEC=0
      NGELEC=0
      LPELC=GZPELC()
      ELC_ET=ELETMIN
      IF(LPELC.NE.0) THEN
        DO WHILE (LPELC.GT.0)
          ETPASS= .FALSE.
          ET=Q(LPELC+7)
          IF(ET.GE.ELC_ET) etpass=.true.
          if (etpass) GOTO 50
          if (nv.gt.1) then
            call em_etzv(lpelc,nvert,et_new)
            do k = 1, nvert
              if (et_new(k).ge.ELC_ET) then
                etpass = .true.
                IF(ETPASS) GOTO 50
              endif
            enddo
          endif
          IF(.NOT.ETPASS) GOTO 60
   50     continue
          NELEC=NELEC+1
          call cleanem(lpelc,0,status,istat)
          call cleanem_cquans(nvar,cquan)
          chisqr = cquan(4)
          emf = cquan(9)
          eisol = cquan(13)
          status = (chisqr.lt.300.and.eisol.lt.0.3)
          IF (.NOT. STATUS) GOTO 60
          NGELEC= NGELEC+1
   60     CONTINUE
          LPELC=LQ(LPELC)          
        ENDDO
      ENDIF
C
C
C ****  photons
C
      NPHOT=0
      NGPHOT=0
C
      LPPHO=GZPPHO()
      PHOT_ET=PHETMIN
      IF(LPPHO.NE.0) THEN
        DO WHILE (LPPHO.GT.0)
          ET=Q(LPPHO+7)
          ETPASS= .FALSE.
          IF(ET.GE.PHOT_ET) etpass=.true.
          if (etpass) GOTO 70
          if (nv.gt.1) then
            call em_etzv(lppho,nvert,et_new)
            do k = 1, nvert
              if (et_new(k).ge.PHOT_ET) then
                etpass = .true.
                IF(ETPASS) GOTO 70
              endif
            enddo
          endif
          IF(.NOT.ETPASS) GOTO 80
   70     continue
          NPHOT=NPHOT+1
          call cleanem(lppho,0,status,istat)
          call cleanem_cquans(nvar,cquan)
          chisqr = cquan(4)
          emf = cquan(9)
          eisol = cquan(13)
          status = (chisqr.lt.300.and.eisol.lt.0.3)
          IF (.NOT. STATUS) GOTO 80
          NGPHOT= NGPHOT+1
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
C check multiple vertices...
            NPVERT=1
            PTV=0
            LVERH=GZVERH()
            IF( LVERH.GT.0 ) NPVERT=IQ(LVERH+2)
            IF( NPVERT.GT.1) THEN  ! Must have mult. verts.
              LVERT=LQ(LVERH-1)
              IF( LVERT.GT.0) PTV=PTFIX(LPMUO,LVERT)
              IF(PTV.GT.PTMU) THEN
                IF(PTMU.LT.PTCUT.AND.PTV.GE.PTCUT) MU_RECOV=.TRUE.
              ENDIF
            ENDIF
C do cuts...
            IF(IQ(LMUOT+4).EQ.5) GOTO 1000    ! reject A-layer stubs
            IF(PTMU.LT.PTCUT.AND.PTV.LT.PTCUT) GOTO 1000
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
      NGELPH = NGELEC+NGPHOT
      IF (NGELPH.GE.1 .AND. PASSED_MU) PASSED = .TRUE. 
C
      TOP_TOP_EMU = PASSED 
      
  999 RETURN
      END
