      SUBROUTINE ANLWENU
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Analyzes events, looking for W -> e nu
C-
C-   Inputs  : PNUT and PELC banks (PPHO if desired)
C-   Outputs : Enables writeout of interesting events
C-              sets bits in EVENT_HEAD bank word 30 to flag event:
C-              bit 32 set => a hot cell jet was found in the event
C-              bit 31 set => W->ev candidate (electron with matched track) 
C-              bit 30 set => W->ev candidate (electron w/o matched track)
C-   Controls: RCP
C-
C-   Rewritten  17-DEC-1990   Norman A. Graf
C-   Rewritten  7-DEC-1992   Norman A. Graf
C-   Updated  16-DEC-1992   Ulrich Heintz  use CLEANEM, write WEV stream
C-   Updated  19-JAN-1993   Ulrich Heintz  correct histograms for cal HV 
C-   Updated  19-FEB-1993   Ulrich Heintz  add WZ_HOT_CELL 
C-   Updated  17-JUL-1993   Ulrich Heintz  call CLEANEM to recompute mask 
C-   Updated  29-JUL-1993   Ulrich Heintz  eliminate HV correction
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVERT.LINK'
      COMMON /WEV/ NWEV
      LOGICAL FIRST,GOOD_EVENT
      DATA FIRST /.TRUE./
      INTEGER IER
C
      LOGICAL WRITE_W,OK,WZ_HOT_CELL,FLAG_EVENT,MICRO_BLANK
      LOGICAL DO_GAMMA_ANALYSIS,DO_ELECTRON_ANALYSIS
      LOGICAL SKIP_HOT_CELLS,CORRECT_HOT_CELLS,JET_CORRECTED,HOT_CELL
      INTEGER GZPELC,LPELC,GZPPHO,LPPHO,LCACL,LCASH,LPNUT,GZPNUT
      INTEGER I,J,K,NEL,NEM,TIGHT_IETA,LOOSE_IETA,NWEV,JBIT
      REAL EM_ET,EM_ET_CUT,TIGHT_ETMAX,LOOSE_ETMAX,PELC_ETMAX,PPHO_ETMAX
      REAL EL_E(5,20),EL_PHI(20),EL_ETA(20),EL_IETA(20)
      REAL EM_E(5,20),EM_PHI(20),EM_ETA(20),EM_IETA(20)
C
      INTEGER IPHI_HOT(5),IETA_HOT(5),RUN,RUNNO
      REAL EDPTH(5),PDPTH(5),ENERGY_HOT(5),NUTR(4,3),CORR_ET_MISS_CUT
C
      REAL ET_MISS,ET_MISS_CUT,ET_MISS_SCALAR,PHI_MISS,PNUT(2),ET_MISS1
C
      REAL FACTOR,MT_ELEC,MT_GAMMA,PT_W,PHI_MISS1,PNUTX,PNUTY,EM(16)
C
      INTEGER LJETS,GZJETS,NJETS,IVERS,NCJ,JER
      REAL JETVEC(7),JTHETA,JPHI,JETA
      REAL TEMPLATE(5,4)
      DATA TEMPLATE /
     &  1.,6.,0.7,0.,0.,                    ! CONE R=0.7
     &  1.,6.,0.5,0.,0.,                    ! CONE R=0.5
     &  1.,6.,0.3,0.,0.,                    ! CONE R=0.3
     &  2.,7.,1.,8.,1. /                    ! NN 1x1
C
      REAL E_HOT(3)
      REAL    CJ_ETA(25), CJ_PHI(25), CJ_PT(25), CJ_E(4,25)
      REAL DIFFPHI,DIFF_PHI,DPHICUT
      REAL J_ETMIN_CUT,J_ETA_CUT
      INTEGER CLEANEM_MASK,CLEANEM_MASK_LOOSE,STATUS
C
      INTEGER NUMRUN,CC
C----------------------------------------------------------------------
C
      GOOD_EVENT=.FALSE.
      CALL DHDIR('WZ_RCP','WENU_DIRECTORY',IER,' ') ! Create/Set HBOOK directory
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('WZ','ANLWENU',' ERROR SETTING HBOOK DIRECTORY','W')
      ENDIF
C
      IF(FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('WZ_RCP')
        CALL EZGET_l('W_E_WRITE_W',WRITE_W,IER)
        IF(IER.EQ.0)CALL EZGET_l('W_E_SKIP_HOT_CELLS',SKIP_HOT_CELLS
     &       ,IER)
        IF(IER.EQ.0)CALL EZGET_l('W_E_CORRECT_HOT_CELLS',
     &    CORRECT_HOT_CELLS,IER)
        IF(IER.EQ.0)CALL EZGET_l('W_E_DO_ELECTRON_ANALYSIS',
     &    DO_ELECTRON_ANALYSIS,IER)
        IF(IER.EQ.0)CALL EZGET_l('W_E_DO_GAMMA_ANALYSIS',
     &    DO_GAMMA_ANALYSIS,IER)
        IF(IER.EQ.0)CALL EZGET_i('W_E_CLEANEM_MASK',CLEANEM_MASK,IER)
        CLEANEM_MASK_LOOSE=IAND(CLEANEM_MASK,z'FFFF') ! no tracking cuts
        IF(IER.EQ.0)CALL EZGET('W_E_ET_CUT',EM_ET_CUT,IER)
        IF(IER.EQ.0)CALL EZGET('W_E_ETMISS_CUT',ET_MISS_CUT,IER)
        IF(IER.EQ.0)CALL EZGET('W_E_CORR_ETMISS_CUT',CORR_ET_MISS_CUT,
     &    IER)
        IF(IER.EQ.0)CALL EZGET('W_E_J_ETMIN_CUT',J_ETMIN_CUT,IER)
        IF(IER.EQ.0)CALL EZGET('W_E_J_ETA_CUT',J_ETA_CUT,IER)
        IF(IER.EQ.0)CALL EZGET_l('W_E_FLAG_EVENT',FLAG_EVENT,IER)
        IF(IER.EQ.0)CALL EZGET_l('W_E_MICRO_BLANK',MICRO_BLANK,IER)
        IF(IER.NE.0)CALL ERRMSG('RCP','ANLWENU',
     &    'error getting RCP parameters','W') 
        CALL EZRSET
C
C... the following are histograms of distributions before cut in the sequence
C    they are performed
        CALL HBOOK1(10,'missing Et',100,0.,100.,0.)
        CALL HBOOK1(11,'missing Et phi',72,0.,SNGL(TWOPI),0.)
        CALL HBOOK2(13,'corrected missing Et vs missing Et',100,0.,100.,
     &    100,0.,100.,0.)
        CALL HBOOK1(14,'largest PELC Et',100,0.000001,100.,0.)
        CALL HBOOK1(15,'largest PPHO Et',100,0.000001,100.,0.)
        CALL HBOOK1(16,'largest tight electron Et (CC)',100,0.000001,
     &    100.,0.)
        CALL HBOOK1(17,'largest loose electron Et (CC)',100,0.000001,
     &    100.,0.)
        CALL HBOOK1(18,'largest tight electron Et (EC)',100,0.000001,
     &    100.,0.)
        CALL HBOOK1(19,'largest loose electron Et (EC)',100,0.000001,
     &    100.,0.)
C
        CALL HBOOK1(102,'jet multiplicity (tight electrons)',10,-0.5,
     &    9.5,0.)
        CALL HBOOK1(502,'jet multiplicity (loose electrons)',10,-0.5,
     &    9.5,0.)
C
        CALL HBOOK1(100,'Mt of electron clusters',60,0.,120.,0.)
        CALL HBOOK1(101,'Pt of W(e)',250,0.,250.,0.)
        CALL HBOOK1(109,'E of electron clusters',60,0.,240.,0.)
        CALL HBOOK1(110,'Et of electron clusters',60,0.,120.,0.)
        CALL HBOOK1(111,'Missing Et of W(e)',60,0.,120.,0.)
        CALL HBOOK1(114,'Miss Et-Et of electron clusters',100,-25.,25.,
     &    0.)
        CALL HBOOK2(116,'NCJ vs Miss Et-Et of electron clusters',10,0.,
     &    10.,100,-25.,25.,0.)
C
        CALL HBOOK2(120,'Eta vs phi of electrons',40,-4.,4.,
     &                                          32,0.,sngl(twopi),0.)
        CALL HBOOK2(121,'detector eta vs phi of tight electrons',80,
     &    -40.,40.,32,0.,sngl(twopi),0.)
        CALL HBOOK2(153,'NCJ vs Mt  of electron clusters',5,-0.5,4.5,
     &      100,0.,200.,0.)
        CALL HBOOK2(154,'NCJ vs Pt W of electron clusters',5,-0.5,4.5,
     &      100,0.,200.,0.)
        CALL HBOOK1(155,'Mt e CC  NCJ=0',100,0.,100.,0.)
        CALL HBOOK1(156,'Mt e ECN NCJ=0',100,0.,100.,0.)
        CALL HBOOK1(157,'Mt e ECS NCJ=0',100,0.,100.,0.)
        CALL HBOOK1(160,'CC W+0 electron Et',60,0.,120.,0.)
        CALL HBOOK1(161,'ECN W+0 electron Et',60,0.,120.,0.)
        CALL HBOOK1(162,'ECS W+0 electron Et',60,0.,120.,0.)
        CALL HBOOK1(170,'(W+1) electron Et',60,0.,120.,0.)
        CALL HBOOK2(171,'(W+1) Eta vs phi of W electrons',40,-4.,4.,
     &                                             32,0.,sngl(twopi),0.)
        CALL HBOOK1(172,'(W+1) JET Et',60,0.,120.,0.)
        CALL HBOOK2(173,'Eta vs phi of (W+1) JET',40,-4.,4.,
     &                                             32,0.,sngl(twopi),0.)
C
        CALL HBOOK1(500,'Mt of photon clusters',60,0.,120.,0.)
        CALL HBOOK1(501,'Pt of W(g)',100,0.,250.,0.)
        CALL HBOOK1(509,'E of photon clusters',60,0.,240.,0.)
        CALL HBOOK1(510,'Et of photon clusters',60,0.,120.,0.)
        CALL HBOOK1(511,'Missing Et of W(g)',60,0.,120.,0.)
        CALL HBOOK1(514,'Miss Et-Et of photon clusters',100,-25.,25.,0.)
        CALL HBOOK2(516,'NCJ vs Miss Et-Et of photon clusters',10,0.,
     &    10.,100,-25.,25.,0.)
        CALL HBOOK2(520,'Eta vs phi of photons',40,-4.,4.,
     &                                             32,0.,sngl(twopi),0.)
        CALL HBOOK2(521,'detector eta vs phi of loose electrons',80,
     &    -40.,40.,32,0.,sngl(twopi),0.)
        CALL HBOOK2(553,'NCJ vs Mt  of photon clusters',5,-0.5,4.5,
     &      100,0.,200.,0.)
        CALL HBOOK2(554,'NCJ vs Pt W of photon clusters',5,-0.5,4.5,
     &      100,0.,200.,0.)
        CALL HBOOK1(555,'Mt g CC  NCJ=0',100,0.,100.,0.)
        CALL HBOOK1(556,'Mt g ECN NCJ=0',100,0.,100.,0.)
        CALL HBOOK1(557,'Mt g ECS NCJ=0',100,0.,100.,0.)
        CALL HBOOK1(560,'CC W+0 photon Et',60,0.,120.,0.)
        CALL HBOOK1(561,'ECN W+0 photon Et',60,0.,120.,0.)
        CALL HBOOK1(562,'ECS W+0 photon Et',60,0.,120.,0.)
        CALL HBOOK1(570,'(W+1) photon Et',60,0.,120.,0.)
        CALL HBOOK2(571,'(W+1) Eta vs phi of W photons',40,-4.,4.,
     &                                             32,0.,sngl(twopi),0.)
        CALL HBOOK1(572,'(W+1) JET Et(g)',60,0.,120.,0.)
        CALL HBOOK2(573,'Eta vs phi of (W+1) JET(g)',40,-4.,4.,
     &                                             32,0.,sngl(twopi),0.)
C
      ENDIF
C
      CALL FLGSET('WRITE_STREAM_WEV',.FALSE.)       ! WEV stream
      IF(FLAG_EVENT)IQ(LHEAD+30)=IAND(IQ(LHEAD+30),z'1FFFFFFF')
C
C **** check for micro blank bit
C
      IF(MICRO_BLANK.AND.JBIT(IQ(LHEAD+30),1).EQ.1)GOTO 999
C
C ****  MISSING ET...
C
      RUN=RUNNO()
      LPNUT = GZPNUT(2)
      IF(LPNUT.GT.0)THEN
        ET_MISS = Q(LPNUT+7)
        PHI_MISS = Q(LPNUT+10)
        PNUT(1) = ET_MISS*COS(PHI_MISS)
        PNUT(2) = ET_MISS*SIN(PHI_MISS)
        CALL HFILL(10,ET_MISS,0.,1.)
        CALL HFILL(11,PHI_MISS,0.,1.)
      ELSE
        CALL ERRMSG('no PNUT(2) bank','ANLWENU','skip this event','W')
        GOTO 999
      ENDIF
C
C ****  Now identify events with "HOT CELL" jets and correct missing ET...
C
      HOT_CELL=WZ_HOT_CELL(E_HOT)  
      IF ( HOT_CELL ) THEN  ! if a hot cell-jet was found
        IF(SKIP_HOT_CELLS) GOTO 999         ! skip events with hot cells
        IF(FLAG_EVENT)IQ(LHEAD+30)=IOR(IQ(LHEAD+30),z'80000000') ! flag in event_head bank
      ENDIF
      PNUTX  = PNUT(1)+E_HOT(1)
      PNUTY  = PNUT(2)+E_HOT(2)
      PHI_MISS1= ATAN2(PNUTY,PNUTX)
      ET_MISS1 = SQRT(PNUTX**2+PNUTY**2)
      CALL HFILL(13,ET_MISS,ET_MISS1,1.)  
      IF(ET_MISS.GT.ET_MISS_CUT.OR.ET_MISS1.GT.CORR_ET_MISS_CUT) THEN
        IF(CORRECT_HOT_CELLS.AND.HOT_CELL)THEN       ! correct missing Et for hot cells
          PNUT(1)=PNUTX
          PNUT(2)=PNUTY
          ET_MISS=ET_MISS1
          JER = 1
          JET_CORRECTED = .TRUE.
        ENDIF
C
C **** Jets...
C
        CALL SET_CAPH('CONE_JET',TEMPLATE(1,1),IER)
        CALL GTJETS_NOEP
        CALL GTJETS_TOTAL(NJETS,IER)
        NCJ = 0
        DO I = 1,NJETS
          CALL GTJETS(I,IVERS,JETVEC,JTHETA,JPHI,JETA,JER)
C
          IF(JER.EQ.0 .AND. JETVEC(5) .GT. J_ETMIN_CUT .AND.
     &       ABS(JETA).LT.J_ETA_CUT) THEN
            NCJ = NCJ + 1
            DO K = 1,4
              CJ_E(K,NCJ) = JETVEC(K)
            ENDDO
            CJ_PT(NCJ)  = JETVEC(5)
            CJ_PHI(NCJ) = JPHI
            CJ_ETA(NCJ) = JETA
          ENDIF
        ENDDO
        CALL GTJETS_JNEP_RESET
        CALL RESET_CAPH
C
C ****  now for electrons...
C
        NEL = 0   ! number of tight electrons
        NEM = 0   ! number of loose electrons
        TIGHT_ETMAX=0.
        LOOSE_ETMAX=0.
        PELC_ETMAX=0.
        PPHO_ETMAX=0.
        LPELC = GZPELC()
        DO WHILE(LPELC.GT.0)
          EM_ET = Q(LPELC+7)
          IF(EM_ET.GT.PELC_ETMAX)PELC_ETMAX=EM_ET
          CALL CLEANEM(LPELC,1,OK,STATUS)
          OK = IAND(STATUS,CLEANEM_MASK).EQ.0 
          IF(OK) THEN    ! if it passes tight electron mask
            IF(EM_ET.GT.TIGHT_ETMAX)THEN
              TIGHT_ETMAX=EM_ET
              TIGHT_IETA=IFIX(Q(LPELC+19))
            ENDIF
            IF(EM_ET.GT.EM_ET_CUT) THEN
              NEL = NEL + 1
              EL_E(1,NEL)  = Q(LPELC+3)
              EL_E(2,NEL)  = Q(LPELC+4)
              EL_E(3,NEL)  = Q(LPELC+5)
              EL_E(4,NEL)  = Q(LPELC+6)
              EL_E(5,NEL)  = Q(LPELC+7)
              EL_PHI(NEL)  = Q(LPELC+10)
              EL_ETA(NEL)  = Q(LPELC+9)
              EL_IETA(NEL) = Q(LPELC+19)
            ENDIF
          ELSE
            CALL CLEANEM(LPELC,1,OK,STATUS)
            OK = IAND(STATUS,CLEANEM_MASK_LOOSE).EQ.0 
            IF(OK) THEN  ! if it passes loose electron mask
              IF(EM_ET.GT.LOOSE_ETMAX)THEN
                LOOSE_ETMAX=EM_ET
                LOOSE_IETA=IFIX(Q(LPELC+19))
              ENDIF
              IF(EM_ET.GT.EM_ET_CUT) THEN
                NEM = NEM + 1
                EM_E(1,NEM)  = Q(LPELC+3)
                EM_E(2,NEM)  = Q(LPELC+4)
                EM_E(3,NEM)  = Q(LPELC+5)
                EM_E(4,NEM)  = Q(LPELC+6)
                EM_E(5,NEM)  = Q(LPELC+7)
                EM_PHI(NEM)  = Q(LPELC+10)
                EM_ETA(NEM)  = Q(LPELC+9)
                EM_IETA(NEM) = Q(LPELC+19)
              ENDIF
            ENDIF
          ENDIF
          LPELC = LQ(LPELC)
        ENDDO
C
        LPPHO = GZPPHO()
        DO WHILE(LPPHO.GT.0)
          EM_ET = Q(LPPHO+7)
          IF(EM_ET.GT.PPHO_ETMAX)PPHO_ETMAX=EM_ET
          CALL CLEANEM(LPPHO,0,OK,STATUS)
          OK = IAND(STATUS,CLEANEM_MASK_LOOSE).EQ.0 
          IF(OK) THEN   ! if it passes loose electron mask
            LCACL = LQ(LPPHO-2)
            LCASH = LQ(LCACL-2)
            CALL CEMDPTH(LCASH,IETA_HOT,IPHI_HOT,EDPTH,PDPTH,ENERGY_HOT)
            IF(EM_ET.GT.LOOSE_ETMAX)THEN
              LOOSE_ETMAX=EM_ET
              LOOSE_IETA=IETA_HOT(3)
            ENDIF
            IF(EM_ET.GT.EM_ET_CUT) THEN
              NEM = NEM + 1
              EM_E(1,NEM)  = Q(LPPHO+3)
              EM_E(2,NEM)  = Q(LPPHO+4)
              EM_E(3,NEM)  = Q(LPPHO+5)
              EM_E(4,NEM)  = Q(LPPHO+6)
              EM_E(5,NEM)  = Q(LPPHO+7)
              EM_PHI(NEM)  = Q(LPPHO+10)
              EM_ETA(NEM)  = Q(LPPHO+9)
              EM_IETA(NEM) = FLOAT(IETA_HOT(3))
            ENDIF
          ENDIF
          LPPHO = LQ(LPPHO)
        ENDDO
        CALL HFILL(14,PELC_ETMAX,0.,1.)
        CALL HFILL(15,PPHO_ETMAX,0.,1.)
        IF(ABS(TIGHT_IETA).LT.13)THEN
          CALL HFILL(16,TIGHT_ETMAX,0.,1.)
        ELSE
          CALL HFILL(18,TIGHT_ETMAX,0.,1.)
        ENDIF
        IF(TIGHT_ETMAX.LE.EM_ET_CUT)THEN
          IF(ABS(LOOSE_IETA).LT.13)THEN
            CALL HFILL(17,LOOSE_ETMAX,0.,1.)
          ELSE
            CALL HFILL(19,LOOSE_ETMAX,0.,1.)
          ENDIF
        ENDIF
C
        IF(DO_ELECTRON_ANALYSIS) THEN
          DO I = 1,NEL
            GOOD_EVENT=.TRUE.
            FACTOR = 1 - COS(EL_PHI(I) - PHI_MISS)
            MT_ELEC = SQRT(2*ET_MISS*EL_E(5,I)*FACTOR)
C
C ****  pt of W
C
            PT_W = SQRT( (PNUT(1)+ EL_E(1,I))**2+
     &                    (PNUT(2)+ EL_E(2,I))**2 )
C
            IF(FLAG_EVENT)IQ(LHEAD+30)=IOR(IQ(LHEAD+30),z'40000000') ! flag in event_head bank
            CC = 0.
            IF(ABS(EL_IETA(I)).LT.13) CC = 1.
C
            CALL HFILL(102,FLOAT(NCJ),0.,1.)
            CALL hfill(100,mt_elec,0.,1.)
            CALL hfill(101,pt_w,0.,1.)
            CALL hfill(110,el_e(5,i),0.,1.)
            CALL hfill(111,et_miss,0.,1.)
            CALL hfill(114,et_miss-el_e(5,i),0.,1.)
            CALL hfill(120,el_eta(i),el_phi(i),1.)
            CALL hfill(121,el_ieta(i),el_phi(i),1.)
            CALL hfill(153,float(ncj),mt_elec,1.)
            CALL hfill(154,float(ncj),pt_w,1.)
C
            IF(ncj.EQ.0) THEN
              IF(abs(el_ieta(i)).LT.13 ) THEN   ! CC
                CALL hfill(155,mt_elec,0.,1.)
                CALL hfill(160,el_e(5,i),0.,1.)
              ELSEIF(el_ieta(i).GT.0) THEN      ! ECS
                CALL hfill(157,mt_elec,0.,1.)
                CALL hfill(162,el_e(5,i),0.,1.)
              ELSE                              ! ECN
                CALL hfill(156,mt_elec,0.,1.)
                CALL hfill(161,el_e(5,i),0.,1.)
              ENDIF
            ENDIF
C
            IF(ncj .EQ. 1) THEN
              CALL hfill(170,el_e(5,i),0.,1.)
              CALL hfill(171,el_ieta(i),el_phi(i),1.)
              CALL hfill(172,cj_pt(ncj),0.,1.)
              CALL hfill(173,cj_eta(ncj),cj_phi(ncj),1.)
            ENDIF
C
          ENDDO
        ENDIF
C
        IF(DO_GAMMA_ANALYSIS) THEN
          DO I = 1,NEM
            GOOD_EVENT=.TRUE.
            FACTOR = 1 - COS(EM_PHI(I) - PHI_MISS)
            MT_GAMMA = SQRT(2*ET_MISS*EM_E(5,I)*FACTOR)
            PT_W = SQRT( (PNUT(1)+ EM_E(1,I))**2+
     &                    (PNUT(2)+ EM_E(2,I))**2 )
            IF(FLAG_EVENT)IQ(LHEAD+30)=IOR(IQ(LHEAD+30),z'20000000') ! flag in event_head bank
            CC = 0.
            IF(ABS(EM_IETA(I)).LT.13) CC = 1.
C
            CALL HFILL(502,FLOAT(NCJ),0.,1.)
            CALL hfill(500,mt_gamma,0.,1.)
            CALL hfill(501,pt_w,0.,1.)
            CALL hfill(510,em_e(5,i),0.,1.)
            IF(cc.EQ.1) THEN
              CALL hfill(517,float(ncj),em_e(4,i),1.)
              CALL hfill(518,float(ncj),em_e(4,i),1.)
            ENDIF
            CALL hfill(511,et_miss,0.,1.)
            CALL hfill(514,et_miss-em_e(5,i),0.,1.)
            CALL hfill(520,em_eta(i),em_phi(i),1.)
            CALL hfill(521,em_ieta(i),em_phi(i),1.)
            CALL hfill(553,float(ncj),mt_gamma,1.)
            CALL hfill(554,float(ncj),pt_w,1.)
C
            IF(ncj.EQ.0)THEN
              IF(abs(em_ieta(i)).LT.13) THEN     ! CC
                CALL hfill(555,mt_elec,0.,1.)
                CALL hfill(560,em_e(5,i),0.,1.)
              ELSEIF(em_ieta(i).GT.0) THEN       ! ECS
                CALL hfill(557,mt_elec,0.,1.)
                CALL hfill(562,em_e(5,i),0.,1.)
              ELSE                               ! ECN
                CALL hfill(556,mt_elec,0.,1.)
                CALL hfill(561,em_e(5,i),0.,1.)
              ENDIF
            ENDIF
C
            IF(ncj .EQ. 1) THEN
              CALL hfill(570,em_e(5,i),0.,1.)
              CALL hfill(571,em_ieta(i),em_phi(i),1.)
              CALL hfill(572,cj_pt(ncj),0.,1.)
              CALL hfill(573,cj_eta(ncj),cj_phi(ncj),1.)
            ENDIF
          ENDDO
        ENDIF
C
        IF(GOOD_EVENT)THEN
          NWEV=NWEV+1
          IF(WRITE_W) CALL FLGSET('WRITE_STREAM_WEV',.TRUE.)
        ENDIF
C
      ENDIF   !end of missing ET loop
C
  999 RETURN
      END
