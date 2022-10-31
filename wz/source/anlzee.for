      SUBROUTINE ANLZEE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Analyzes events, looking for Z -> ee
C-
C-   Inputs  : PELC, PPHO, and CACL banks
C-   Outputs : An exaltation of histograms
C-              sets bits in EVENT_HEAD bank word 30 to flag event:
C-              bit 29 set => Z->ee candidate (both electrons w matched tracks) 
C-              bit 28 set => Z->ee candidate (one electron w matched tracks) 
C-              bit 27 set => Z->ee candidate (both electrons w/o matched tracks) 
C-   Controls: RCP
C-
C-   Rewritten  17-DEC-1990   Norman A. Graf
C-   Rewritten  7-DEC-1992   Norman A. Graf
C-   Updated  16-DEC-1992   Ulrich Heintz  use CLEANEM, write ZEE stream
C-   Updated  19-JAN-1993   Ulrich Heintz  correct histograms for cal HV 
C-   Updated  19-FEB-1993   Ulrich Heintz  add CLEANEM_MASK_LOOSE 
C-   Updated  17-JUL-1993   Ulrich Heintz  call CLEANEM to recompute mask 
C-   Updated  29-JUL-1993   Ulrich Heintz  eliminate HV correction 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      COMMON /ZEE/ NZEE
      LOGICAL FIRST,GOOD_EVENT
      DATA FIRST /.TRUE./
      INTEGER IER
C
      LOGICAL OK,WRITE_Z,FLAG_EVENT
      LOGICAL DO_LOOSE_ELECTRONS,DO_ONE_LOOSE_ELECTRON
      INTEGER CLEANEM_MASK,CLEANEM_MASK_LOOSE
      INTEGER GZPELC,LPELC,GZPPHO,LPPHO
      INTEGER I,J,K,NEL,NEM,STATUS,RUNNO,RUN,NZEE
      REAL EM_ET,EM_ET_CUT
      REAL PELC_ETMAX,PPHO_ETMAX,TIGHT_ETMAX,LOOSE_ETMAX
      REAL PELC_ETMAX2,PPHO_ETMAX2,TIGHT_ETMAX2,LOOSE_ETMAX2
      REAL EL_E(5,20),EL_PHI(20),EL_ETA(20),EL_IETA(20),EM(16),NUTR(4,3)
      REAL EM_E(5,20),EM_PHI(20),EM_ETA(20),EM_IETA(20),EM_TYP(20)
C
      REAL PEM,EEM,MEM,PT_Z
C
      INTEGER LPNUT,GZPNUT
      REAL ET_MISS,ET_MISS_SCALAR,PHI_MISS,PNUT(2)
C
      INTEGER LCACL,LCASH
      INTEGER IETA_HOT(5),IPHI_HOT(5),LAYER_HOT(5)
      REAL EDPTH(5),PDPTH(5),ENERGY_HOT(5)
C
C----------------------------------------------------------------------
C
      GOOD_EVENT=.FALSE.
      CALL DHDIR('WZ_RCP','ZEE_DIRECTORY',IER,' ') ! Create/Set HBOOK directory
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('WZ','ANLZEE',' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      IF(FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('WZ_RCP')
        CALL EZGET('Z_E_ET_CUT',EM_ET_CUT,IER)
        IF(IER.EQ.0)CALL EZGET_l('Z_E_WRITE_Z',WRITE_Z,IER)
        IF(IER.EQ.0)CALL EZGET_l('Z_E_DO_ONE_LOOSE_ELECTRON',
     &    DO_ONE_LOOSE_ELECTRON,IER)
        IF(IER.EQ.0)CALL EZGET_l('Z_E_DO_LOOSE_ELECTRONS',
     &    DO_LOOSE_ELECTRONS,IER)
        IF(IER.EQ.0)CALL EZGET_i('Z_E_CLEANEM_MASK',CLEANEM_MASK,IER)
        IF(IER.EQ.0)CALL EZGET_i('Z_E_CLEANEM_MASK_LOOSE',
     &    CLEANEM_MASK_LOOSE,IER)
        IF(IER.EQ.0)CALL EZGET_l('Z_E_FLAG_EVENT',FLAG_EVENT,IER)
        IF(IER.NE.0)CALL ERRMSG('RCP','ANLZEE',
     &    'error getting RCP parameters','W') 
        CALL EZRSET
C
C... the following are histograms of distributions before cut in the sequence
C    they are performed
        CALL HBOOK1(10,'missing E?T!',100,0.,100.,0.)
        CALL HBOOK1(14,'largest PELC E?T!',100,0.000001,100.,0.)
        CALL HBOOK1(15,'second largest PELC E?T!',100,0.000001,100.,0.)
        CALL HBOOK1(16,'largest PPHO E?T!',100,0.000001,100.,0.)
        CALL HBOOK1(17,'second largest PPHO E?T!',100,0.000001,100.,0.)
        CALL HBOOK2(18,'1^st! vs 2^nd! tight electron E?T!',100,
     &    0.000001,100.,100,0.000001,100.,0.)
        CALL HBOOK2(19,'1^st! tight vs 1^st! loose electron E?T!',100,
     &    0.000001,100.,100,0.000001,100.,0.)
        CALL HBOOK2(20,'1^st! vs 2^nd! loose electron E?T!',100,
     &    0.000001,100.,100,0.000001,100.,0.)
C
        CALL HBOOK1(50,' MASS ALL ',60,0.,240.,0.)
        CALL HBOOK1(51,' MASS ALL ',80,70.,110.,0.)
C
        CALL HBOOK1(100,' MASS EE ',60,0.,240.,0.)
        CALL HBOOK1(101,' MASS EE ',80,70.,110.,0.)
        CALL HBOOK1(102,' PT Z EE ',250,0.,250.,0.)
        CALL HBOOK1(103,' Missing E?T! EE ',60,0.,120.,0.)
        CALL HBOOK1(104,' E?T! of tight electrons',60,0.,120.,0.)
C
        CALL HBOOK1(140,' MASS Ee ',60,0.,240.,0.)
        CALL HBOOK1(141,' MASS Ee ',80,70.,110.,0.)
        CALL HBOOK1(142,' PT Z Ee ',250,0.,250.,0.)
        CALL HBOOK1(143,' Missing E?T! Ee ',60,0.,120.,0.)
        CALL HBOOK1(144,' E?T! of tight electrons',60,0.,120.,0.)
        CALL HBOOK1(145,' E?T! of loose electrons',60,0.,120.,0.)
C
        CALL HBOOK1(150,' MASS ee ',60,0.,240.,0.)
        CALL HBOOK1(151,' MASS ee ',80,70.,110.,0.)
        CALL HBOOK1(152,' PT Z ee ',250,0.,250.,0.)
        CALL HBOOK1(153,' Missing E?T! ee ',60,0.,120.,0.)
        CALL HBOOK1(154,' E?T! of loose electrons',60,0.,120.,0.)
C
        CALL HBOOK1(200,'E?T! of tight electrons',60,0.,120.,0.)
        CALL HBOOK1(202,'E?T! of loose electrons',60,0.,120.,0.)
C
        CALL HBOOK1(300,'CC MASS Z(EE)',60,0.,240.,0.)
        CALL HBOOK1(301,'CE MASS Z(EE)',60,0.,240.,0.)
        CALL HBOOK1(302,'EE MASS Z(EE)',60,0.,240.,0.)
        CALL HBOOK1(303,'CC MASS Z(EE)',20,70.,110.,0.)
        CALL HBOOK1(304,'CE MASS Z(EE)',20,70.,110.,0.)
        CALL HBOOK1(305,'EE MASS Z(EE)',20,70.,110.,0.)
C
        CALL HBOOK1(310,'CC MASS Z(Ee)',60,0.,240.,0.)
        CALL HBOOK1(311,'CE MASS Z(Ee)',60,0.,240.,0.)
        CALL HBOOK1(312,'EE MASS Z(Ee)',60,0.,240.,0.)
        CALL HBOOK1(313,'CC MASS Z(Ee)',20,70.,110.,0.)
        CALL HBOOK1(314,'CE MASS Z(Ee)',20,70.,110.,0.)
        CALL HBOOK1(315,'EE MASS Z(Ee)',20,70.,110.,0.)
C
      ENDIF
C
      CALL FLGSET('WRITE_STREAM_ZEE',.FALSE.)       ! ZEE stream
      IF(FLAG_EVENT)IQ(LHEAD+30)=IAND(IQ(LHEAD+30),z'E3FFFFFF')
C
C ****  MISSING ET...
C
      RUN=RUNNO()
      LPNUT = GZPNUT(2) 
      ET_MISS = Q(LPNUT+7)
      PHI_MISS = Q(LPNUT+10)
      ET_MISS_SCALAR = Q(LPNUT+14)
      PNUT(1) = ET_MISS*COS(PHI_MISS)
      PNUT(2) = ET_MISS*SIN(PHI_MISS)
      CALL HFILL(10,ET_MISS,0.,1.)
C
C ****  electrons...
C
      NEL = 0
      NEM = 0
      TIGHT_ETMAX=0.
      LOOSE_ETMAX=0.
      PELC_ETMAX=0.
      PPHO_ETMAX=0.
      TIGHT_ETMAX2=0.
      LOOSE_ETMAX2=0.
      PELC_ETMAX2=0.
      PPHO_ETMAX2=0.
      LPELC = GZPELC()
      DO WHILE(LPELC.GT.0)
        EM_ET = Q(LPELC+7)
        IF(EM_ET.GT.PELC_ETMAX)THEN
          PELC_ETMAX2=PELC_ETMAX
          PELC_ETMAX=EM_ET
        ELSEIF(EM_ET.GT.PELC_ETMAX2)THEN
          PELC_ETMAX2=EM_ET
        ENDIF
        CALL CLEANEM(LPELC,1,OK,STATUS)
        OK = IAND(STATUS,CLEANEM_MASK).EQ.0 
        IF(OK) THEN    ! if it passes tight electron mask
          IF(EM_ET.GT.TIGHT_ETMAX)THEN
            TIGHT_ETMAX2=TIGHT_ETMAX
            TIGHT_ETMAX=EM_ET
          ELSEIF(EM_ET.GT.TIGHT_ETMAX2)THEN
            TIGHT_ETMAX2=EM_ET
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
            CALL HFILL(200,EL_E(5,NEL),0.,1.)
          ENDIF
        ELSE
          CALL CLEANEM(LPELC,1,OK,STATUS)
          OK = IAND(STATUS,CLEANEM_MASK_LOOSE).EQ.0 
          IF(OK) THEN  ! if it passes loose electron mask
            IF(EM_ET.GT.LOOSE_ETMAX)THEN
              LOOSE_ETMAX2=LOOSE_ETMAX
              LOOSE_ETMAX=EM_ET
            ELSEIF(EM_ET.GT.LOOSE_ETMAX2)THEN
              LOOSE_ETMAX2=EM_ET
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
              EM_TYP(NEM)  = 1
              CALL HFILL(202,EM_E(5,NEM),0.,1.)
            ENDIF
          ENDIF
        ENDIF
        LPELC = LQ(LPELC)
      ENDDO
C
      LPPHO = GZPPHO()
      DO WHILE(LPPHO.GT.0)
        EM_ET = Q(LPPHO+7)
        IF(EM_ET.GT.PPHO_ETMAX)THEN
          PPHO_ETMAX2=PPHO_ETMAX
          PPHO_ETMAX=EM_ET
        ELSEIF(EM_ET.GT.PPHO_ETMAX2)THEN
          PPHO_ETMAX2=EM_ET
        ENDIF
        CALL CLEANEM(LPPHO,0,OK,STATUS)
        OK = IAND(STATUS,CLEANEM_MASK_LOOSE).EQ.0 
        LCACL = LQ(LPPHO-2)
        LCASH = LQ(LCACL-2)
        IF(OK) THEN   ! if it passes loose electron mask
          IF(EM_ET.GT.LOOSE_ETMAX)THEN
            LOOSE_ETMAX2=LOOSE_ETMAX
            LOOSE_ETMAX=EM_ET
          ELSEIF(EM_ET.GT.LOOSE_ETMAX2)THEN
            LOOSE_ETMAX2=EM_ET
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
            CALL CEMDPTH(LCASH,IETA_HOT,IPHI_HOT,EDPTH,PDPTH,ENERGY_HOT)
            EM_IETA(NEM) = IETA_HOT(3)
            EM_TYP(NEM)  = 2
            CALL HFILL(202,EM_E(5,NEM),0.,1.)
          ENDIF
        ENDIF
        LPPHO = LQ(LPPHO)
      ENDDO
      CALL HFILL(14,PELC_ETMAX,0.,1.)
      CALL HFILL(15,PELC_ETMAX2,0.,1.)
      CALL HFILL(16,PPHO_ETMAX,0.,1.)
      CALL HFILL(17,PPHO_ETMAX2,0.,1.)
      CALL HFILL(18,TIGHT_ETMAX,TIGHT_ETMAX2,1.)
      IF(TIGHT_ETMAX2.LE.EM_ET_CUT)THEN
        CALL HFILL(19,TIGHT_ETMAX,LOOSE_ETMAX,1.)
      ENDIF
      IF(TIGHT_ETMAX.LE.EM_ET_CUT)THEN
        CALL HFILL(20,LOOSE_ETMAX,LOOSE_ETMAX2,1.)
      ENDIF
C
C ****  loop over two tight electrons
C
      DO J=1,NEL-1
        DO K=J+1,NEL
          GOOD_EVENT=.TRUE.
          PEM = 0.0
          DO I = 1, 3
            PEM =  PEM + ( EL_E(I,J)+EL_E(I,K) )**2
          END DO
          PEM = SQRT (PEM)
          EEM = EL_E(4,J) + EL_E(4,K)
          MEM = SQRT (ABS( (EEM+PEM)*(EEM-PEM) ) )
          PT_Z=SQRT((EL_E(1,J) + EL_E(1,K))**2
     &                     +(EL_E(2,J) + EL_E(2,K))**2 +1.E-09)
          IF(FLAG_EVENT)IQ(LHEAD+30)=IOR(IQ(LHEAD+30),z'10000000') ! flag in event_head bank
          CALL HFILL(50,MEM,0.0,1.0)
          CALL HFILL(51,MEM,0.0,1.0)
          CALL HFILL(100,MEM,0.0,1.0)
          CALL HFILL(101,MEM,0.0,1.0)
          CALL HFILL(102,PT_Z,0.0,1.0)
          CALL HFILL(103,ET_MISS,0.0,1.0)
          CALL HFILL(104,EL_E(5,J),0.0,1.0)
          CALL HFILL(104,EL_E(5,K),0.0,1.0)
          IF(ABS(EL_IETA(J)).LT.13) THEN   !Central electron
            IF(ABS(EL_IETA(K)).LT.13) THEN  !CC
              CALL HFILL(300,MEM,0.,1.)
              CALL HFILL(303,MEM,0.,1.)
            ELSE                            !CE
              CALL HFILL(301,MEM,0.,1.)
              CALL HFILL(304,MEM,0.,1.)
            ENDIF
          ELSE                              !End electron
            IF(ABS(EL_IETA(K)).LT.13) THEN  !EC
              CALL HFILL(301,MEM,0.,1.)
              CALL HFILL(304,MEM,0.,1.)
            ELSE                            !EE
              CALL HFILL(302,MEM,0.,1.)
              CALL HFILL(305,MEM,0.,1.)
            ENDIF
          ENDIF
        END DO
      END DO
C
C ****  Loop over combinations of one tight + one loose electron
C
      IF(DO_ONE_LOOSE_ELECTRON)THEN         ! if one loose electron allowed
        DO J = 1,NEL
          DO K= 1,NEM
            GOOD_EVENT=.TRUE.
            PEM = 0.0
            DO I = 1, 3
              PEM =  PEM + ( EL_E(I,J)+EM_E(I,K) )**2
            END DO
            PEM = SQRT (PEM)
            EEM = EL_E(4,J) + EM_E(4,K)
            MEM = SQRT (ABS( (EEM+PEM)*(EEM-PEM) ) )
            PT_Z=SQRT((EL_E(1,J) + EM_E(1,K))**2
     &                     +(EL_E(2,J) + EM_E(2,K))**2 +1.E-09)
            CALL HFILL(50,MEM,0.0,1.0)
            CALL HFILL(51,MEM,0.0,1.0)
            CALL HFILL(140,MEM,0.0,1.0)
            CALL HFILL(141,MEM,0.0,1.0)
            CALL HFILL(142,PT_Z,0.0,1.0)
            CALL HFILL(143,ET_MISS,0.0,1.0)
            CALL HFILL(144,EL_E(5,J),0.0,1.0)
            CALL HFILL(145,EM_E(5,K),0.0,1.0)
            IF(FLAG_EVENT)IQ(LHEAD+30)=IOR(IQ(LHEAD+30),z'8000000') ! flag in event_head bank
            IF(ABS(EL_IETA(J)).LT.13) THEN   !Central electron
              IF(ABS(EM_IETA(K)).LT.13) THEN
                CALL HFILL(310,MEM,0.,1.)
                CALL HFILL(313,MEM,0.,1.)
              ELSE
                CALL HFILL(311,MEM,0.,1.)
                CALL HFILL(314,MEM,0.,1.)
              ENDIF
            ELSE
              IF(ABS(EM_IETA(K)).LT.13) THEN
                CALL HFILL(311,MEM,0.,1.)
                CALL HFILL(314,MEM,0.,1.)
              ELSE
                CALL HFILL(312,MEM,0.,1.)
                CALL HFILL(315,MEM,0.,1.)
              ENDIF
            ENDIF
          END DO
        END DO
      ENDIF
C
C ****  Loop over loose electrons
C
      IF(DO_LOOSE_ELECTRONS)THEN    ! if two loose electrons allowed
        DO J = 1,NEM-1
          DO K=J+1,NEM
            GOOD_EVENT=.TRUE.
            PEM = 0.0
            DO I = 1, 3
              PEM =  PEM + ( EM_E(I,J)+EM_E(I,K) )**2
            END DO
            PEM = SQRT (PEM)
            EEM = EM_E(4,J) + EM_E(4,K)
            MEM = SQRT (ABS( (EEM+PEM)*(EEM-PEM) ) )
            PT_Z=SQRT((EM_E(1,J) + EM_E(1,K))**2
     &                     +(EM_E(2,J) + EM_E(2,K))**2 +1.E-09)
            IF(FLAG_EVENT)IQ(LHEAD+30)=IOR(IQ(LHEAD+30),z'4000000') ! flag in event_head bank
            CALL HFILL(50,MEM,0.0,1.0)
            CALL HFILL(51,MEM,0.0,1.0)
            CALL HFILL(150,MEM,0.0,1.0)
            CALL HFILL(151,MEM,0.0,1.0)
            CALL HFILL(152,PT_Z,0.0,1.0)
            CALL HFILL(153,ET_MISS,0.0,1.0)
            CALL HFILL(154,EM_E(5,J),0.0,1.0)
            CALL HFILL(154,EM_E(5,K),0.0,1.0)
          END DO
        END DO
      ENDIF
C
      IF(GOOD_EVENT)THEN
        NZEE=NZEE+1
        IF(WRITE_Z) CALL FLGSET('WRITE_STREAM_ZEE',.TRUE.)
      ENDIF
C        
  999 RETURN
      END
