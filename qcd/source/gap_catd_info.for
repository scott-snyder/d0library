       SUBROUTINE GAP_CATD_INFO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-
C-   Created   23-JAN-1994   BJM - extract CATD info between leading jets
C-                           AGB and BJM - originally CATD_ANAL
C-   Modified  20-MAY-1994   AGB if thresh between 100 and 200 make
C-                           all plots for this thresh--allows 150 cut
C-                           to work for MSTA's
C-   Modified  06-JUL-1994   BJM Fix 3jet bug
C-   Modified  28-OCT-1994   Added comments
C-   Modified  07-NOV-1994   AGB Update for CW
C-   Modified  08-NOV-1994   Add MINB switch
C-   Modified  22-FEB-1995   TLT Use UNPACK_CATD
C-   Modified  12-APR-1995   TLT Move thresh check so that no plots are
C-                           made if not above threshold (i.e. thresh 150)
C-   Modified  10-MAY-1995   AGB Use QCD_UNPACK_CATD
C-   Updated   14-JUL-1995   BH  fix 0 tower problem
C-   Updated   24-JUL-1995    BH  replace packed arrays
C-   Modified  02-SEP-1995   AGB add NEM in ETA slices
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:LKCAPH.INC'
C
      INCLUDE 'D0$INC:QCD_EVT_INFO.INC/LIST'
      INCLUDE 'D0$INC:QCD_JET.INC/LIST'
      INCLUDE 'D0$INC:QCD_NTUP_INFO.INC/LIST'
      INCLUDE 'D0$INC:QCD_JUTL_HEAD.INC/LIST'
C
      INCLUDE 'D0$INC:GAP_CATD_INFO.INC/LIST'   ! CATD info
C
      INTEGER NMAX
      PARAMETER(NMAX = 5000)
C
      INTEGER NEM_BET,NHD_BET
      REAL ETHD, ETEM

      INTEGER I, J, L
C
      INTEGER  LCATD, GZCATD
      REAL ETA(NMAX),PHI(NMAX),E(4,NMAX), DELETA(NMAX)
      INTEGER  IPHI(NMAX), IETA(NMAX)
      INTEGER N_EM, N_HAD

      LOGICAL GAP_CHECK_HOT_CELL, HOT

      REAL  ZTEMP
      REAL PETEM
      REAL DRL, DRH, DRJ, DRJ1, DRJ2, PHIA, DRLA, DRHA
      LOGICAL IN_JET, JET_IN_GAP
      INTEGER IEB,IES
      INTEGER NEMWDS, NHDWDS, NMUTWR
      INTEGER IPNTEM,IPNTHD,IPNTMU,ETMNEM,ETMNHD,EMNMUO
      INTEGER IETA5,IBIN
C----------------------------------------------------------------------
C
C Make Sure CATD+HEAD banks exist
C
      LCATD = GZCATD()
      IF(LCATD.LE.0) THEN
        CALL ERRMSG('GAP_CATD_INFO','GAP_CATD_INFO',
     &              ' NO CATD bank','W')
        GO TO 998
      END IF
C
C Determine detector IETA of edge of jets (need ZOFF from QCD_UPK_JETS)
C
      IF (TRK_NV.LE.0.OR.TRK_Z(1).LT.-900.) THEN
        ZTEMP=ZOFF(IRUN)
      ELSE
        ZTEMP = TRK_Z(1)
      ENDIF
      IF (NJETS.GE.2) THEN
        CALL GAP_CAL_GAP(ETAJ(1),ETAJ(2),GAP_CONE_RADIUS,IEL,IEH,
     &                   ZTEMP)
      ELSE
        IEL = 0
        IEH = 0
      ENDIF
C
C Keep track of cells between beam and jet
C
      IEB=IEH+20*GAP_CONE_RADIUS
      IES=IEL-20*GAP_CONE_RADIUS
C
      IF(DO_EM_DELTA_R) THEN
        DO I=1,NEM_DELTA_R_DR
           CALL GAP_CAL_GAP(ETAJ(1),ETAJ(2),EM_DELTA_R_DR(I),
     &          IELDR(I),IEHDR(I),ZTEMP)
        ENDDO
      ENDIF
C
C Get CATD pointers and initialzie CATD variables
C
      CALL VZERO(NEM_CATD,NEMMAX)
      CALL VZERO(NHD_CATD,NHDMAX)
      CALL VZERO(NEM_CONE,NEMCMAX)
      CALL VZERO(NEM_ACONE,NEMCMAX)
      CALL VZERO(NEM_DELTA_R,NEMCMAX)
      CALL VZERO(NEM_IETA,NEMIMAX)
      CALL VZERO(NEM_IETAG,NEMIMAX)
      CALL VZERO(NEM_JET,NEMCMAX)
      CALL VZERO(NEM_JALL,NEMCMAX)
      CALL VZERO(NEM_EVT,6)
      CALL VZERO(NEM_ETABIN,NETAMAX)
      NPKEM    = 0
      NPKEMJ   = 0
      NPKEMB   = 0
      NEM_RAW  = 0
      NHD_RAW  = 0
C
      HOT = .FALSE.
      NEM_BET=0
      NHD_BET=0
      NBIN=NETAMAX
C
C ****  Get EM tower info from CATD:
C
      CALL GTCATD(LCATD,IPNTEM,IPNTHD,IPNTMU,NEMWDS,NHDWDS,
     &                         NMUTWR,ETMNEM,ETMNHD,EMNMUO)
      IF (NEMWDS.GT.0) THEN
        CALL QCD_UNPACK_CATD(1,NEMWDS,N_EM,IETA,IPHI,ETA,PHI,DELETA,E)
      ELSE
        N_EM = 0
      ENDIF
C
C Loop over CATD EM towers
C
      DO L = 1, N_EM
C
C Use new ZVERTE to get vertex from JUTL and get ET right
C

        ETEM = SQRT(E(1,L)**2+E(2,L)**2)
C
C Allow possibility of all plots being calculated at threshold higher
C than default threshold, for case where thresh=100, but we want plots
C for 150 for example
C
          IF(EM_ET_THRESHOLDS(1).GT.0.1.AND.EM_ET_THRESHOLDS(1).LT.0.2)
     +    THEN
            IF(ETEM.LT.EM_ET_THRESHOLDS(1)) GO TO 90
          END IF
C
        NEM_EVT(1)=NEM_EVT(1)+1
C
C Determine if tower/cell is hot (ILYR=-1 is EM tower)
C
        HOT = GAP_CHECK_HOT_CELL(IETA(L),IPHI(L),-1,RUNNUM)
C
        IF(.NOT.HOT) NEM_EVT(2)=NEM_EVT(2)+1
C
C Get NEM vs. IETA (regardless of leading jets)
C
        IF(.NOT.HOT.AND.DO_EM_IETA_GAP) THEN
          DO I = 1, NEM_IETA_GAPS
            IF (ABS(IETA(L)).LE.EM_IETA_GAPS(I)) THEN
              NEM_IETA(I) = NEM_IETA(I) + 1
            ENDIF
          ENDDO
        ENDIF
C
C Get NEM in ETA slices
C
        IF(.NOT.HOT.AND.DO_EM_ETABIN) THEN
          IF(IETA(L).LT.0) THEN
            IETA5=(IETA(L)+1)/5
            IBIN=IETA5+8
          ELSE
            IETA5=(IETA(L)-1)/5
            IBIN=IETA5+9
          END IF
          IF(IBIN.LE.0.OR.IBIN.GT.16) THEN
            CALL ERRMSG('GAP_CATD_INFO','GAP_CATD_INFO',
     &        'IETA out of RANGE','F')
          END IF
          NEM_ETABIN(IBIN)=NEM_ETABIN(IBIN)+1
        END IF
C
C IF MINB set then don't care about jets
C Save packed info for <=2 in EM_CATD_LIST  and >2 in EM_BEAM_LIST
C
        IF(.NOT.HOT.AND.MINB) THEN
          IF (ABS(IETA(L)).LE.MBIETA) THEN
C
C Get Packed EM CATD ET,IETA,IPHI (don't use bit 32)
C
            IF (DO_PACKED_EM.AND.NPKEM.LT.NUM_PACKED_EM) THEN
              NPKEM=NPKEM+1
              EM_BTWN_ETA(NPKEM) = IETA(L)
              EM_BTWN_PHI(NPKEM) = IPHI(L)
              PETEM = MAX(0.0,MIN(ETEM,TOWER_MAX_ET))
              EM_BTWN_ET(NPKEM) = PETEM
            ENDIF
          ELSE
            IF (DO_PACKED_EMB.AND.NPKEMB.LT.NUM_PACKED_EMB) THEN
              NPKEMB=NPKEMB+1
              EM_BEAM_ETA(NPKEMB) = IETA(L)
              EM_BEAM_PHI(NPKEMB) = IPHI(L)
              PETEM = MAX(0.0,MIN(ETEM,TOWER_MAX_ET))
              EM_BEAM_ET(NPKEMB) = PETEM
            ENDIF
          END IF
          GO TO 90
        ENDIF
C
C Look Between leading jets
C
        IF(IETA(L).LT.IEH.AND.IETA(L).GT.IEL) THEN
C
C Get NEM for all towers
C
          NEM_RAW = NEM_RAW + 1
          IF (HOT) GOTO 90
          NEM_BET=NEM_BET+1
C
C Get NEM vs. Et threshold
C
          IF (DO_EM_CATD) THEN
            DO J = 1, NEM_ET
              IF (J.EQ.1) THEN
                 NEM_CATD(1) = NEM_CATD(1) + 1 ! no cut for 200 MeV
              ELSEIF (ETEM.GT.EM_ET_THRESHOLDS(J)) THEN
                NEM_CATD(J) = NEM_CATD(J) + 1
              ENDIF
            ENDDO
          ENDIF
C
C Get Packed EM CATD ET,IETA,IPHI (don't use bit 32)
C
          IF (DO_PACKED_EM.AND.NPKEM.LT.NUM_PACKED_EM) THEN
            NPKEM=NPKEM+1
            EM_BTWN_ETA(NPKEM) = IETA(L)
            EM_BTWN_PHI(NPKEM) = IPHI(L)
            PETEM = MAX(0.0,MIN(ETEM,TOWER_MAX_ET))
            EM_BTWN_ET(NPKEM) = PETEM
          ENDIF
C
C Get NEM for different cone sizes DELTA_ETA_C = j1eta-j2eta-2R
C
          IF(DO_EM_DELTA_R) THEN
            DO I = 1,NEM_DELTA_R_DR
              IF(IETA(L).LT.IEHDR(I).AND.IETA(L).GT.IELDR(I)) THEN
                NEM_DELTA_R(I) = NEM_DELTA_R(I) + 1
              ENDIF
            ENDDO
          ENDIF
C
C Get NEM outside cones in gap and outide cones 180 degrees away
C
          IF(DO_EM_CONE) THEN
            DRL =  SQRT( (ETA(L)-ETAJ(IL))**2+
     &             ACOS(COS(PHI(L)-PHIJ(IL)))**2 )
            DRH =  SQRT( (ETA(L)-ETAJ(IH))**2+
     &             ACOS(COS(PHI(L)-PHIJ(IH)))**2 )
            PHIA = PHI(L) + 3.1415926
            DRLA = SQRT((ETA(L)-ETAJ(IL))**2+
     &             ACOS(COS(PHIA-PHIJ(IL)))**2)
            DRHA = SQRT((ETA(L)-ETAJ(IH))**2+
     &             ACOS(COS(PHIA-PHIJ(IH)))**2)
            DO I = 1, NEM_CONE_DR
              IF (DRL.GT.EM_CONE_DR(I).AND.DRH.GT.EM_CONE_DR(I)) THEN
                NEM_CONE(I) = NEM_CONE(I) + 1
              ENDIF
              IF (DRLA.GT.EM_CONE_DR(I).AND.DRHA.GT.EM_CONE_DR(I)) THEN
                NEM_ACONE(I) = NEM_ACONE(I) + 1
              ENDIF
            ENDDO
          ENDIF
C
C Get NEM vs. IETA in gap
C
          IF(DO_EM_IETA_IN_GAP) THEN
            DO I = 1, NEM_IETA_GAPS
              IF (ABS(IETA(L)).LE.EM_IETA_GAPS(I)) THEN
                NEM_IETAG(I) = NEM_IETAG(I) + 1
              ENDIF
            ENDDO
          ENDIF
C
C Get NEM with jets in gap removing all jets from gap
C
          IF(DO_REMOVE_JETS_IN_GAP.AND.NJETS.GT.2) THEN
            DO J = 1, NEM_REMOVE_DR
              IN_JET = .FALSE.
              DO I = 3, NJETS
                IF (ETJ(I).GT.REMOVE_JET_ET.AND.
     &            ETAJ(I).GT.ETAJ(IL).AND.ETAJ(I).LT.ETAJ(IH)) THEN
                  DRJ=SQRT((ETA(L)-ETAJ(I))**2+
     &                ACOS(COS(PHI(L)-PHIJ(I)))**2)
                  IN_JET = IN_JET .OR. (DRJ.LE.REMOVE_JET_RADIUS(J))
                ENDIF
              ENDDO
              IF (.NOT.IN_JET) NEM_JALL(J) = NEM_JALL(J) + 1
            ENDDO
C
C Get NEM with >= 1jet in gap removing first jet from gap
C
            DO I = 3, NJETS
              IF (ETJ(I).GT.REMOVE_JET_ET.AND.
     &            ETAJ(I).GT.ETAJ(IL).AND.ETAJ(I).LT.ETAJ(IH)) THEN
                DRJ = SQRT((ETA(L)-ETAJ(I))**2+
     &                ACOS(COS(PHI(L)-PHIJ(I)))**2)
                DO J = 1, NEM_REMOVE_DR
                  IF (DRJ.GT.REMOVE_JET_RADIUS(J)) THEN
                    NEM_JET(J) = NEM_JET(J) + 1
                  ENDIF
                ENDDO
               GOTO 85    !remove only first jet
              ENDIF
            ENDDO
  85        CONTINUE
          ENDIF  !DO_REMOVE_JETS_IN_GAP
C
C Look at cells not between jets
C
        ELSE
          IF (HOT) GOTO 90
C
C cells in jets
C
          DRJ1=SQRT((ETA(L)-ETAJ(1))**2+ACOS(COS(PHI(L)-PHIJ(1)))**2)
          IF(DRJ1.LT.GAP_CONE_RADIUS) NEM_EVT(3)=NEM_EVT(3)+1
          DRJ2=SQRT((ETA(L)-ETAJ(2))**2+ACOS(COS(PHI(L)-PHIJ(2)))**2)
          IF(DRJ2.LT.GAP_CONE_RADIUS) NEM_EVT(4)=NEM_EVT(4)+1
C
C cells between jets and beam
C
          IF(IETA(L).LT.IES) NEM_EVT(5)=NEM_EVT(5)+1
          IF(IETA(L).GT.IEB) NEM_EVT(6)=NEM_EVT(6)+1
C
C save packed info
C
          IF(IETA(L).GT.IEB.OR.IETA(L).LT.IES) THEN
            IF (DO_PACKED_EMB.AND.NPKEMB.LT.NUM_PACKED_EMB) THEN
              NPKEMB=NPKEMB+1
              EM_BEAM_ETA(NPKEMB) = IETA(L)
              EM_BEAM_PHI(NPKEMB) = IPHI(L)
              PETEM = MAX(0.0,MIN(ETEM,TOWER_MAX_ET))
              EM_BEAM_ET(NPKEMB) = PETEM
            ENDIF
          ELSE
C
C If it's not between jets or between jets and beam save it as NPKEMJ
C (includes whole jet band)
C
            IF (DO_PACKED_EMJ.AND.NPKEMJ.LT.NUM_PACKED_EMJ) THEN
              NPKEMJ=NPKEMJ+1
              EM_JETS_ETA(NPKEMJ) = IETA(L)
              EM_JETS_PHI(NPKEMJ) = IPHI(L)
              PETEM = MAX(0.0,MIN(ETEM,TOWER_MAX_ET))
              EM_JETS_ET(NPKEMJ) = PETEM
            ENDIF
          ENDIF

C
        ENDIF ! IEL/IEH
C
   90 ENDDO ! (EM Towers)
C
C Check event for jet/jets in gap (set to -1 to flag events w/o jet in gap)
C
      JET_IN_GAP = .FALSE.
      IF(DO_REMOVE_JETS_IN_GAP.AND.NJETS.GT.2) THEN
        DO I = 3, NJETS
          IF (ETJ(I).GT.REMOVE_JET_ET.AND.
     &        ETAJ(I).GT.ETAJ(IL).AND.ETAJ(I).LT.ETAJ(IH)) THEN
            JET_IN_GAP = .TRUE.
          ENDIF
        ENDDO
      ENDIF
      IF (DO_REMOVE_JETS_IN_GAP.AND..NOT.JET_IN_GAP) THEN
        DO J = 1, NEM_REMOVE_DR
          NEM_JALL(J) = -1
          NEM_JET(J) = -1
        ENDDO
      ENDIF

C
C Get NEM vs. IETA between jet (if EM_IETA_GAPS(I).gt.IEL.or.IEH set to -1)
C
      IF(DO_EM_IETA_IN_GAP) THEN
        DO I = 1, NEM_IETA_GAPS
          IF (EM_IETA_GAPS(I).LT.IEL.OR.EM_IETA_GAPS(I).GT.IEH)THEN
            NEM_IETAG(I) = -1
          ENDIF
        ENDDO
      ENDIF
C
C ****  Get Hadronic tower info from CATD
C
      IF (NHDWDS.GT.0) THEN
        CALL QCD_UNPACK_CATD(2,NHDWDS,N_HAD,IETA,IPHI,ETA,PHI,DELETA,E)
      ELSE
        N_HAD = 0
      ENDIF
C
C Loop over hadronic towers
C
      DO L = 1, N_HAD
C
C       Use new ZVERTE to get vertex from JUTL and get ET right
C
        ETHD=SQRT(E(1,L)**2+E(2,L)**2)
C
C Check for hot cell in HAD+ICD+MG tower (ILYR=-17)
C
        HOT = GAP_CHECK_HOT_CELL(IETA(L),IPHI(L),-17,RUNNUM)
        IF(IETA(L).LT.IEH.AND.IETA(L).GT.IEL) THEN
          NHD_RAW = NHD_RAW + 1
          IF (HOT) GOTO 95
          NHD_BET=NHD_BET+1
C
C Get NHD vs. Et threshold
C
          IF (DO_HD_CATD) THEN
            DO J = 1, NHD_ET
              IF (HD_ET_THRESHOLDS(J).EQ.0.4) THEN
                 NHD_CATD(1) = NHD_CATD(1) + 1 ! no cut for 400 MeV
              ELSEIF (ETHD.GT.HD_ET_THRESHOLDS(J)) THEN
                NHD_CATD(J) = NHD_CATD(J) + 1
              ENDIF
            ENDDO
          ENDIF
        ENDIF
C
   95 ENDDO
C
  998 CONTINUE
C
C Write run/event
C
      IF (DO_WRITE_RUN_EVT) THEN
        CALL GAP_WRITE_RUN_EVT
      ENDIF
C
  999 RETURN
      END
