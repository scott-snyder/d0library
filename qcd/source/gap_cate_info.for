      SUBROUTINE GAP_CATE_INFO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Store various quantities from CATE bank for
C-                         forward rapidity gap studies
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-
C-   Created   08-NOV-1995   Andrew G. Brandt (based on GAP_CATD_INFO) by BJM
C-   Updated  10-JAN-1996   Bob Hirosky  add qms path switch
C-   Updated  24-JAN-1996   Bob Hirosky  add more slice info
C-   Updated  25-JAN-1996   Bob Hirosky  ADD NEW SLICE WORDS
C-   Updated   4-FEB-1996   Bob Hirosky  fix ad hoc tower size corrections
C-                                       use non-wgted tower Energy in
C-                                           energy sums (SECATE)
C-   Updated   9-MAR-1996   Andrew G. Brandt  rename QMS_FIX.INC to QMS for SGI
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:QCD_EVT_INFO.INC/LIST'
      INCLUDE 'D0$INC:QCD_JET.INC/LIST'
      INCLUDE 'D0$INC:QCD_NTUP_INFO.INC/LIST'
      INCLUDE 'D0$INC:QCD_JUTL_HEAD.INC/LIST'
      INCLUDE 'D0$INC:QMS.INC'
C
      INCLUDE 'D0$INC:GAP_CATD_INFO.INC/LIST'   ! CATD/CATE info
C
      INTEGER NMAX
      PARAMETER(NMAX = 5000)
C
      INTEGER NEM_BET,NHD_BET
      INTEGER I, J, L
C
      REAL ETA(NMAX),PHI(NMAX)
      INTEGER  IPHI, IETA
      INTEGER NCATE, NEM_CATE, LAYER(30), NLAYER, IER
      REAL ECATE, EN(7)
      LOGICAL GAP_CHECK_HOT_CELL, HOT

      REAL  ZTEMP
      REAL PECATE,SECATE
      REAL DRL, DRH, DRJ, DRJ1, DRJ2, PHIA, DRLA, DRHA
      LOGICAL IN_JET, JET_IN_GAP
      INTEGER IEB,IES
      INTEGER IETA5,IETA10,IBIN
C----------------------------------------------------------------------
C
      IF (QMS_DATA) THEN
        CALL PATHST('RECO') ! reset path for QMS DATA
      ENDIF
C
C Determine detector IETA of edge of jets (need ZOFF from QCD_UPK_JETS)
C
      IF (CATDE.EQ.1) THEN      !may not want this information
        IF (TRK_NV.LE.0.OR.TRK_Z(1).LT.-900.) THEN
          ZTEMP=ZOFF(IRUN)
        ELSE
          ZTEMP = TRK_Z(1)
        ENDIF
        IF (NJETS.GE.2) THEN
          CALL GAP_CAL_GAP(ETAJ(1),ETAJ(2),GAP_CONE_RADIUS,IEL,IEH,
     &                     ZTEMP)
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
     &            IELDR(I),IEHDR(I),ZTEMP)
          ENDDO
        ENDIF
      ELSE
        IEL = 0
        IEH = 0
      ENDIF   ! CATDE EQ 1 block
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
      CALL VZERO(NEMNEG_ETABIN,NETAMAX)
      CALL VZERO(NEMNEG_BINTH2,NMORMAX)
      CALL VZERO(NEMNEG_BINTH3,NMORMAX)
      CALL VZERO(NEMPOS,NPOSMAX)
      CALL VZERO(NEMNEG,NNEGMAX)
      CALL VZERO(NHDPOS,NPOSMAX)
      CALL VZERO(NHDNEG,NNEGMAX)
      CALL VZERO(NEM_BINTH2,NMORMAX)
      CALL VZERO(NEM_BINTH3,NMORMAX)
      CALL VZERO(NHD_ETABIN,NMORMAX)
      CALL VZERO(NHD_BINTH2,NMORMAX)
      CALL VZERO(NHD_BINTH3,NMORMAX)
      CALL VZERO(NHDNEG_ETABIN,NETAMAX)
      CALL VZERO(NHDNEG_BINTH2,NMORMAX)
      CALL VZERO(NHDNEG_BINTH3,NMORMAX)
      CALL VZERO(E_NEGB,NETAMAX)
      CALL VZERO(E_NEG1B,NETAMAX)
      CALL VZERO(E_POSB,NETAMAX)
      CALL VZERO(E_POS1B,NETAMAX)
      CALL VZERO(H_NEGB,NETAMAX)
      CALL VZERO(H_NEG1B,NETAMAX)
      CALL VZERO(H_POSB,NETAMAX)
      CALL VZERO(H_POS1B,NETAMAX)
      NPKEM    = 0
      NPKEMJ   = 0
      NPKEMB   = 0
      NPKNEG   = 0
      NEM_RAW  = 0
      NHD_RAW  = 0
      E_NEG = 0.0
      E_NEG1 = 0.0
      E_POS = 0.0
      E_POS1 = 0.0
      H_NEG = 0.0
      H_NEG1 = 0.0
      H_POS = 0.0
      H_POS1 = 0.0
C
      HOT = .FALSE.
      NEM_BET=0
      NHD_BET=0
      NBIN=NETAMAX
      NMORB=NMORMAX
      NNEGB=NETAMAX
      NNEGMOR=NMORMAX
      NHDBIN=NETAMAX
      NHDMOR=NMORMAX
      NHDNEGB=NETAMAX
      NHDNEGMOR=NMORMAX
C
C ****  Number of tower info from CATE:
C
      CALL GTCATE_TOTAL(NCATE,NEM_CATE,IER)
      IF(IER.NE.0) THEN
        CALL ERRMSG('GAP_CATE_INFO','GAP_CATE_INFO',
     &              ' NO CATE bank','W')
        GO TO 998
      END IF
C
C Loop over CATD EM towers
C
      DO L = 1, NEM_CATE
C
C ****  EM info from CATE:
C
        CALL GTCATE(L,IETA,IPHI,LAYER,NLAYER,EN,IER)
        ECATE=EN(4)
C
C Ad Hoc correction for different cell sizes (but save true values for packed)
C
        SECATE=ECATE
        IF(ABS(IETA).EQ.33) THEN
          ECATE=ECATE/2.2/2.0
        ELSE IF(ABS(IETA).EQ.34) THEN
          ECATE=ECATE/2.8/2.0
        ELSE IF(ABS(IETA).EQ.35) THEN
          ECATE=ECATE/4.0/2.0
        END IF
C
        NEM_EVT(1)=NEM_EVT(1)+1
C
C Count number of positive and negative energy cells
C
        DO I = 1, NEM_POS
          IF(ECATE.GT.EM_POS_THRESHOLDS(I)) NEMPOS(I)=NEMPOS(I)+1
        END DO
        DO I = 1, NEM_NEG
          IF(ECATE.LT.EM_NEG_THRESHOLDS(I)) NEMNEG(I)=NEMNEG(I)+1
        END DO
C
C
C Determine if tower/cell is hot (ILYR=-1 is EM tower)
C
CCC leave at .FALSE. for now
CCC        HOT = GAP_CHECK_HOT_CELL(IETA,IPHI,-1,RUNNUM)
C
        IF(.NOT.HOT) NEM_EVT(2)=NEM_EVT(2)+1
C
        IF(IETA.LT.0) THEN
          IETA5=(IETA+1)/5
          IBIN=IETA5+8
        ELSE
          IETA5=(IETA-1)/5
          IBIN=IETA5+9
        END IF
        IF(IBIN.LE.0.OR.IBIN.GT.16) THEN
          CALL ERRMSG('GAP_CATE_INFO','GAP_CATE_INFO',
     &          'IETA out of RANGE','F')
        END IF
C
C SAVE TOTAL positive and negative energy, in bins too!
C
        IF (SECATE.LT.0.0) THEN
          E_NEG  = E_NEG + SECATE
          E_NEGB(IBIN) = E_NEGB(IBIN) + SECATE
        ELSE
          E_POS  = E_POS + SECATE
          E_POSB(IBIN) = E_POSB(IBIN) + SECATE
        ENDIF
C
C Save packed negative energy cells
C
        IF (ECATE.LT.EM_NEG_THRESHOLDS(1)) THEN
          E_NEG1 = E_NEG1 + SECATE           ! negative energy w/ threshold
          E_NEG1B(IBIN) = E_NEG1B(IBIN) + SECATE
          IF (DO_PACKED_NEG.AND.NPKNEG.LT.NUM_PACKED_NEG) THEN
            NPKNEG=NPKNEG+1
            EM_NEG_ETA(NPKNEG) = IETA
            EM_NEG_PHI(NPKNEG) = IPHI
            PECATE = MAX(SECATE,TOWER_MIN_ET)
            EM_NEG_ET(NPKNEG) = PECATE
          ENDIF
C
C Get NEM NEG in ETA slices
C
          IF(.NOT.HOT.AND.DO_EMNEG_ETABIN) THEN
            NEMNEG_ETABIN(IBIN)=NEMNEG_ETABIN(IBIN)+1
C
C Keep eta bins for 2nd and 3rd thresholds
C
            IF(DO_EMNEG_MORBIN) THEN
              IF(ECATE.LT.EM_NEG_THRESHOLDS(2)) THEN
                NEMNEG_BINTH2(IBIN)=NEMNEG_BINTH2(IBIN)+1
              END IF
              IF(ECATE.LT.EM_NEG_THRESHOLDS(3)) THEN
                NEMNEG_BINTH3(IBIN)=NEMNEG_BINTH3(IBIN)+1
              END IF
            END IF
          END IF ! end of bin block
C
        ENDIF ! (ECATE.LT.EM_NEG_THRESHOLDS(1))
C
C Require energy above lowest positive threshold or go to next tower
C
        IF(ECATE.LE.EM_POS_THRESHOLDS(1)) GO TO 90
C
C Get NEM in ETA slices
C
        E_POS1 = E_POS1 + SECATE     ! positive energy w/ threshold
        E_POS1B(IBIN) = E_POS1B(IBIN) + SECATE
        IF(.NOT.HOT.AND.DO_EM_ETABIN) THEN
          NEM_ETABIN(IBIN)=NEM_ETABIN(IBIN)+1
C
C Keep eta bins for 2nd and 3rd thresholds
C
          IF(DO_EM_MORBIN) THEN
            IF(ECATE.GT.EM_POS_THRESHOLDS(2)) THEN
              NEM_BINTH2(IBIN)=NEM_BINTH2(IBIN)+1
            END IF
            IF(ECATE.GT.EM_POS_THRESHOLDS(3)) THEN
              NEM_BINTH3(IBIN)=NEM_BINTH3(IBIN)+1
            END IF
          END IF
        END IF  ! end of bin block
C
C Get NEM vs. IETA (regardless of leading jets)
C
        IF(.NOT.HOT.AND.DO_EM_IETA_GAP) THEN
          DO I = 1, NEM_IETA_GAPS
            IF (ABS(IETA).LE.EM_IETA_GAPS(I)) THEN
              NEM_IETA(I) = NEM_IETA(I) + 1
            ENDIF
          ENDDO
        ENDIF
C
C IF MINB set then don't care about jets
C Save packed info for <=2 in EM_CATD_LIST  and >2 in EM_BEAM_LIST
C and then bail out of cell loop (note only keep towers above POS threshold)
C
        IF(.NOT.HOT.AND.MINB) THEN
          IF (ABS(IETA).LE.MBIETA) THEN
C
C Get Packed EM CATD ET,IETA,IPHI (don't use bit 32)
C
            IF (DO_PACKED_EM.AND.NPKEM.LT.NUM_PACKED_EM) THEN
              NPKEM=NPKEM+1
              EM_BTWN_ETA(NPKEM) = IETA
              EM_BTWN_PHI(NPKEM) = IPHI
              PECATE = MAX(0.0,MIN(SECATE,TOWER_MAX_ET))
              EM_BTWN_ET(NPKEM) = PECATE
            ENDIF
          ELSE
            IF (DO_PACKED_EMB.AND.NPKEMB.LT.NUM_PACKED_EMB) THEN
              NPKEMB=NPKEMB+1
              EM_BEAM_ETA(NPKEMB) = IETA
              EM_BEAM_PHI(NPKEMB) = IPHI
              PECATE = MAX(0.0,MIN(SECATE,TOWER_MAX_ET))
              EM_BEAM_ET(NPKEMB) = PECATE
            ENDIF
          END IF
          GO TO 90
        ENDIF
C
C Bail out of loop unless CATDE=1
C
        IF (CATDE.NE.1) GO TO 90
C
C Look Between leading jets
C
        IF(IETA.LT.IEH.AND.IETA.GT.IEL) THEN
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
            DO J = 1, NEM_POS
              IF (ECATE.GT.EM_POS_THRESHOLDS(J)) THEN
                NEM_CATD(J) = NEM_CATD(J) + 1
              ENDIF
            ENDDO
          ENDIF
C
C Get Packed EM CATD ET,IETA,IPHI (don't use bit 32)
C
          IF (DO_PACKED_EM.AND.NPKEM.LT.NUM_PACKED_EM) THEN
            NPKEM=NPKEM+1
            EM_BTWN_ETA(NPKEM) = IETA
            EM_BTWN_PHI(NPKEM) = IPHI
            PECATE = MAX(0.0,MIN(SECATE,TOWER_MAX_ET))
            EM_BTWN_ET(NPKEM) = PECATE
          ENDIF
C
C Get NEM for different cone sizes DELTA_ETA_C = j1eta-j2eta-2R
C
          IF(DO_EM_DELTA_R) THEN
            DO I = 1,NEM_DELTA_R_DR
              IF(IETA.LT.IEHDR(I).AND.IETA.GT.IELDR(I)) THEN
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
              IF (DRLA.GT.EM_CONE_DR(I).AND.DRHA.GT.EM_CONE_DR(I))
     &            THEN
                NEM_ACONE(I) = NEM_ACONE(I) + 1
              ENDIF
            ENDDO
          ENDIF
C
C Get NEM vs. IETA in gap
C
          IF(DO_EM_IETA_IN_GAP) THEN
            DO I = 1, NEM_IETA_GAPS
              IF (ABS(IETA).LE.EM_IETA_GAPS(I)) THEN
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
     &              ETAJ(I).GT.ETAJ(IL).AND.ETAJ(I).LT.ETAJ(IH)) THEN
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
   85       CONTINUE
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
          IF(IETA.LT.IES) NEM_EVT(5)=NEM_EVT(5)+1
          IF(IETA.GT.IEB) NEM_EVT(6)=NEM_EVT(6)+1
C
C save packed info
C
          IF(IETA.GT.IEB.OR.IETA.LT.IES) THEN
            IF (DO_PACKED_EMB.AND.NPKEMB.LT.NUM_PACKED_EMB) THEN
              NPKEMB=NPKEMB+1
              EM_BEAM_ETA(NPKEMB) = IETA
              EM_BEAM_PHI(NPKEMB) = IPHI
              PECATE = MAX(0.0,MIN(SECATE,TOWER_MAX_ET))
              EM_BEAM_ET(NPKEMB) = PECATE
            ENDIF
          ELSE
C
C If it's not between jets or between jets and beam save it as NPKEMJ
C (includes whole jet band)
C
            IF (DO_PACKED_EMJ.AND.NPKEMJ.LT.NUM_PACKED_EMJ) THEN
              NPKEMJ=NPKEMJ+1
              EM_JETS_ETA(NPKEMJ) = IETA
              EM_JETS_PHI(NPKEMJ) = IPHI
              PECATE = MAX(0.0,MIN(SECATE,TOWER_MAX_ET))
              EM_JETS_ET(NPKEMJ) = PECATE
            ENDIF
          ENDIF

C
        ENDIF ! IEL/IEH
C
   90 ENDDO ! (EM Towers)
C
C Check event for jet/jets in gap (set to -1 to flag events w/o jet in gap)
C For CATDE=1 only
C
      IF(CATDE.EQ.1) THEN
        JET_IN_GAP = .FALSE.
        IF(DO_REMOVE_JETS_IN_GAP.AND.NJETS.GT.2) THEN
          DO I = 3, NJETS
            IF (ETJ(I).GT.REMOVE_JET_ET.AND.
     &          ETAJ(I).GT.ETAJ(IL).AND.ETAJ(I).LT.ETAJ(IH)) THEN
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
      ENDIF  ! end of post loop CATDE=1 block
C
C Loop over hadronic towers
C
      DO L = NEM_CATE+1, NCATE
C
        CALL GTCATE(L,IETA,IPHI,LAYER,NLAYER,EN,IER)
        ECATE=EN(4)
C
C Ad Hoc correction for different cell sizes (but save true values for packed)
C
        SECATE=ECATE
        IF(ABS(IETA).EQ.33) THEN
          ECATE=ECATE/2.2/2.0
        ELSE IF(ABS(IETA).EQ.34) THEN
          ECATE=ECATE/2.8/2.0
        ELSE IF(ABS(IETA).EQ.35) THEN
          ECATE=ECATE/4.0/2.0
        ELSE IF(ABS(IETA).EQ.36) THEN
          ECATE=ECATE/3.5/2.0
        ELSE IF(ABS(IETA).EQ.37) THEN
          ECATE=ECATE/5.3/2.0
        END IF
C
C Count number of positive and negative energy cells
C
        DO I = 1, NHD_POS
          IF(ECATE.GT.HD_POS_THRESHOLDS(I)) NHDPOS(I)=NHDPOS(I)+1
        END DO
        DO I = 1, NHD_NEG
          IF(ECATE.LT.HD_NEG_THRESHOLDS(I)) NHDNEG(I)=NHDNEG(I)+1
        END DO
C
C Check for hot cell in HAD+ICD+MG tower (ILYR=-17)
C
CCC        HOT = GAP_CHECK_HOT_CELL(IETA,IPHI,-17,RUNNUM)
C
        IF(IETA.LT.0) THEN
          IETA5=(IETA+1)/5
          IBIN=IETA5+8
        ELSE
          IETA5=(IETA-1)/5
          IBIN=IETA5+9
        END IF
        IF(IBIN.LE.0.OR.IBIN.GT.16) THEN
          CALL ERRMSG('GAP_CATE_INFO','GAP_CATE_INFO',
     &          'IETA out of RANGE','F')
        END IF
C
C SAVE TOTAL positive and negative energy, in bins too!
C
        IF (SECATE.LT.0.0) THEN
          H_NEG  = H_NEG + SECATE
          H_NEGB(IBIN) = H_NEGB(IBIN) + SECATE
        ELSE
          H_POS  = H_POS + SECATE
          H_POSB(IBIN) = H_POSB(IBIN) + SECATE
        ENDIF
C
C Save packed negative energy cells
C
        IF (ECATE.LT.HD_NEG_THRESHOLDS(1)) THEN
C
C Get NEM NEG in ETA slices
C
          H_NEG1 = H_NEG1 + SECATE       ! neg energy w/ threshold
          H_NEG1B(IBIN) = H_NEG1B(IBIN) + SECATE
C
          IF(.NOT.HOT.AND.DO_HDNEG_ETABIN) THEN
            NHDNEG_ETABIN(IBIN)=NHDNEG_ETABIN(IBIN)+1
C
C Keep eta bins for 2nd and 3rd thresholds
C
            IF(DO_HDNEG_MORBIN) THEN
              IF(ECATE.LT.HD_NEG_THRESHOLDS(2)) THEN
                NHDNEG_BINTH2(IBIN)=NHDNEG_BINTH2(IBIN)+1
              END IF
              IF(ECATE.LT.HD_NEG_THRESHOLDS(3)) THEN
                NHDNEG_BINTH3(IBIN)=NHDNEG_BINTH3(IBIN)+1
              END IF
            END IF
          END IF ! end of bin block
C
        ENDIF ! (ECATE.LT.HD_NEG_THRESHOLDS(1))

C
C Require energy above lowest positive threshold or go to next tower
C
        IF(ECATE.LE.HD_POS_THRESHOLDS(1)) GO TO 95
C
C Get NHD in ETA slices
C
CCC then look at slice information
CCC use CATDE also
C
        H_POS1 = H_POS1 + SECATE     ! pos energy w/ threashold
        H_POS1B(IBIN) = H_POS1B(IBIN) + SECATE
C
        IF(.NOT.HOT.AND.DO_HD_ETABIN) THEN
          NHD_ETABIN(IBIN)=NHD_ETABIN(IBIN)+1
C
C Keep eta bins for 2nd and 3rd thresholds
C
          IF(DO_HD_MORBIN) THEN
            IF(ECATE.GT.HD_POS_THRESHOLDS(2)) THEN
              NHD_BINTH2(IBIN)=NHD_BINTH2(IBIN)+1
            END IF
            IF(ECATE.GT.HD_POS_THRESHOLDS(3)) THEN
              NHD_BINTH3(IBIN)=NHD_BINTH3(IBIN)+1
            END IF
          END IF
        END IF

C
C Bail out of loop unless CATDE=1
C
        IF (CATDE.NE.1) GO TO 95
C
        IF(IETA.LT.IEH.AND.IETA.GT.IEL) THEN
          NHD_RAW = NHD_RAW + 1
          IF (HOT) GOTO 95
          NHD_BET=NHD_BET+1
C
C Get NHD vs. Et threshold
C
          IF (DO_HD_CATD) THEN
            DO J = 1, NHD_POS
              IF (ECATE.GT.HD_POS_THRESHOLDS(J)) THEN
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
  999 IF (QMS_DATA) THEN
        CALL PATHST('MDST') ! reset path for QMS DATA
      ENDIF
C
      RETURN
      END
