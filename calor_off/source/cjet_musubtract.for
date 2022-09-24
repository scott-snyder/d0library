      SUBROUTINE CJET_MUSUBTRACT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine subtracts the muon from its 
C-                    associated JETS bank.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  21-FEB-1993   Alex Smith
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAEP.LINK'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
C
      INTEGER   LLJETS,LPMUO,LLCAEH,LMUCA,LMUON,LLJPTS
      INTEGER   PT_MUCA, PT_CAEH, PT_JPTS
      INTEGER   NCH_MUCA,NRT_MUCA,NRT_CAEH
      INTEGER   GZPMUO,GZCAEH,IER
      INTEGER   IETA,IPHI,ILYR,I
      INTEGER   NS,JCELL,NCELL_JET
      REAL      PX,PY,PZ,IPMUON,PT,SMALL,SPX,SPY
      REAL      PATHLENGTH,PMUON,PHI,THETA
      REAL      DROP_DR,JET_ETA,JET_PHI,MU_ETA,MU_PHI,DR,DPHI
      INTEGER   NTRAKS,ITRAK
      DATA  SMALL/0.000001/

      LOGICAL   FIRST,EZERR,DROP_UNASSOC_JETS
      DATA FIRST/.TRUE./
      SAVE FIRST
      SAVE DROP_UNASSOC_JETS
      SAVE DROP_DR
C
C --- Get RCP parameters: ---------------------------------------------------
C
      IF ( FIRST )THEN                  ! LOCAL INIT
        FIRST = .FALSE.
        CALL INRCP('CAJETS_RCP',IER)
        CALL EZPICK('CAJETS_RCP')       ! SELECT JETS RCP BANK
        IF (IER.EQ.0)
     &    CALL EZGET_l('DROP_UNASSOC_JETS',DROP_UNASSOC_JETS,IER)
        IF (IER.EQ.0)
     &    CALL EZGET('DROP_DR',DROP_DR,IER)
        IF (EZERR(IER)) THEN
          CALL ERRMSG('NO_CAJETS_RCP','CJET_MUON',
     &      'CAJETS_RCP not found in CJET_MUON.','W')
        ENDIF
        CALL EZRSET
      ENDIF
C
C --- Get pointer to JETS from PMUO reference link: -------------------------
C
      NTRAKS = 0
      LPMUO=GZPMUO(0)
      DO WHILE (LPMUO.GT.0)      
        NTRAKS = NTRAKS + 1
        LPMUO = LQ(LPMUO)
      END DO
      LPMUO=GZPMUO(0)
      IF(LPMUO.LE.0) THEN        ! Abort if PMUO does not exist
        CALL ERRMSG('NO_PMUO','CJET_MUSUBTRACT',
     &    'SKIP CJET_MUSUBTRACT','W')
        GOTO 999
      ENDIF
      DO ITRAK = 1, NTRAKS
        LPMUO=GZPMUO(ITRAK)
        IF(IQ(LPMUO-3).LT. 6) THEN  ! Abort if old PMUO version
          CALL ERRMSG('OLD PMUO VERSION, RERUN MURECO',
     &      'CJET_MUSUBTRACT',
     &        'SKIP CJET_MUSUBTRACT','W')
          GOTO 999
        ENDIF
        LMUON = LQ(LPMUO - IQ(LPMUO-2) - 2)
        IF(LMUON.LE.0) THEN        ! Abort if MUON does not exist
          CALL ERRMSG('NO MUON BANK','CJET_MUSUBTRACT',
     &      'SKIP CJET_MUSUBTRACT','W')
          GOTO 999
        ENDIF
        LMUCA=LQ(LMUON-2)
        IF(LMUCA.LE.0) THEN        ! Abort if MUCA does not exist
          CALL ERRMSG('NO MUCA BANK','CJET_MUSUBTRACT',
     &      'SKIP CJET_MUSUBTRACT','W')
          GOTO 999
        ENDIF
        NS = IQ(LPMUO-2)
        LLJETS = LQ(LPMUO-NS-5)
        IF (LLJETS .EQ. 0) GO TO 999  ! no JET in cone about muon
        LLJPTS = LQ(LLJETS-1)
C
C --- loop over MUCA hits for the first time; find total path ---------------
C     length of muon in CAL.
C
        NCH_MUCA=IQ(LMUCA+3)              ! Number of cells
        NRT_MUCA=IQ(LMUCA+2)              ! MUCA repetition number
        PT_MUCA=LMUCA
        IF(NCH_MUCA.LE.0 .OR. NRT_MUCA.LE.0) GOTO 800
        DO I=1,NCH_MUCA
          PATHLENGTH = PATHLENGTH + Q(PT_MUCA+8)
          PT_MUCA=PT_MUCA+NRT_MUCA
        END DO
C
C --- Loop through MUCA a second time, see if any cells are in JET ----------
C
        NCH_MUCA=IQ(LMUCA+3)              ! Number of cells
        NRT_MUCA=IQ(LMUCA+2)              ! MUCA repetition number
        PT_MUCA=LMUCA
        PMUON = ABS(Q(LPMUO+13)) - ABS(Q(LPMUO+33)) ! P - PMIP
        NCELL_JET = IQ(LLJPTS+2)
        IF(NCH_MUCA.LE.0 .OR. NRT_MUCA.LE.0) GOTO 800
        LLCAEH = GZCAEH()
        IF(LLCAEH.LE.0) THEN        ! Abort if CAEH does not exist
          CALL ERRMSG('NO CAEH BANK','CJET_MUSUBTRACT',
     &      'SKIP CJET_MUSUBTRACT','W')
          GOTO 999
        ENDIF
        NRT_CAEH = IQ(LLCAEH+2)
        DO I=1,NCH_MUCA
          PT_JPTS = LLJPTS
          DO JCELL=1, NCELL_JET
            PT_CAEH = LLCAEH + NRT_CAEH*(IQ(PT_JPTS+3)-1)
            IETA = IQ(PT_MUCA+4)
            IPHI = IQ(PT_MUCA+5)
            ILYR = IQ(PT_MUCA+6)
C
C --- If MUCA cell is in JET, then calculate the muon contribution to that --
C     cell and subtract it from the JETS bank.
C
            IF ( IQ(PT_CAEH+12) .EQ. IETA
     &        .AND. IQ(PT_CAEH+13) .EQ. IPHI
     &        .AND. IQ(PT_CAEH+14) .EQ. ILYR ) THEN
             
              IPMUON = ( PMUON * Q(PT_MUCA+8) )/
     &          (PATHLENGTH+SMALL)
              CALL CJET_MUENERGY(IETA,IPHI,ILYR,IPMUON,
     &          PT,PX,PY,PZ,SPX,SPY)
C
C --- Correct JETS bank for this cell: --------------------------------------
C
              Q(LLJETS+2) = Q(LLJETS+2) - PX
              Q(LLJETS+3) = Q(LLJETS+3) - PY
              Q(LLJETS+4) = Q(LLJETS+4) - PZ
              Q(LLJETS+5) = Q(LLJETS+5) - IPMUON
              Q(LLJETS+6) = Q(LLJETS+6) - PT
              THETA = ABS(ATAN( (SQRT( Q(LLJETS+2)**2   
     &           + Q(LLJETS+3)**2) )/( Q(LLJETS+4)+SMALL) ))  ! THETA
C              THETA = THETA-INT(THETA/3.1416)*3.1416
C              IF (THETA.LT.0.0) THETA = THETA + 3.1416
              Q(LLJETS+7) = THETA
              PHI = ATAN( Q(LLJETS+3)/( Q(LLJETS+2)
     &          +SMALL) )                                     ! PHI
              PHI = PHI-INT(PHI/6.2830)*6.2830
              IF (PHI.LT.0.0) PHI = PHI + 6.2830
              Q(LLJETS+8) = PHI
              Q(LLJETS+9) =  - LOG( (TAN( Q(LLJETS+7))/2)+SMALL ) ! ETA
C
            END IF
            PT_JPTS = PT_JPTS + 1
          END DO
          PT_MUCA=PT_MUCA+NRT_MUCA
        END DO
C
C --- If requested, drop JETS bank if outside cone of PMUO: -----------------
C
        IF (DROP_UNASSOC_JETS) THEN
          LPMUO = GZPMUO(ITRAK)
          NS = IQ(LPMUO-2)
          LLJETS = LQ(LPMUO-NS-5)
          JET_ETA = Q(LLJETS+9)
          JET_PHI = Q(LLJETS+8)
          JET_PHI = JET_PHI-INT(JET_PHI/6.2830)*6.2830
          IF (JET_PHI.LT.0.0) JET_PHI = JET_PHI + 6.2830
          Q(LLJETS+8) = JET_PHI
          MU_ETA = Q(LPMUO+16)
          MU_PHI = Q(LPMUO+17)
          MU_PHI = MU_PHI-(INT(MU_PHI/6.2830)*6.2830)
          IF (MU_PHI.LT.0.0) MU_PHI = MU_PHI + 6.2830
          DPHI = ABS(MU_PHI - JET_PHI)
          DR = SQRT ( (MU_ETA - JET_ETA)**2 +
     &      (DPHI)**2 )
          IF (DR.LE.DROP_DR) THEN
            LPMUO = GZPMUO(ITRAK)
            NS = IQ(LPMUO-2)
            LQ(LPMUO-NS-5) = 0
            CALL MZDROP (IXMAIN,LLJETS,' ')
          END IF
        END IF
  800   CONTINUE
      END DO    ! LOOP OVER MUONS
  999 RETURN
      END
