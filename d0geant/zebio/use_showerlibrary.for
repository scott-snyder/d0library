      SUBROUTINE USE_SHOWERLIBRARY
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : LOOP THROUGH ISP1 TRACKS AND DEPOSIT EM
C-   AND HADRONS IN CALORIMETER. PUT MUONS ON GEANT STACK
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  14-JUN-1990   Rajendran Raja
C-   Updated  21-JAN-1993   K. Wyatt Merritt  Remove zeroing of calorimeter
C-                                            sums - done properly in TEVCAL 
C-   Updated  25-JAN-1993   K. Wyatt Merritt  First try at keeping electron
C-                                            tracks on the stack also (they
C-                                            will be stopped in STPCAL) 
C-   Modified 19-Apr-1993   Herbert Greenlee
C-      Added force-decay code
C-   Updated   3-MAY-1993   W. Dharmaratna   Changes to find the closes
C-                                          electron track from SHLB
CC-
C----------------------------------------------------------------------
      IMPLICIT NONE
c
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:SHLCON.INC'
      INCLUDE 'D0$INC:SHLDAT.INC'
      INCLUDE 'D0$INC:SHSORT.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:D0LOG.INC'
      INCLUDE 'D0$INC:DCALOG.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:GCKINE.INC'
      INCLUDE 'D0$INC:GCTRAK.INC'
      INCLUDE 'D0$INC:GCKING.INC'
C     INCLUDE 'D0$INC:HCAL.INC'
      INCLUDE 'D0$PARAMS:ISAJET_CODES.DEF'
C
      INCLUDE 'D0$LINKS:IZISP1.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISV2.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISP2.LINK/LIST'
      INTEGER LISAE,LISV1,LISP1,LJET,LISV2,LISP2
      EQUIVALENCE (CSTLNK(31),LISAE),(CSTLNK(32),LISV1)
      EQUIVALENCE (CSTLNK(33),LISP1),(CSTLNK(34),LJET)
      EQUIVALENCE (CSTLNK(35),LISV2),(CSTLNK(36),LISP2)
      INCLUDE 'D0$INC:SHLITR.INC'
C
      INTEGER GZISAE,GZISV1
      INTEGER ISPART
C
      INTEGER IER
      LOGICAL FIRST,FRSTVX
      DATA FIRST/.TRUE./
      INTEGER JTRA,JTRA1
      INTEGER JQ
      INTEGER CYC,CYC_MAX
      REAL    RNDM,ELECTRON_LOOP_ECUT
C
      REAL    P4(4)
      INTEGER RKEY(NKEY)
C
      INTEGER I,J,K,L
      REAL    PT_MUON,PT_MUON_FULL_GEANT
      REAL    PT_ELEC,PT_ELEC_FULL_GEANT
      INTEGER NVTXG
      INTEGER PRD_KEY,INDX_KEY
      LOGICAL MOVE_TRACK_TO_SHOWER
C-
C- Force-decay variables.
C-
      CHARACTER*20 CPART                ! NAPART is hollerith in /GCKINE/, but
      EQUIVALENCE (CPART,NAPART)        ! CHARACTER in GFPART
      INTEGER UB(10), NWB               ! Dummy user array
      INTEGER DECAY_TYPE                ! Force decay flag:
                                        !   0 = No decays.
                                        !   1 = Unbiased, unit weight
                                        !   2 = Biased, weighted
      LOGICAL DECAY
      INTEGER MAX_FORCE                 ! Number of particles to force-decay
      REAL MIN_FORCE_PT                 ! Minimum PT of particles to
                                        ! force-decay
      REAL MIN_FORCE_P                  ! Minimum scalar momentum of particles
                                        ! to force-decay
      REAL MAX_FORCE_ETA                ! Maximum eta of particles
                                        ! to force-decay
      INTEGER NTRY
      INTEGER NFORCE                    ! Number of particles to force-decay
      INTEGER DECAY_SRT(NSRTMX)         ! List of particles eligible for decay
      INTEGER NDECAY                    ! The number of particles that are
      INTEGER FORCE_SRT(NSRTMX)         ! List of particles to force-decay
      INTEGER FORCE_FLAG(NSRTMX)        ! Force decay flag:
                                        !   0 - Not eligible for decay.
                                        !   1 - Particle is eligible for 
                                        !       decay (biased or unbiased 
                                        !       mode).
                                        !   2 - Decay with probability 1
                                        !       (biased mode only).
      INTEGER PT_GCAH
      REAL VTX_ORG(3)                   ! Vertex of parent particle
      REAL VTX_ENT(3)                   ! Entry pt. into cal. (sh. lib.)
      REAL VTX_R
      INTEGER IETAC_SHLB                ! Eta index of showerlib. track
      REAL PATHLEN                      ! Distance from vertex to interaction
                                        !   point in calorimeter
      REAL DECAY_DIST                   ! Mean decay distance
      REAL DECAYLEN                     ! Actual decay distance.
      REAL DECAY_PROB                   ! Total decay probablity.
      REAL ZERO_PROB                    ! Probability to have zero decays.
      REAL VTX_DECAY(3)                 ! Decay vertex
      REAL PPT                          ! Parent particle transverse mom.
      REAL PMOM                         ! Parent particle momentum
      REAL GPT                          ! Daughter transverse mom.
      REAL WEIGHT                       ! Event weight (biased mode).
      LOGICAL TEMP_LINK
      INTEGER KBINOM                    ! CERNLIB binomial coefficient
C- Inverse hyperbolic sine statement function
      REAL ASINH, X
      ASINH(X) = SIGN(1.,X)*ALOG(ABS(X) + SQRT(X**2+1.))
C----------------------------------------------------------------------
C
      CALL DROP_CAD                     ! DROP CAD BANKS IF THEY EXIST.
      FRSTVX = .TRUE.
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('SHOWERLIBRARY_RCP')
        CALL EZGET('PT_MUON_FULL_GEANT',PT_MUON_FULL_GEANT,IER)
        CALL EZGET('PT_ELEC_FULL_GEANT',PT_ELEC_FULL_GEANT,IER)
        CALL EZGET_l('MOVE_TRACK_TO_SHOWER',MOVE_TRACK_TO_SHOWER,IER)
        CALL EZGET_i('DECAY_TYPE', DECAY_TYPE, IER)
        IF(FORCE_SHLB)THEN
          DECAY_TYPE = 2
        ENDIF
        CALL EZGET_i('NFORCE', MAX_FORCE, IER)
        CALL EZGET('MIN_FORCE_PT', MIN_FORCE_PT, IER)
        CALL EZGET('MIN_FORCE_P', MIN_FORCE_P, IER)
        CALL EZGET('MAX_FORCE_ETA', MAX_FORCE_ETA, IER)
        CALL ERRMAX ('SHOWERLIBRARY',-1,9999)
        CALL EZGET('ELECTRON_LOOP_ECUT',ELECTRON_LOOP_ECUT,IER)
        CALL EZRSET
C
      ENDIF
C
      TEMP_LINK = MOVE_TRACK_TO_SHOWER .OR. DECAY_TYPE.GT.0
      IF (TEMP_LINK) THEN
        CALL MZLINT(IXCOM,'/SHLITR/',TRACK_LINK(-1),
     &    TRACK_LINK(NSRTMX),TRACK_LINK(-1)) ! init temp structural link area
      ENDIF
      CALL GTWGHT(WEIGHT)                    ! Event weight from forced decays.
      KEY(1) = 1
      KEY(2) = 1
      KEY(3) = 1
      KEY(4) = 1
      KEY(5) = 1
C
      LISAE=GZISAE()                  ! GET LINK
C
      LISV1=GZISV1()
      JTRA=0                              ! number of tracks.
      NSRT = 0                          ! SORT COUNTER
      DO WHILE (LISV1 .NE. 0)
        CALL UCOPY(Q(LISV1+7),VERTEX,3)
        CALL ISAJET_GEANT_VTX(LISV1,NVTXG,FRSTVX)       ! store vtx
        FRSTVX = .FALSE.
        LISP1=LQ(LISV1-IZISP1)          ! NO GZISP1!!
        JTRA1 = 0
        DO WHILE (LISP1 .NE. 0)
C
C GET PRIMARY KEY HERE
C
          CALL UCOPY(Q(LISP1+2),P4,4)
          ISPART = IQ(LISP1+1)         ! PARTICLE ID
C- Put high PT muons on the GEANT stack
          IF(IABS(ISPART).EQ.14)THEN     ! MUON
            PT_MUON = SQRT(Q(LISP1+2)**2 + Q(LISP1+3)**2)
            IF(PT_MUON.GT.PT_MUON_FULL_GEANT)THEN
              JTRA=JTRA + 1                 ! COUNT TRACKS
              JTRA1 = JTRA1 + 1             ! TRACK NUMBER AT THIS VERTEX.
              CALL ISAJET_GEANT_TRACK(LISP1,NVTXG,JTRA,JTRA1)
              IF ( DTRK.NE.2 .AND. PD0.GT.0 )THEN
                CALL ERRMSG('SHOWERLIBRARY','USE_SHOWERLIBRARY',
     &            'MUON PUT ON GEANT STACK ','W')
              ENDIF
              GO TO 190
            ENDIF
          ENDIF
C- Put high PT electrons on the GEANT stack
          IF (IABS(ISPART) .EQ. 12) THEN     ! ELECTRON
            PT_ELEC = SQRT(Q(LISP1+2)**2 + Q(LISP1+3)**2)
            IF(PT_ELEC.GT.PT_ELEC_FULL_GEANT)THEN
              JTRA=JTRA + 1                 ! COUNT TRACKS
              JTRA1 = JTRA1 + 1             ! TRACK NUMBER AT THIS VERTEX.
C          PUT ELECTRON ON GEANT STACK
              CALL ISAJET_GEANT_TRACK(LISP1,NVTXG,JTRA,JTRA1)
              IF ( DTRK.NE.2 .AND. PD0.GT.0 )THEN
                CALL ERRMSG('SHOWERLIBRARY','USE_SHOWERLIBRARY',
     &            'ELECTRON PUT ON GEANT STACK ','W')
              ENDIF
            ENDIF
          ENDIF
          CALL GETBIN_NOCD(ISPART,VERTEX,P4)     ! GET PRIMARY KEY
          PRD_KEY = 1                  ! INIT
          DO I = 1 , NKEY
            PRD_KEY = PRD_KEY*KEY(I)
          ENDDO
          IF(PRD_KEY.EQ.0)GO TO 190   ! ONE OF THE KEYS = 0
C
  321     CYC_MAX = CYCLES(INDX_KEY(KEY))
C
          IF ( CYC_MAX.EQ.0 ) THEN
            IF(KEY(3).GT.1)THEN
              KEY(3) = KEY(3) - 1
              IF ( DTRK.NE.2 .AND. PD0.GT.0 )THEN
                WRITE(LOUT,*)
     &            ' NO CYCLES FOR THIS KEY.LOOKING AT LOWER MOMENTUM ',
     &            KEY
              ENDIF
              GO TO 321
            ELSE
              CALL ERRMSG('SHOWERLIBRARY','USE_SHOWERLIBRARY',
     &            'NO NON ZERO KEYS WITH LOWER MOMENTUM FOUND','W')
              GO TO 190
            ENDIF
          ELSE
            CYC = CYC_MAX*RNDM(0) + 1     ! RANDOM CYCLE
C
            CALL UCOPY_i(KEY,RKEY,NKEY)
C
          ENDIF
C
C ****  Find the closest shower
C
          IF(IABS(ISPART).EQ.12)THEN     ! ELECTRON
            IF(P4(4).GT.ELECTRON_LOOP_ECUT)THEN
              CALL SHLB_ELEC_MATCH_KEY(LISP1,KEY,CYC_MAX,CYC,IER)
              IF(IER.NE.0) CALL ERRMSG('SHOWERLIBRARY'
     &         ,'USE_SHOWERLIBRARY','SHLB_ELEC_MATCH_KEY FAILED','W')
            ENDIF
          ENDIF      
C
          NSRT = NSRT + 1
          IF(NSRT.GT.NSRTMX)THEN
            CALL ERRMSG('SHOWERLIBRARY','USE_SHOWERLIBRARY',
     &        'TOO MANY TRACKS TO BE SORTED ','W')
          ELSE
C
          ENDIF
C
          CALL UCOPY_i(RKEY,RKEY_SRT(1,NSRT),NKEY)
          CYC_SRT(NSRT) = CYC
          IETAC_SRT(NSRT) = IETAC_PRIMARY
          IPHIC_SRT(NSRT) = IPHIC_PRIMARY
          CALL UCOPY(P4,P4_SRT(1,NSRT),4)
          ITRA_SRT(NSRT) = JTRA
          IMAP(NSRT) = NSRT             ! INITIAL MAP
          FORCE_FLAG(NSRT) = 0
          IF (TEMP_LINK) TRACK_LINK(NSRT) = LISP1
C
          CALL PACK_REC(RKEY,CYC,KEYS_SRT(NSRT))        ! PACK KEY AWAY
C
  190     CONTINUE      ! jump to here if track be skipped...
C        -- going to next linear link (isp1)...
          LISP1=LQ(LISP1)
        ENDDO
C     -- going to next isv1 linear link...
        LISV1=LQ(LISV1)
      ENDDO
C
C ****  NOW TO SORT ON KEYS AND DO RZVIN.
C
      IF ( DTRK.NE.2 .AND. PD0.GT.0 )
     &  CALL GTIMER('USE_SHOWERLIBRARY: BEFORE SORTING')
C
      CALL SRTINT(KEYS_SRT,NSRT,IMAP)
C-
C- Flag particles that are eligible for decay.  No particles are eligible
C- in no-decay mode (DECAY_TYPE = 0).  All charged pions, charged kaons and
C- K-longs are eligible in unbiased mode (DECAY_TYPE = 1).  Additional 
C- kinematic cuts are applied in biased mode (DECAY_TYPE = 2).
C-
      NDECAY = 0
      IF(DECAY_TYPE.GT.0)THEN
        DO K = 1, NSRT
C- Cut on particle type.  Make charged pions, charged kaons and K-longs
C- eligible for decay.
          LISP1 = TRACK_LINK(K)
          ISPART = IQ(LISP1+1)
          IF(    IABS(ISPART) .NE. PION
     &      .AND.IABS(ISPART) .NE. KAON
     &      .AND.     ISPART  .NE. KLONG)GO TO 200
C- Biased mode kinematic cuts.
          IF(DECAY_TYPE.GT.1)THEN
C- Cut on PT of particles to force-decay.
            PPT = SQRT(Q(LISP1+2)**2 + Q(LISP1+3)**2)
            IF(PPT.LT.MIN_FORCE_PT)GO TO 200
C- Cut on momentum of particles to force-decay.
            PMOM = SQRT(PPT**2 + Q(LISP1+4)**2)
            IF(PMOM.LT.MIN_FORCE_P)GO TO 200
C- Cut on eta of particles to force-decay.
            IF(ABS(Q(LISP1+9)).GT.MAX_FORCE_ETA)GO TO 200
          ENDIF
          NDECAY = NDECAY + 1
          DECAY_SRT(NDECAY) = K
          FORCE_FLAG(K) = 1
  200     CONTINUE
        END DO
      ENDIF
C-
C- In biased mode, generate a list of particles to force-decay.  Randomly
C- select up to MAX_FORCE particles from eligible particles.  The actual
C- number of particles to be decayed is stored in NFORCE.
C-                      
      NFORCE = 0
      IF(DECAY_TYPE.GT.1)THEN
        NTRY = 0
        DO WHILE (NFORCE.LT.MAX_FORCE .AND. NTRY.LT.NSRTMX)
          I = NFORCE + 1
          NTRY = NTRY + 1
          K = NDECAY * RNDM(0) + 1
          IF(K.LT.1 .OR. K.GT.NDECAY)GO TO 300
          L = DECAY_SRT(K)
C- Don't force-decay the same particle twice.
          DO J = 1,I-1
            IF(FORCE_SRT(J).EQ.L)GO TO 300
          END DO
          FORCE_SRT(I) = L
          FORCE_FLAG(L) = 2
          NFORCE = I
  300     CONTINUE
        END DO
      ENDIF
C
      ZERO_PROB = 1.
      DO I = 1 , NSRT
        CALL RZVIN1(SHLB,NSHLB,NDATA,RKEY_SRT(1,IMAP(I)),
     &    CYC_SRT(IMAP(I)),' ')
        IF (IQUEST(1).NE.0.OR.IQUEST(6).NE.CYC_SRT(IMAP(I)))THEN
          CALL ERRMSG('SHOWERLIBRARY','USE_SHOWERLIBRARY',
     &        'ERROR DURING RZVIN1','W')
          WRITE(LOUT,605)(RKEY_SRT(K,IMAP(I)),K=1,NKEY),
     &      CYC_SRT(IMAP(I))
  605     FORMAT(' *******  Error during RZ READ ******',(I8))
          DO 606 JQ=1,10
            WRITE(LOUT,607)IQUEST(JQ)
  606     CONTINUE
  607     FORMAT(I8)
        ELSE
          IETAC_PRIMARY = IETAC_SRT(IMAP(I))
          IPHIC_PRIMARY = IPHIC_SRT(IMAP(I))
C- Decide whether to decay this particle.
          IF(FORCE_FLAG(IMAP(I)).GT.0)THEN
            LISP1 = TRACK_LINK(IMAP(I))
            LISV1 = LQ(LISP1+1)
C- Load the common block /GCKINE/ with the parameters of the parent particle.
            IVERT = IQ(LISV1-5)
            ISPART = IQ(LISP1+1)
            CALL ISAGEA(ISPART,IPART)
            CALL UCOPY(VTX_ORG, VERT, 3)
            CALL UCOPY(Q(LISP1+2), PVERT, 4)
            CALL GFPART(IPART,CPART,ITRTYP,AMASS,CHARGE,TLIFE,UB,NWB)
C- Load /GCTRAK/
            PMOM = SQRT(PVERT(1)**2 + PVERT(2)**2 + PVERT(3)**2)
            CALL UCOPY(VERT, VECT, 3)
            VECT(4) = PVERT(1)/PMOM
            VECT(5) = PVERT(2)/PMOM
            VECT(6) = PVERT(3)/PMOM
            VECT(7) = PMOM
            GETOT = PVERT(4)
            GEKIN = GETOT - AMASS
C- Calculate the entry point of the showerlibrary track into the calorimater
C- taking into account phi rotation and a possible z reflection of the
C- showerlibrary track.
            PT_GCAH = SHLB(2)
            CALL UCOPY(SHLB(PT_GCAH+1), VTX_ENT, 3)
            VTX_R = SQRT(VTX_ENT(1)**2 + VTX_ENT(2)**2)
            PPT = SQRT(PVERT(1)**2 + PVERT(2)**2)
            VTX_ENT(1) = VTX_R * PVERT(1)/PPT
            VTX_ENT(2) = VTX_R * PVERT(2)/PPT
            IETAC_SHLB = SHLB(10)
            IF(IETAC_PRIMARY * IETAC_SHLB .LT. 0)THEN
              VTX_ENT(3) = -VTX_ENT(3)
            ENDIF
C- Calculate the mean decay length and total decay probablility for this
C- particle. 
            CALL UCOPY(Q(LISV1+7), VTX_ORG, 3)
            PATHLEN = SQRT((VTX_ORG(1) - VTX_ENT(1))**2
     &                   + (VTX_ORG(2) - VTX_ENT(2))**2
     &                   + (VTX_ORG(3) - VTX_ENT(3))**2)
     &                   + SHLB(PT_GCAH+11)
            DECAY_DIST = TLIFE * 2.9979E10 * GETOT / AMASS
            DECAY_PROB = 1. - EXP(-PATHLEN/DECAY_DIST)
C- Decide if this particle actually decays.  For biased mode, also calculate
C- the contribution of this particle to the event weight.
            IF(DECAY_TYPE.EQ.1)THEN
              DECAY = RNDM(0) .LT. DECAY_PROB
            ELSE
              DECAY = FORCE_FLAG(IMAP(I)).GT.1
              ZERO_PROB = ZERO_PROB * (1.-DECAY_PROB)
              IF(DECAY)WEIGHT = WEIGHT * DECAY_PROB / (1.-DECAY_PROB)
            ENDIF
          ELSE
            DECAY = .FALSE.
          ENDIF
C- Here we use the showerlibrary shower if the particle did not decay, or
C- we decay the particle and put the decay products on the Geant stack.
          IF(.NOT.DECAY)THEN
            CALL USE_SHLB(P4_SRT(1,IMAP(I)),ITRA_SRT(IMAP(I)))
            IF (MOVE_TRACK_TO_SHOWER) THEN
              CALL SHLB_MOVE_TRACK(P4_SRT(1,IMAP(I)),
     &          TRACK_LINK(IMAP(I)))
            ENDIF
          ELSE
C- Decay the parent particle by calling GDECAY.  Decay products appear in 
C- the common /GCKING/.
            CALL GDECAY
C- Calculate a random actual decay length using a truncated exponential
C- distribution.
            DECAYLEN = -DECAY_DIST * 
     &        ALOG(1.-RNDM(0)*(1.-EXP(-PATHLEN/DECAY_DIST)))
C- Calculate the decay vertex.
            DO J = 1,3
              VTX_DECAY(J) = VTX_ORG(J) + DECAYLEN * PVERT(J)/PMOM
            ENDDO
C- Book and fill an ISV2 bank for the decayed parent particle.
            CALL BKISV2(LISV2)
            LQ(LISP1-4) = LISV2
            LQ(LISV2-3) = LISP1
            LQ(LISV2-2) = LISV1
            IQ(LISV2+1) = ISPART
            CALL UCOPY(PVERT, Q(LISV2+2), 3)
            Q(LISV2+5) = PMOM
            Q(LISV2+6) = AMASS
            CALL UCOPY(VTX_DECAY, Q(LISV2+7), 3)
            IQ(LISV2+10) = 11
C- Put the decay vertex on the Geant stack.
            CALL ISAJET_GEANT_VTX(LISV2, NVTXG, .FALSE.)
C- Make an ISP2 bank for each decay product and put it on the Geant stack.
            JTRA1 = 0
            DO J = 1,NGKINE
              IPART = GKIN(5,J)
              CALL GFPART(IPART,CPART,ITRTYP,AMASS,CHARGE,TLIFE,UB,NWB)
              CALL BKISP2(LISV2, LISP2)
              CALL GEAISA(IPART, IQ(LISP2+1))
              CALL UCOPY(GKIN(1,J), Q(LISP2+2), 4)
              Q(LISP2+6) = AMASS
              Q(LISP2+7) = ATAN2(GKIN(2,J), GKIN(1,J))
              GPT = SQRT(GKIN(1,J)**2+GKIN(2,J)**2)
              Q(LISP2+8) = ATAN2(GPT, GKIN(3,J))
              Q(LISP2+9) = ASINH(GKIN(3,J)/GPT)
              JTRA = JTRA + 1
              JTRA1 = JTRA1 + 1
              CALL ISAJET_GEANT_TRACK(LISP2, NVTXG, JTRA, JTRA1)
            ENDDO
          ENDIF
        ENDIF
      ENDDO
C-
C- Add the overall normalization contribution to the event weight and put
C- the result in a WGHT bank.
C-
      IF(DECAY_TYPE.GT.1)THEN
        WEIGHT = WEIGHT * KBINOM(NDECAY, NFORCE) * ZERO_PROB
        CALL WGHTFL(WEIGHT)
      ENDIF
C
C ****  INITIALIZE JETS BANKS
C
      IF ( (SCAL(1).EQ.1.) .AND. (SCAL(4).NE.0.) ) CALL JETINI
C
C ****  Store energy into CAEP bank and zero E_WRK for further use
C
      CALL CALBLD(0)
      IF (TEMP_LINK) TRACK_LINK(-1) = 0 ! deactivate temp link area
      IF ( DTRK.NE.2 .AND. PD0.GT.0 )
     &  CALL GTIMER('USE_SHOWERLIBRARY: END OF SHOWER LIBRARY ACCESS')
  999 CONTINUE
      RETURN
      END
