      SUBROUTINE PJ_EKS(LPJHDI,ETCUT,DR_EKS,NPJ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Apply the Ellis/Kunszt/Soper (EKS) merging
C-                          to PJETS found by PJCONE or PJPART
C-
C-   Inputs  : LPJHDI, ETCUT, DR_EKS
C-   Outputs : NPJ
C-   Controls: None
C-
C-             LPJHDI - PJET header PJET banks hang from
C-             ETCUT  - Transverse Energy cut ( minimum for defining a JET ).
C-             DR_EKS - dR = SQRT(dETA**2+dPHI**2) EKS merging cut
C-             NPJ    - No. of Parton Jets found.
C-
C-   Created  13-NOV-1992   Brent J. May
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:LKPJET.INC'
      INTEGER LPJHDI, LPJETI, GZISAQ
C PJET VARIABLES
      INTEGER NPMAX
      PARAMETER (NPMAX=100)
      REAL    PJ_PX(NPMAX), PJ_PY(NPMAX), PJ_PZ(NPMAX), PJ_E(NPMAX)
      REAL    PJ_MS(NPMAX), PJ_PHI(NPMAX), PJ_THE(NPMAX)
      REAL    PJ_ETA(NPMAX), PJ_P(NPMAX), PJ_PT(NPMAX), PJ_ET(NPMAX)
      INTEGER PJ_ID(NPMAX)        ! PARTON LINKS TO ISAQ
      INTEGER PJ_NP(NPMAX)        ! NUMBER OF PARTONS IN PARTON JET
      REAL    EPS, ETCUT
      PARAMETER( EPS = 1.0E-5 )
      INTEGER PJ_LNK(NPMAX,NPMAX) ! LINK TO PARTONS IN PARTON JET
C EKS VARIABLES
      INTEGER NPJ, NPT, NJNEW, EKS_JT(NPMAX,NPMAX)
      INTEGER II, JJ, IJ, J1, J2, JET1, JET2, JETM
      INTEGER JET_SORT(NPMAX), JET_ORD(NPMAX), EKS_NJ(NPMAX)
      REAL DR_EKS, DR_J, ET_J, ETA_J, PHI_J, D_PHI, ET_SORT(NPMAX)
      LOGICAL MERGED
C----------------------------------------------------------------------
      MERGED = .FALSE.
C
      LPJHD = LPJHDI           !PJET HEADER BANK
      IF (LPJHD.EQ.0) THEN
        CALL ERRMSG ( 'PJETS', 'PJ_EKS',
     &    'No PJHD bank available to get jets', 'W')
        GOTO 999
      ENDIF
C
C Get PJET bank hanging off of PJHD
C
      LPJETI = LQ(LPJHD-1)
      LPJET = LPJETI
      IF (LPJET.EQ.0) GOTO 999    ! No PJETs found
C
C Read in PJET and fill arrays
C
      NPJ = 0
      DO WHILE (LPJET.NE.0)
        NPJ = NPJ + 1
        PJ_ET(NPJ)  = Q(LPJET+2)
        PJ_PX(NPJ)  = Q(LPJET+3)
        PJ_PY(NPJ)  = Q(LPJET+4)
        PJ_PZ(NPJ)  = Q(LPJET+5)
        PJ_E(NPJ)   = Q(LPJET+6)
        PJ_MS(NPJ)  = Q(LPJET+7)
        PJ_PHI(NPJ) = Q(LPJET+8)
        PJ_THE(NPJ) = Q(LPJET+9)
        PJ_ETA(NPJ) = Q(LPJET+10)
C       GET PJPT (POINTERS TO PARTONS/PARTICLES IN PJET) INFO
        LPJPT = LQ(LPJET-1)
        NPT = IQ(LPJPT-3)-1
        DO II = 1, NPT
          PJ_LNK(II,NPJ) = LQ(LPJPT-II-1)
        ENDDO
        PJ_NP(NPJ) = NPT
        LPJET = LQ(LPJET)
      ENDDO
      IF (NPJ.LE.1) GOTO 999    ! Need at least 2 jets to merge
C
C Order Jets by ET
C
      DO JJ = 1 , NPJ
        JET_SORT(JJ) = JJ
        ET_SORT(JJ) = PJ_ET(JJ)
      ENDDO
      CALL ISASRT(ET_SORT,NPJ,JET_SORT)   ! Ascending order
      DO JJ = 1 , NPJ
        JET_ORD(JJ) = JET_SORT(NPJ-JJ+1)  ! Descending order
      ENDDO
C
C Loop over jet pairs to find which satisfy eks merging
C
      DO J1 = 1, NPJ-1             ! loop down from largest et jet
        JET1 = JET_ORD(J1)         ! ordered index to PJ_ arrays
        IF (JET1.EQ.-1) GOTO 275   ! skip jet, already merged into another
        EKS_NJ(JET1) = 1           ! no. of jets associated with JET1
        EKS_JT(1,JET1) = JET1      ! link JET1 with itself
        DO J2 = J1+1, NPJ          ! loop over lower et jets
          JET2 = JET_ORD(J2)       ! ordered index to PJ_ arrays
          IF (JET2.EQ.-1) GOTO 250 ! skip jet, already merged into another
          EKS_NJ(JET1) = EKS_NJ(JET1) + 1   ! link JET2 to JET1 as
          EKS_JT(EKS_NJ(JET1),JET1) = JET2  ! candidate to be merged
          ET_J = 0                            ! et of candidate new jet
          ETA_J = 0
          PHI_J = 0
          DO II = 1, EKS_NJ(JET1)       ! loop over jets linked to JET1
            JETM = EKS_JT(II,JET1)      ! ordered index to PJ_ arrays
            ET_J = ET_J + PJ_ET(JETM)   ! sum et of all candidates
            ETA_J = ETA_J + PJ_ETA(JETM)*PJ_ET(JETM)
            PHI_J = PHI_J + PJ_PHI(JETM)*PJ_ET(JETM)
          ENDDO
          IF (ET_J.LT.ETCUT) GOTO 250  ! et of new composite must be > etcut
          ETA_J = ETA_J/ET_J  ! eta center of composite jet
          PHI_J = PHI_J/ET_J  ! phi center of composite jet
C         Check that each jet is within DR_EKS of jet center
          DO II = 1, EKS_NJ(JET1)  ! loop over jets linked to JET1
            JETM = EKS_JT(II,JET1)
            D_PHI = ABS(PJ_PHI(JETM)-PHI_J)
            IF (D_PHI.GT.PI) D_PHI = TWOPI - D_PHI
            DR_J = SQRT(D_PHI**2+(PJ_ETA(JETM)-ETA_J)**2)
            IF (DR_J.GE.DR_EKS) THEN   ! not all jets fit in cone
              EKS_NJ(JET1) = EKS_NJ(JET1) - 1  ! remove jet2 from jet1 list
              GOTO 250                         ! get next jet
            ENDIF
          ENDDO
C         Merge smaller jet (JET2) into larger jet (JET1)
C         Connect partons in smaller jet to larger one
          MERGED = .TRUE.
          DO JJ = 1, PJ_NP(JET2)   ! attach pointers of JET2 to JET1
            PJ_LNK(PJ_NP(JET1)+JJ,JET1)=PJ_LNK(JJ,JET2)
          ENDDO
C         Add JET1 and JET2 4-momentum and determine true center
          PJ_NP(JET1)  = PJ_NP(JET1) + PJ_NP(JET2)
          PJ_PX(JET1)  = PJ_PX(JET1) + PJ_PX(JET2)
          PJ_PY(JET1)  = PJ_PY(JET1) + PJ_PY(JET2)
          PJ_PZ(JET1)  = PJ_PZ(JET1) + PJ_PZ(JET2)
          PJ_E(JET1)   = PJ_E(JET1) + PJ_E(JET2)
          PJ_ET(JET1)  = PJ_ET(JET1) + PJ_ET(JET2)
          PJ_PT(JET1)  = SQRT( PJ_PX(JET1)**2 + PJ_PY(JET1)**2 )
          PJ_P(JET1)   = SQRT( PJ_PT(JET1)**2 + PJ_PZ(JET1)**2 )
          PJ_MS(JET1)  = SQRT( PJ_E(JET1)**2 - PJ_P(JET1)**2 )
          PJ_PHI(JET1) = ATAN2( PJ_PY(JET1),PJ_PX(JET1)+EPS)
          IF(PJ_PHI(JET1).LT.0.)PJ_PHI(JET1)=PJ_PHI(JET1)+TWOPI
          PJ_THE(JET1) = ATAN2( PJ_PT(JET1),PJ_PZ(JET1)+EPS)
          PJ_ETA(JET1) = -ALOG( TAN(PJ_THE(JET1)/2.) + EPS )
          JET_ORD(J2) = -1  ! flag JET2 as used (merged into larger jet)
  250     CONTINUE
        ENDDO
  275   CONTINUE
      ENDDO
C
C Find final list of jets: delete jets with JET_ORD=-1 and PJ_ET<ETCUT
C
      IF (.NOT.MERGED) GOTO 999
      JJ = 0
      NJNEW = 0                ! number of new jets < NPJ
      DO WHILE (JJ.LT.NPJ)     ! loop over all PJETs
        NJNEW = NJNEW + 1
        JJ = JJ + 1
        JETM=JET_ORD(JJ)       ! ordered index to PJ_ arrays
        IF (JET_ORD(NJNEW).EQ.-1 .OR. PJ_ET(JETM).LT.ETCUT) THEN
          JET_ORD(NJNEW) = JET_ORD(NJNEW+1)   ! shift up list to delete
          NJNEW = NJNEW - 1                   ! not a good jet
        ENDIF
      ENDDO
      NPJ = NJNEW    ! new number of jets returned to PJETFL for PJHD
C
C Drop all pjets hanging off of PJHD
C
      CALL MZDROP(IXCOM,LPJETI,'L')
C
C Rebook and fill PJET bank with new jets
C
      DO JJ = 1, NPJ
        IJ = JET_ORD(JJ)
        CALL BKPJET(LPJHD,LPJET)
        Q(LPJET+2) = PJ_ET(IJ)
        Q(LPJET+3) = PJ_PX(IJ)
        Q(LPJET+4) = PJ_PY(IJ)
        Q(LPJET+5) = PJ_PZ(IJ)
        Q(LPJET+6) = PJ_E(IJ)
        Q(LPJET+7) = PJ_MS(IJ)
        Q(LPJET+8) = PJ_PHI(IJ)
        Q(LPJET+9)  = PJ_THE(IJ)
        Q(LPJET+10) = PJ_ETA(IJ)
        CALL BKPJPT(LPJET,PJ_NP(IJ),LPJPT)  ! Book new PJPT bank
        DO II = 1, PJ_NP(IJ)
          LQ(LPJPT-II-1) = PJ_LNK(II,IJ)    ! Fill PJPT with new ISAQ links
        ENDDO
        IF (PJ_NP(IJ).GT.1) THEN            ! Fill split/merged flag
          IQ(LPJET+13) = 1                  !  jet was merged
        ELSE
          IQ(LPJET+13) = 0                  !  jet was not merged
        ENDIF
      ENDDO
C
  999 RETURN
      END
