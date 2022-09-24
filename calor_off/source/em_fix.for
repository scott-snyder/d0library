      LOGICAL FUNCTION EM_FIX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Returned value  :  TRUE always!
C-   Inputs  :
C-   Outputs :
C-   Controls:  CAPHEL.RCP, CLEANEM.RCP
C-
C-   Created  15-JUL-1995   Meenakshi Narain
c-   Modified 21-aug-1995   Doug Norman/Meenakshi Narain
c-                          Add window energies, elikelihood
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:CHMATR_NEW.INC'
      INCLUDE 'D0$INC:CEMPRF.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$LINKS:IZCAWX.LINK'
      INCLUDE 'D0$LINKS:IZCAW7.LINK'
      INCLUDE 'D0$LINKS:IZCAW5.LINK'
      INCLUDE 'D0$LINKS:IZCAW3.LINK'
      INCLUDE 'D0$LINKS:IZCAWC.LINK'
      INCLUDE 'D0$LINKS:IZCASH.LINK'
      INCLUDE 'D0$INC:CTRAK.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER GZPELC,GZPPHO
      INTEGER NUM_CELL,NCL
      LOGICAL CPHINI,OK
      LOGICAL DO_HMATRIX,WINDOW_OK,CREATE_CAWX_BANKS
      INTEGER IER,I,LCLUS,NVBST
      INTEGER LCAW7,LCAW5,LCAW3,LCAWC
      INTEGER IEVERT,KVERT
C
      INTEGER ELECTRON
      PARAMETER( ELECTRON = 1  )        ! Electromagnetic clusters
C
      REAL    ZV(14),DZ(14),ZVTX_INFO(3,14)
      REAL    PHI_CENTER,THETA,ETA,RHO
      REAL    ECLUS,ETAPHI(3),DETAPHI(3)
      REAL    ETCLUS,TQUAN(50),CQUAN(50),ELIKE,E_LKL,FHAD
      REAL    ENGY_TOT,Z_INT,ERR_Z_INT,zclus
      REAL    XBAR_EM(4,3),ZV_EM,RV_EM,PRBRZ,PRBXY,PRB
      EXTERNAL ELIKE
C
      INTEGER NVER,NV,IETA,IPHI
      INTEGER TRK,STATUS,MVAR
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      LOGICAL USE_MONTE_VERTEX
      LOGICAL ELECT,LZTRKS,USE_LOG,USE_CASH,OKZ
      LOGICAL DO_CLEANEM,DO_ELFIT
C----------------------------------------------------------------------
      EM_FIX = .TRUE.
      OK = CPHINI()                     ! Needs to be called each time
C
      IF(FIRST)THEN                     ! LOCAL INIT
        FIRST = .FALSE.
        CALL EZPICK('CAPHEL_RCP')
        CALL EZGET('WEIGHT_CUT',WEIGHT_CUT,IER)
        CALL EZGET_l('DO_HMATRIX',DO_HMATRIX,IER)
        CALL EZGET_l('DO_CLEANEM',DO_CLEANEM,IER)
        CALL EZGET_l('DO_ELFIT',DO_ELFIT,IER)
        CALL EZGET_l('DO_ZTRAKS',LZTRKS,IER)
        CALL EZGET_l('USE_LOG_WEIGHTED_CENTER',USE_LOG,IER)
        CALL EZGET_l('USE_MONTE_CARLO_VERTEX',USE_MONTE_VERTEX,IER)
        IF (IER.NE.0) THEN
          CALL ERRMSG('EM_FIX','CALORIMETER',
     &        'problem reading CAPHEL RCP parameters','F')
        ENDIF
        CALL EZRSET
        USE_CASH = .TRUE.
      ENDIF
C
      ELECT = .FALSE.

      LPELC = GZPELC()
      LPPHO = GZPPHO()
      IF (LPELC.EQ.0 .AND. LPPHO.EQ.0) THEN
        GOTO 999
      ENDIF
C
C ****  Retrieve primary vertex...
C
      IF ( USE_MONTE_VERTEX ) THEN
        CALL ZVERTX(ZV,DZ)                ! Isajet Vertex
      ELSE
        CALL VERTEX_INFO(14,NVER,ZVTX_INFO,OKZ) ! Vertex from tracking
C            Only consider the main primary vertex
        IF ( OKZ ) THEN
          IF(NVER.EQ.0) THEN
            CALL ERRMSG('No Vertices','EM_FIX','z set to 0','W')
            CALL VZERO(ZV,14)
          ELSE
            NV = MIN(14,NVER)
            DO I = 1, NV
              ZV(I) = ZVTX_INFO(1,I)
            ENDDO
          ENDIF
        ELSE
          CALL ERRMSG('Error getting Vertices',
     &      'EM_FIX','z set to 0','W')
          CALL VZERO(ZV,14)
        ENDIF
      ENDIF
C
C:::   Extend PELC/PPHO/HMTE/HMTP bank lenghts
C
      CALL EM_FIX_UPDBANKS
C
C:::   Start of cluster do loop
C
      LCLUS = GZPELC()
      TRK   = 1
      ELECT = .TRUE.
      IF (LCLUS.EQ.0) THEN
        LCLUS = GZPPHO()
        ELECT = .FALSE.
      ENDIF
      DO WHILE (LCLUS.NE.0)
        LCACL = LQ(LCLUS-2)
        LCASH = LQ(LCACL-2)
        VERT(3) = ZV(1)
        ECLUS = Q(LCACL+7)
        ETCLUS = Q(LCACL+8)
        CALL UCOPY(Q(LCACL+14),SHOWER_CENTER,3)   ! Crude shower center
C
CCC        CALL CAW7FL
C          BOOK CAWI AND CAWX BANKS AND FILL THEM, THEN RETRIEVE
C          WINDOW ENERGIES FOR BANKS AND WRITE TO PELC/PPHO
C
C
c
        CALL CEMENR(NDPTH,ENDPTH,PEDPTH,ETOT,ET,ETRANS,EMAX,
     &    ETAMX,PHIMX,USE_CASH)
        IF (LCASH.NE.0)
     &    CALL CASH_ETAPHIMX(LCASH,IETA,IPHI,IER)
C
C ****  Fix shower centorid
C
        CALL CM3POS_PV(LCASH,WEIGHT_CUT,XBAR3,DBAR3,
     &    ETAPHI,DETAPHI,ZV(1))
        CALL UCOPY(XBAR3,Q(LCLUS+23),3)   !x,y,z
        CALL UCOPY(XBAR3,Q(LCACL+14),3)   !x,y,z
        CALL UCOPY(DBAR3,Q(LCACL+20),3)   !dispersion dx,dy,dz
        CALL UCOPY(XBAR3,SHOWER_CENTER,3) !update shower center
C
C ****  update theta, phi, eta, Ex, Ey, Ez, Et in PELC/PPHO and CACL
C
        RHO   = SQRT(XBAR3(2)**2+XBAR3(1)**2)
        Theta = ATAN2(RHO,XBAR3(3)-ZV(1))
        Phi_center   = ETAPHI(2)
        Eta   = -ALOG(TAN(Q(LCACL+11)/2.0))
        Eclus = Q(LCLUS+6)
C
        Q(LCLUS+3)  = ECLUS*COS(PHI_CENTER)*SIN(THETA)  ! Ex
        Q(LCLUS+4)  = ECLUS*SIN(PHI_CENTER)*SIN(THETA)  ! Ey
        Q(LCLUS+5)  = ECLUS*COS(THETA)           ! Ez
        Q(LCLUS+7)  = ECLUS*SIN(THETA)     ! Et
        Q(LCLUS+8)  = theta
        Q(LCLUS+9)  = eta
        Q(LCLUS+10) = phi_center
C
        Q(LCACL+4)  = Q(LCLUS+3)
        Q(LCACL+5)  = Q(LCLUS+4)
        Q(LCACL+6)  = Q(LCLUS+5)
        Q(LCACL+8)  = Q(LCLUS+6)
        Q(LCACL+11) = theta
        Q(LCACL+12) = phi_center
        Q(LCACL+13) = Eta
C
C ****  get electron/photon quality
C
        IF(DO_CLEANEM)THEN
          CALL CLEANEM(LCLUS,TRK,OK,STATUS)
          IQ(LCLUS+30) = STATUS
          CALL CLEANEM_CQUANS(MVAR,CQUAN)
          Q(LCLUS+32) = CQUAN(13)   ! isolation
          Q(LCLUS+33) = CQUAN(9)   ! emfraction
          CALL CLEANEM_TQUANS(MVAR,TQUAN)
          Q(LCLUS+20)  = TQUAN(12)          ! fill track-match significance
          Q(LCLUS+45)  = TQUAN(22)          !TRD ACCEPTANCE
          Q(LCLUS+44)  = TQUAN(23)
          IF (TQUAN(13).GT.0) THEN
            Q(LCLUS+46)  = TQUAN(13)         ! IF CDC
          ELSE
            Q(LCLUS+46)  = TQUAN(14)         ! IF FDC
          ENDIF
c
          IF (TQUAN(12).GT.0.) then ! track in road for pelc/ppho
            Q(LCLUS+41)  = -1.
            Q(LCLUS+42)  = -1.
            Q(LCLUS+43)  = -1.
            IF (ABS(Q(LCLUS+19)).LE.12) THEN
              FHAD         = 0.5
C              
              CALL ELIKE_SET_MASK_CC(31)
              E_LKL =   ELIKE(LCLUS,FHAD,IER)  !ALL FIVE VARIABLES
              IF (IER.GE.0) THEN
                Q(LCLUS+41)  = E_LKL
              ENDIF
              CALL ELIKE_SET_MASK_CC(15)        !NOTRD
              E_LKL =   ELIKE(LCLUS,FHAD,IER)
              IF (IER.GE.0) THEN
                Q(LCLUS+42)  = E_LKL
              ENDIF
              CALL ELIKE_SET_MASK_CC(17)       ! TRD AND DEDX ONLY
              E_LKL =   ELIKE(LCLUS,FHAD,IER)
              IF (IER.GE.0) THEN
                Q(LCLUS+43)  = E_LKL
              ENDIF
C
            ELSE
C
              FHAD         = 0.62
              CALL ELIKE_SET_MASK_EC(15)        !NOTRD
              E_LKL =   ELIKE(LCLUS,FHAD,IER)
              IF (IER.GE.0) THEN
                Q(LCLUS+41)  = E_LKL    ! for EC 4 and 5 var are the same
                Q(LCLUS+42)  = E_LKL
              ENDIF
C
            ENDIF
          ENDIF   
c
        ENDIF       ! if cleanem
C
C ****  CALL global fit package
C
        IF(DO_ELFIT.AND.ELECT)THEN
          CALL CELGLB(LCLUS)
        ENDIF
C
c       Calculate the projected vertex based on layer centroids
C
        CALL EMVTX(LCLUS,XBAR_EM,ZV_EM,RV_EM,PrbRZ,PrbXY,Prb,NVBST)
        Q(LCLUS+34)=ZV_EM
        Q(LCLUS+35)=RV_EM
        IQ(LCLUS+36)=NVBST
C compute the id of the closest reco vertex
        kvert = ievert(lclus,z_int,err_z_int,zclus)
        IQ(LCLUS+47)= kvert
C
  300   CONTINUE                          ! GO TO NEXT CLUSTER
        LCLUS = LQ(LCLUS)
        IF (LCLUS.EQ.0..AND.ELECT) THEN
          LCLUS = GZPPHO()
          ELECT = .FALSE.
        ENDIF
  500   CONTINUE
      END DO         !CLUSTER LOOP
C
C ****  window energies
C
      WINDOW_OK=CREATE_CAWX_BANKS()  ! DOES IT FOR ALL PELCS/PPHOS
      LCLUS = GZPELC()               ! WATCH OUT FOR FALLING ZEBRAS HERE
      ELECT = .TRUE.
      IF (LCLUS.EQ.0..AND.ELECT) THEN
        LCLUS = GZPPHO()
        ELECT = .FALSE.
      ENDIF
      DO WHILE (LCLUS.GT.0)
        LCACL = LQ(LCLUS-2)
        LCASH = LQ(LCACL-IZCASH)
        LCAW7 = LQ(LCASH-IZCAW7)
        LCAW5 = LQ(LCASH-IZCAW5)
        LCAW3 = LQ(LCASH-IZCAW3)
        LCAWC = LQ(LCASH-IZCAWC)
        ENGY_TOT = 0.0
        IF (LCAWC.GT.0) THEN
          NUM_CELL = IQ(LCAWC+2)
          DO NCL = 1,NUM_CELL
            ENGY_TOT = ENGY_TOT+Q(LCAWC+2*NCL+2)
          END DO
          Q(LCLUS+37) = ENGY_TOT
        END IF
c
        ENGY_TOT = 0.0
        IF (LCAW3.GT.0) THEN
          NUM_CELL = IQ(LCAW3+2)
          DO NCL = 1,NUM_CELL
            ENGY_TOT = ENGY_TOT+Q(LCAW3+2*NCL+2)
          END DO
          Q(LCLUS+38) = ENGY_TOT
        END IF
c
        ENGY_TOT = 0.0
        IF (LCAW5.GT.0) THEN
          NUM_CELL = IQ(LCAW5+2)
          DO NCL = 1,NUM_CELL
            ENGY_TOT = ENGY_TOT+Q(LCAW5+2*NCL+2)
          END DO
          Q(LCLUS+39) = ENGY_TOT
        END IF
c
        ENGY_TOT = 0.0
        IF (LCAW7.GT.0) THEN
          NUM_CELL = IQ(LCAW7+2)
          DO NCL = 1,NUM_CELL
            ENGY_TOT = ENGY_TOT+Q(LCAW7+2*NCL+2)
          END DO
          Q(LCLUS+40) = ENGY_TOT
        END IF
c
        LCLUS = LQ(LCLUS)
        IF (LCLUS.EQ.0..AND.ELECT) THEN
          LCLUS = GZPPHO()
          ELECT = .FALSE.
        ENDIF
      END DO
c           Drop CAWC, CAW7, CAW5, CAW3 banks but keep CAWX
      LPELC=GZPELC()
      IF (LPELC.GT.0) CALL DROP_CAW(LPELC)
      LPPHO=GZPPHO()
      IF (LPPHO.GT.0) CALL DROP_CAW(LPPHO)
C
C ****  Call Caphel analysis routine
C
  900 CALL CPHANL              ! CAPHEL ANALYSIS ROUTINE
C
  999 RETURN
      END
