      LOGICAL FUNCTION VKINEMA(NPVES)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : offline tool for general least-squares
C                          fitting  of  Vees, which were found in
C                          Central Detector.
C-
C-   Inputs  :    NPVES  = Vee ID number
C-   Outputs :    HYP    - the best fit hypothesis
C                 IEND   = 1 or 2 for successful fit
C                 PRB    - the best fit probability
C-   Controls:
C-
C-   Created 10-APR-1991   V. Burtovoy
C-   Updated  14-DEC-1991   Daria Zieminska  handle errors; call PVESDR
C-   Updated  21-SEP-1993   H. Castilla, B. Gomez & O. Ramirez to include the
C-                          decay lambda --> proton + pion &                    
C-                                lambda --> pion +  proton
C-   Updated  21-NOV-1993   Daria Zieminska  require p1,P2>P1_MIN 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:VEEKIN.INC/LIST'
      INCLUDE 'D0$INC:PI.DEF/LIST'
      CHARACTER*8 HYP(4)
      INTEGER NPVES,LPVES,GZPVES
      INTEGER IEND(4),PRUNIT,USUNIT
      INTEGER LVERT,LTRK(2),ISTAT(4),LBIT,NHYP,K,IER
      LOGICAL FIRST,OK,DO_KS,DO_LM
      REAL K0,PT,PT_MIN,P1_MIN,PVEE_MIN,PHI,DPHI,DXDZ,DYDZ,DDXZ,DDYZ
      REAL THETA,DTHETA,TAN,DTHETA_VEE,ELM,CONST
      REAL DPHI_VEE,PIM,P1, P2, P3,PROTON,LAMBDA,PROB,ETHE,EPHI
      REAL PRB(4),CHI(4),ST(4,10),ET(4,10)
      DATA HYP / 'Ks','Lambda','Lambda','Gamma'/
      SAVE FIRST,PRUNIT,CONST
      DATA PIM/ 0.1395679/, K0/ 0.497671 /,PROTON/ 0.93827231/
      DATA LAMBDA/ 1.11563/, ELM/ 0.51099906E-3/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VEES_RCP')
        CALL EZGET(  'DO_KS',  DO_KS,IER)
        CALL EZGET(  'DO_LM',  DO_LM,IER)
        CALL EZGET(  'PT_MIN',  PT_MIN,IER)
        CALL EZGET('PVEE_MIN',PVEE_MIN,IER)
        CALL EZGET('P1_MIN',P1_MIN,IER)
        CALL EZGET(     'NTR',     NTR,IER)
        CALL EZRSET
        CONST= 0.001143095213      ! = 0.014*sqrt(0.02)/sqrt(3.)
        PRUNIT=USUNIT()
      ENDIF
      VKINEMA=.FALSE.
      CALL VZERO(ST,40)
      CALL VZERO(ET,40)
      LPVES = GZPVES(NPVES)
      LVERT = LQ(LPVES-2)
      STR(2,3) = Q(LPVES + 21)              ! Theta angle for Vee
      ETR(2,3) = Q(LPVES + 22)              ! Theta error for Vee
      STR(3,3) = Q(LPVES + 23)              ! Phi angle for Vee
      ETR(3,3) = Q(LPVES + 24)              ! Phi error for Vee
      CALL UCOPY(STR(1,1),ST(1,1),40)
      CALL UCOPY(ETR(1,1),ET(1,1),40)
C
C-  Fit for the decay K0s -> pi+ & pi-
C
      IF (.NOT.DO_KS) GOTO 50
      STR(4,1) = PIM                 ! ch. pi-meson mass
      STR(4,2) = PIM                 ! ch. pi-meson mass
      STR(4,3) = K0                  ! K0 mass
      P1 = STR(1,1)                  ! momentum of 1-st pion
      P2 = STR(1,2)                  ! momentum of 2-nd pion
      IND(1)=3                       ! treat P1 as unknown
      IND(2)=3                       ! treat P2 as unknown
      IND(3)=3                       ! K0 mass, angles known, momentum unkown
      IF (P1.GT.0)IND(1)=1           ! use calorimeter measurement
      IF (P2.GT.0)IND(2)=1           ! use calorimeter measurement
      IF (ETR(1,3).GT.0.)IND(3)=1
      CALL STEP1_MOMENTA(P1, P2, P3, PT, OK)
      Q(LPVES+19)=P3
      IF (IND(3).EQ.1) STR(1,3) = P3
      IF (IND(1).EQ.3 .AND. IND(2).EQ.3 .AND. IND(3).EQ.3)GOTO 50
      IF (.NOT.OK)                  GOTO 50
      IF( P1.LT.0.1 .OR. P2.LT.0.1) GOTO 50
      IF( PT .LT. PT_MIN)           GOTO 50
      IF( P3 .LT. PVEE_MIN)         GOTO 50
      IF (IND(1).EQ.3) STR(1,1) = P1       ! 1-st step momentum for 1-st track
      IF (IND(2).EQ.3) STR(1,2) = P2       ! 1-st step momentum for 2-nd track
C
C  Add multiple scattering error
C
      EPHI=CONST/P1/SIN(STR(2,1))
      ETR(3,1)=SQRT(ETR(3,1)**2+EPHI**2)
      EPHI=CONST/P2/SIN(STR(2,2))
      ETR(3,2)=SQRT(ETR(3,2)**2+EPHI**2)
      CALL VKIN_FIT(1,IEND(1),PRB(1),CHI(1))
      IF( STR(1,3) .LT. PVEE_MIN) GOTO 50
      IF( STR(1,1) .LT. P1_MIN) GOTO 50
      IF( STR(1,2) .LT. P1_MIN) GOTO 50
      IF (IEND(1) .GT. 0) VKINEMA = .TRUE.
   50 CONTINUE
      CALL UCOPY(ST(1,1),STR(1,1),40)
      CALL UCOPY(ET(1,1),ETR(1,1),40)
C
C-  Fit for the decay Lambda -> proton & pi
C
      IF (.NOT.DO_LM) GOTO 70
      STR(4,1) = PROTON              ! Proton mass
      STR(4,2) = PIM                 ! Pion mass
      STR(4,3) = LAMBDA              ! Lambda mass
      P1 = STR(1,1)                  ! momentum of 1-st pion
      P2 = STR(1,2)                  ! momentum of 2-nd pion
      IND(1)=3                       ! treat P1 as unknown
      IND(2)=3                       ! treat P2 as unknown
      IND(3)=3                       ! Lambda mass,angles known, momentum unkown
      IF (P1.GT.0)IND(1)=1           ! use calorimeter measurement
      IF (P2.GT.0)IND(2)=1           ! use calorimeter measurement
      IF (ETR(1,3).GT.0.)IND(3)=1
      CALL STEP1_MOMENTA(P1, P2, P3, PT, OK)
      Q(LPVES+19)=P3
      IF (IND(3).EQ.1) STR(1,3) = P3
      IF (IND(1).EQ.3 .AND. IND(2).EQ.3 .AND. IND(3).EQ.3)GOTO 60
      IF (.NOT.OK) GOTO 60
      IF( P1.LT.0.1 .OR. P2.LT.0.1) GOTO 60
      IF( PT .LT. PT_MIN)           GOTO 60
      IF( P3 .LT. PVEE_MIN)         GOTO 60
      STR(1,1) = P1             ! 1-st step momentum for 1-st track
      STR(1,2) = P2             ! 1-st step momentum for 2-nd track
C
C  Add multiple scattering error
C
      EPHI=CONST/P1/SIN(STR(2,1))
      ETR(3,1)=SQRT(ETR(3,1)**2+EPHI**2)
      EPHI=CONST/P2/SIN(STR(2,2))
      ETR(3,2)=SQRT(ETR(3,2)**2+EPHI**2)
      CALL VKIN_FIT(1,IEND(2),PRB(2),CHI(2))
      IF( STR(1,3) .LT. PVEE_MIN)       GOTO 60
      IF( STR(1,1) .LT. P1_MIN) GOTO 60
      IF( STR(1,2) .LT. P1_MIN) GOTO 60
      IF (IEND(2).GT.0) VKINEMA = .TRUE.
   60 CONTINUE
      CALL UCOPY(ST(1,1),STR(1,1),40)
      CALL UCOPY(ET(1,1),ETR(1,1),40)
C
C-  Fit for the decay Lambda -> pi & proton
C
      STR(4,1) = PIM                 ! Pion - mass
      STR(4,2) = PROTON              ! Proton - mass
      STR(4,3) = LAMBDA              ! Lambda - mass
      P1 = STR(1,1)                  ! momentum of 1-st pion
      P2 = STR(1,2)                  ! momentum of 2-nd pion
      IND(1)=3                       ! treat P1 as unknown
      IND(2)=3                       ! treat P2 as unknown
      IND(3)=3                       ! Lambda mass,angles known, momentum unkown
      IF (P1.GT.0)IND(1)=1           ! use calorimeter measurement
      IF (P2.GT.0)IND(2)=1           ! use calorimeter measurement
      IF (ETR(1,3).GT.0.)IND(3)=1
      CALL STEP1_MOMENTA(P1, P2, P3, PT, OK)
      Q(LPVES+19)=P3
      IF (IND(3).EQ.1) STR(1,3) = P3
      IF (IND(1).EQ.3 .AND. IND(2).EQ.3 .AND. IND(3).EQ.3)GOTO 70
      IF(.NOT. OK) GOTO 70
      IF( P1.LT.0.1 .OR. P2.LT.0.1) GOTO 70
      IF( PT .LT. PT_MIN)           GOTO 70
      IF( P3 .LT. PVEE_MIN)         GOTO 70
      STR(1,1) = P1             ! 1-st step momentum for 1-st track
      STR(1,2) = P2             ! 1-st step momentum for 2-nd track
C
C  Add multiple scattering error
C
      EPHI=CONST/P1/SIN(STR(2,1))
      ETR(3,1)=SQRT(ETR(3,1)**2+EPHI**2)
      EPHI=CONST/P2/SIN(STR(2,2))
      ETR(3,2)=SQRT(ETR(3,2)**2+EPHI**2)
      CALL VKIN_FIT(1,IEND(3),PRB(3),CHI(3))
      IF (IEND(3) .GT. 0) VKINEMA = .TRUE.
   70 CONTINUE
      NHYP   = 0
      DO 6 K = 1,4
        IF(IEND(K) .LT. 0)          GOTO 6
        IF( PRB(K) .GT. PROB) THEN
          NHYP   = K
          PROB   = PRB(K)
        ENDIF
    6 CONTINUE
C
C ****  Conclusion
C
      IF (VKINEMA .EQV. .FALSE.) THEN  ! None of the hypothesis satisfied
        CALL PVESDR(NPVES)            ! PVES bank dropped
      END IF
  999 RETURN
      END
