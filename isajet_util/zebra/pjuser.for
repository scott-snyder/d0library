      SUBROUTINE PJUSER(PARTON,LPJHDI,DR_CUT,ET_CUT,MAXIT,IR,MUON,SM,NJ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determine PJETs from Particles (ISP1) using
C-      ETA/PHI CONE algorithm.
C-
C-   Inputs  : PARTON         ...  Logical -use of partons or particles?
C-             LPJHD          ...  Address of PJHD bank to hang PJET from
C-             DR_CUT         ...  DELTA R in ETA/PHI SPACE to look for partons
C-             ET_CUT         ...  Et at which a jet doesn't count
C-             MAXIT          ...  Maximum number of iteration around jet axis
C-             IR             ...  Merging/splitting parameter
C-             MUON           ...  MUON switch  - 0=no MUONS  - 1=use MUONS
C-             SM             ...  Splitting/Merging switch
C-   Outputs : NJ - number of PJETs found - PJET bank
C-   Controls: none
C-
C-   Created  13-NOV-1992   Andrew J. Milder  Heavily modified PJPART
C-                          by Chip Stewart. More closely resembles
C-                          CAJET jet finder. Does both partons
C-                          and particle cones.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C INPUT
      REAL    DR_CUT,ET_CUT
      INTEGER MAXIT,IR,MUON
      LOGICAL PARTON
C INPUT BANK POINTERS
      INTEGER GZISAE,LSUPV,LSUPP
C PARTON MOMENTA
      INCLUDE 'D0$INC:USER_PJET.INC'
      INTEGER TRYMAX
      PARAMETER (TRYMAX=10)  !MAXIMUM NUMBER F TRYS TO GET ABOVE ETCUT
      INTEGER NV1(NPMAX),NP1(NPMAX),NV1_X(NPMAX),NP1_X(NPMAX)
      REAL    P(4),EM,PM,X,Y,Z,SHARE,ET1,ET2,FACTOR
      REAL    P_X(4,NPMAX)
      REAL    PT(NPMAX),P_AXIS(4),PT_JET
C BOOKEEPING ARRAYS, POINTERS
      INTEGER ORDER(NPMAX),SEEDMP(NPMAX),NPART
      INTEGER ID,I,IP,IJ,JP,IDABS,NJET,NJ,IPASS,ITRY,IJET,IJET2,NJ1,NJ2
      LOGICAL LX,LY,LZ,SKIP,SAME_ETA,SAME_PHI
C OBLIGATORY ETA PHI ETC...
      REAL    ETA,PHI,TH,ETMAX,EPS,SM
      CHARACTER MESG*89,NAME*8,LABEL*8
      REAL    ETA1,ETA2,PHI1,PHI2,X1,Y1,DR,DETA,DPHI,PT1
C ZEBRA INCLUDES
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:LKPJET.INC'
      INCLUDE 'D0$LINKS:IZISV1.LINK'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$INC:PI.DEF'
C ZEBRA POINTERS
      INTEGER GZPJHD,LPJHDI,GZISV1,LZFIND,NEVT,GZISAQ
      DATA EPS / .05 /
C----------------------------------------------------------------------
C
      NEVT = NEVT + 1
      NJET = 0
      LPJHD = LPJHDI
      IF(LPJHD.EQ.0) LPJHD = GZPJHD()    ! GET LINK to 1st PJHD if LPJHD zero
C
      IF(LPJHD.EQ.0) CALL BKPJHD(LPJHD)  ! If no PJHD bank then Book 1st PJHD
C
      IF (PARTON) THEN
C
C  Fill arrays with parton momenta from ISAQ banks
C
        LISAQ = GZISAQ()
        IF (LISAQ.EQ.0) THEN
          CALL ERRMSG ( 'PJETS', 'PJUSER',
     &      'No ISAQ banks- skipping event,', 'W' )
          GOTO 999
        ENDIF
        NP = 0
        DO WHILE(LISAQ.NE.0)
C
C ****  NO NEUTRINOS ( OR MUONS if muon switch is set to 0) !
C
          SKIP = .FALSE.
          IDABS=IABS(IQ(LISAQ+1))
C NEUTRINOS
          IF(IDABS.EQ.11.OR.IDABS.EQ.13.OR.IDABS.EQ.15)SKIP =.TRUE.
C MUONS
          IF(IDABS.EQ.14.AND.MUON.EQ.0) SKIP = .TRUE.
C HIGH ETA
          IF (ABS(ETA) .GT. 6.0 ) SKIP = .TRUE.
          IF (.NOT. SKIP ) THEN
            NP = NP + 1
            IF ( NP .GT. NPMAX ) THEN
              CALL ERRMSG ( 'PJETS', 'PJUSER',
     &          'Too many final state partons in ISAQ, using first 1000'
     &          , 'W' )
              GOTO 35
            ENDIF
            CALL UCOPY(Q(LISAQ+2),P_X(1,NP),4)
            NV1_X(NP) = LISAQ
            NP1_X(NP) = NP
          END IF
          LISAQ = LQ(LISAQ)
        ENDDO
      ELSE
C
C  Fill arrays with particle momenta from ISP1 banks
C
        LISAE= GZISAE  ()     ! find ISAJET main bank
        LSUPV = LISAE-IZISV1   ! find pointer to first bank
        NP = 0
   10   CALL GTISV1(LSUPV,LISV1,ID,P,X,Y,Z)
        IF( LISV1 .GT. 0 ) THEN
          LSUPP = LISV1 - IZISP1
   11     CALL GTISP1(LSUPP,LISP1,ID,P,PHI,TH,ETA)
          IF( LISP1 .GT. 0 ) THEN
            SKIP = .FALSE.
            IDABS=IABS(IQ(LISP1+1))
C NEUTRINOS
            IF(IDABS.EQ.11.OR.IDABS.EQ.13.OR.IDABS.EQ.15)SKIP =.TRUE.
C MUONS
            IF(IDABS.EQ.14.AND.MUON.EQ.0) SKIP = .TRUE.
C HIGH ETA
            IF (ABS(ETA) .GT. 6.0 ) SKIP = .TRUE.
            IF (.NOT. SKIP ) THEN
              NP = NP + 1
              IF ( NP .GT. NPMAX ) THEN
                CALL ERRMSG ( 'PJETS', 'PJUSER',
     &
     &'Too many final state particles in ISP1, using first 1000'
     &              , 'W' )
                GOTO 35
              ENDIF                         ! if np .gt. npmax
              CALL UCOPY(P(1),P_X(1,NP),4)
              NV1_X(NP) = LISP1
              NP1_X(NP) = IQ(LISP1-5)
            END IF
            LSUPP = LISP1
            GOTO 11
          END IF
          LSUPV = LISV1
          GOTO 10
        END IF
      ENDIF
   35 CALL VZERO_i(SEEDMP(1),NP)
      CALL VZERO_i(JETMAP,NP*2)            ! zero any jet pointers
      CALL VZERO_i(ISHARE,NJMAX)
C
      DO IP = 1, NP
        ORDER(IP) = IP
        PT(IP) = -1*SQRT( P_X(1,IP)**2 + P_X(2,IP)**2 )
      ENDDO
C
C ****   ORDER PARTONS IN PT
C
      CALL ISASRT(PT,NP,ORDER)
C
      DO IP = 1, NP
        PT(IP) = - PT(IP)
        NV1 (IP) = NV1_X (ORDER(IP))
        NP1 (IP) = NP1_X (ORDER(IP))
        CALL UCOPY(P_X (1,ORDER(IP)), P_PART(1,IP), 4)
      END DO
C
C ****  INITIALIZE JET ALG
C
      ITRY = 0
    1 ETMAX=0.
      IPASS=0
      IP = 0
C
C ****  FIND SEED PARTON WITH HIGHEST UNUSED PT
C
      DO IP = 1, NP                           ! loop over unused cells
        IF (SEEDMP(IP).EQ.0 .AND. JETMAP(1,IP).EQ.0
     &    .AND. JETMAP(2,IP).EQ.0) THEN
          IF(PT(IP) .GT. ETMAX) THEN
            ETMAX= PT (IP)
            SEEDMP(IP)  = NJET + 1    ! DON'T USE SAME PARTON AS SEED TWICE
            DO I = 1, 4
              P_AXIS(I) = P_PART(I,IP)
            END DO
          ENDIF
        ENDIF
      ENDDO
C
C ****  EXIT FOR JET ALGORITHM
C
      IF(ETMAX.EQ. 0.0 ) GOTO 400       !  no more jets to be found
C
C *** EXTRA PARAMETER TO SPEED UP CODE
C
      IF(ITRY .GT. TRYMAX  ) GOTO 400       !  no more jets to be found
C
      NJET=NJET+1
      IF(NJET.GT.NJMAX) THEN
        WRITE(MESG,99) NJMAX
   99   FORMAT(' ERROR IN PJUSER...NJET > ',I5)
        CALL ERRMSG(MESG,'PJUSER','BAD','F')
      ENDIF
      IJET = INT(FLOAT(NJET)/32.) + 1
      NJ1 = NJET - (IJET-1)*32
C
C
C ****  LOOP OVER ITERATIONS THROUGH ALGORITHM
C
    2 IPASS=IPASS+1
C
C **** START A NEW JET
C
      ET_JET(NJET) = 0
      CALL VZERO(P_JET(1,NJET),4)
C
      DO IP = 1, NP                ! loop over ALL cells
        CALL ISPETA(P_AXIS,TH,PHI1,ETA1)
        CALL ISPETA(P_PART(1,IP),TH,PHI2,ETA2)
        DETA = ETA2 - ETA1
        DPHI = ABS(PHI2-PHI1)
        IF (DPHI.GT.PI) DPHI = 2*PI-DPHI
        DR = SQRT(DETA**2+DPHI**2)
        IF( DR  .LT. DR_CUT ) THEN  ! add cell to jet
          ET_JET(NJET) = ET_JET(NJET) + P_PART(4,IP)*SIN(TH)
          DO I = 1, 4
            P_JET(I,NJET)= P_JET(I,NJET) + P_PART(I,IP)
          END DO
          JETMAP(IJET,IP) = IBSET(JETMAP(IJET,IP),NJ1)
        ENDIF
      ENDDO
C
C ****  CHECK IF NEED FURTHER ITERATIONS
C
      LX = P_JET(1,NJET).EQ. P_AXIS(1)
      LY = P_JET(2,NJET).EQ. P_AXIS(2)
      LZ = P_JET(3,NJET).EQ. P_AXIS(3)
      IF ( LX .AND. LY. AND. LZ .OR. IPASS.GE.MAXIT) GOTO 3
C
C ****  SET AXIS TO JET MOMENTA, RESET MAP AND GO THROUGH ANOTHER ITERATION
C
      CALL UCOPY(P_JET(1,NJET),P_AXIS(1),4)
      DO I = 1, NP
        JETMAP(IJET,I) = IBCLR(JETMAP(IJET,I),NJ1)
      ENDDO
      GOTO 2
C
C **** DISCARD JET IF Et < ETCUT OR IF ALREADY FOUND, FILL COMMON OTHERWISE.
C
    3 PT_JET = SQRT(P_JET(1,NJET)**2+P_JET(2,NJET)**2)
      IF(PT_JET .LT. ET_CUT/4.) THEN
        ITRY = ITRY + 1
        DO I = 1, 4
          P_JET(I,NJET) = 0.0
        END DO
        DO IP = 1, NP
          JETMAP(IJET,IP) = IBCLR(JETMAP(IJET,IP),NJ1)
        END DO
        NJET=NJET-1
      ELSE
        ITRY = 0
C
C CHECK IF THIS JET WAS ALREADY FOUND
C
        CALL ISPETA(P_JET(1,NJET),TH,PHI,ETA)
        DO I = 1, NJET-1
          CALL ISPETA(P_JET(1,I),TH,PHI2,ETA2)
          SAME_ETA = (ABS(ETA-ETA2).LT.EPS)
          SAME_PHI = (ABS(PHI-PHI2).LT.EPS)
          IF (SAME_ETA.AND.SAME_PHI) THEN         ! SAME JET, DROP IT
            P_JET(1,NJET) = 0.
            P_JET(2,NJET) = 0.
            GOTO 3
          ENDIF
        ENDDO
C
C DO SPLITTING/MERGING
C
        CALL PJSPLIT(SM,DR_CUT,NJET)
C
      ENDIF
      GO TO 1
C
C ****  FOUND ALL JETS - FILL PJET BANK
C
  400 CONTINUE
C
      DO 295 IJ = 1, NJET
        IJET = INT(FLOAT(IJ)/32.) + 1
        NJ1 = IJ - (IJET-1)*32
C
C ****  FINAL CHECK OF JET PT
C
        IF ( ET_JET(IJ).LT.ET_CUT ) THEN
          NJET = NJET - 1
          GOTO 295
        ENDIF
C
C ****  BOOK AND FILL JET BANK FOR THIS JET
C
        CALL BKPJET(LPJHD,LPJET)
C
C ****  BANK VERSION NUMBER ( VERSION 1 HAS PT AS SECOND WORD
C ****                                2     ET               )
C
        IQ(LPJET+1)= 2
        Q(LPJET+2) = ET_JET(IJ)
        Q(LPJET+3) = P_JET(1,IJ)
        Q(LPJET+4) = P_JET(2,IJ)
        Q(LPJET+5) = P_JET(3,IJ)
        Q(LPJET+6) = P_JET(4,IJ)
        CALL ISPETA(P_JET(1,IJ),TH,PHI,ETA)
        Q(LPJET+8) = PHI
        Q(LPJET+9)  = TH
        Q(LPJET+10) = ETA
        EM = P_JET(4,IJ)
        PM = SQRT ( P_JET(1,IJ)*P_JET(1,IJ)
     &        + P_JET(2,IJ)*P_JET(2,IJ)
     &        + P_JET(3,IJ)*P_JET(3,IJ) )
        Q(LPJET+7) = SQRT ( (EM+PM)* ABS( (EM-PM) ) )  ! SMALL MASS PROBLEM
C
        NPART = 0
        DO IP = 1, NP
          IF (BTEST(JETMAP(IJET,IP),NJ1)) NPART = NPART + 1
        ENDDO
C
        IQ(LPJET+11) = 0
        IQ(LPJET+12) = 0
        IQ(LPJET+13) = ISHARE(IJ)
C
C ****  BOOK AND FILL JET POINTER BANK FOR ALL PJETS
C
        CALL BKPJPT(LPJET,NPART,LPJPT)
        NPART = 0
        DO IP = 1, NP
          IF (BTEST(JETMAP(IJET,IP),NJ1)) THEN
            NPART = NPART + 1
            LQ(LPJPT-NPART-1) = NV1(IP)
          END IF
        ENDDO
  295 CONTINUE
C
  999 CONTINUE
      NJ = NJET
C
      RETURN
      END
