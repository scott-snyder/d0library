      SUBROUTINE PJPART(LPJHDI,DR_CUT,ET_CUT,MAX_IT,IR,MUON,NJET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determine PJETs from Particles (ISP1) using
C-      ETA/PHI CONE algorithm.
C-
C-   Inputs  : LPJHD          ...  Address of PJHD bank to hang PJET from
C-             DR_CUT         ...  DELTA R in ETA/PHI SPACE to look for partons
C-             ET_CUT         ...  Et at which a jet doesn't count
C-             MAX_IT         ...  Maximum number of iteration around jet axis
C-             IR             ...  DONT use Initial Radiation then set IR.NE.0
C-             MUON           ...  MUON switch  - 0=no MUONS  - 1=use MUONS
C-   Outputs : NJET - number of PJETs found - PJET bank
C-   Controls: none
C-
C-   Created   5-NOV-1990   Chip Stewart
C-   Updated  13-NOV-1992   Brent May,  Andy Milder
C-      Remove forcing version 2  (version filled in BKPJHD)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C INPUT
      REAL    DR_CUT,ET_CUT
      INTEGER MAX_IT,IR,MUON
C INPUT BANK POINTERS
      INTEGER GZISAE,LSUPV,LSUPP
C PARTON MOMENTA
      INTEGER NPMAX,NJMAX,TRYMAX
      PARAMETER (NPMAX=1000) !MAX PARTICLES
      PARAMETER (NJMAX=100)  !MAX JETS
      PARAMETER (TRYMAX=10)  !MAXIMUM NUMBER F TRYS TO GET ABOVE ETCUT
      INTEGER NP, NV1(NPMAX),NP1(NPMAX),NV1_X(NPMAX),NP1_X(NPMAX)
      REAL    P(4),EM,PM,X,Y,Z
      REAL    P_PART(4,NPMAX),P_X(4,NPMAX),ET_JET(NPMAX)
      REAL    PT(NPMAX),P_AXIS(4),P_JET(4,NJMAX),PT_JET(NJMAX)
C BOOKEEPING ARRAYS, POINTERS
      INTEGER ORDER(NPMAX),JETMAP(NPMAX),SEEDMP(NPMAX),NPART
      INTEGER ID,I,IP,IJ,JP,IDABS,NJET,IPASS,ITRY
      LOGICAL LX,LY,LZ,SKIP
C OBLIGATORY ETA PHI ETC...
      REAL    ETA,PHI,TH,ETMAX
      CHARACTER MESG*89,NAME*8,LABEL*8
      REAL    ETA1,ETA2,PHI1,PHI2,X1,Y1,DR,DETA,DPHI
C ZEBRA INCLUDES
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:LKPJET.INC'
      INCLUDE 'D0$LINKS:IZISV1.LINK'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$INC:PI.DEF'
C ZEBRA POINTERS
      INTEGER GZPJHD,LPJHDI,GZISV1,LZFIND
C----------------------------------------------------------------------
C
      LPJHD = LPJHDI
      IF(LPJHD.EQ.0) LPJHD = GZPJHD()    ! GET LINK to 1st PJHD if LPJHD zero
C
      IF(LPJHD.EQ.0) CALL BKPJHD(LPJHD)  ! If no PJHD bank then Book 1st PJHD
C
C ****  add up parton momenta from ISAQ
C
      LISAE= GZISAE  ()     ! find ISAJET main bank
      LSUPV = LISAE-IZISV1   ! find pointer to first bank
      NP = 0
   10 CALL GTISV1(LSUPV,LISV1,ID,P,X,Y,Z)
      IF( LISV1 .GT. 0 ) THEN
        LSUPP = LISV1 - IZISP1
   11   CALL GTISP1(LSUPP,LISP1,ID,P,PHI,TH,ETA)
        IF( LISP1 .GT. 0 ) THEN
          SKIP = .FALSE.
          IDABS=IABS(IQ(LISP1+1))
C NEUTRINOS
          IF(IDABS.EQ.11.OR.IDABS.EQ.13.OR.IDABS.EQ.15)SKIP =.TRUE.
C MUONS
          IF(IDABS.EQ.14.AND.MUON.EQ.0) SKIP = .TRUE.
C UNDERLYING EVENT
          IF (IR.NE.0 .AND. LQ(LISP1-3).EQ.0) SKIP = .TRUE.
C HIGH ETA
          IF (ABS(ETA) .GT. 6.0 ) SKIP = .TRUE.
          IF (.NOT. SKIP ) THEN
            NP = NP + 1
            CALL UCOPY(P(1),P_X(1,NP),4)
            NV1_X(NP) = IQ(LISV1-5)
            NP1_X(NP) = IQ(LISP1-5)
          END IF
          LSUPP = LISP1
          GOTO 11
        END IF
        LSUPV = LISV1
        GOTO 10
      END IF
      CALL VZERO(SEEDMP(1),NP)
      CALL VZERO(JETMAP,NP)            ! zero any jet pointers
C
      DO 90 IP = 1, NP
        ORDER(IP) = IP
        PT(IP) = -1*SQRT( P_X(1,IP)*P_X(1,IP)+ P_X(2,IP)*P_X(2,IP) )
   90 CONTINUE
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
      NJET=0
      ITRY = 0
    1 ETMAX=0.
      IPASS=0
      IP = 0
C
C ****  FIND SEED PARTON WITH HIGHEST UNUSED PT
C
  200 IP = IP + 1                       ! loop over unused cells
      IF( IP.LE.NP ) THEN
        IF (SEEDMP(IP).EQ.0 .AND.  JETMAP(IP).EQ.0) THEN
          IF(PT(IP) .GT. ETMAX) THEN
            ETMAX= PT (IP)
            SEEDMP(IP)  = NJET + 1    ! DON'T USE SAME PARTON AS SEED TWICE
            DO I = 1, 4
              P_AXIS(I) = P_PART(I,IP)
            END DO
          ENDIF
        ENDIF
C
        GOTO 200
      ENDIF
C
C ****  EXIT FOR JET ALGORITHM
C
      IF(ETMAX.EQ. 0.0 ) GOTO 400       !  no more jets to be found
C
C *** EXTRA PAREMTER TO SPEED UP CODE
C
      IF(ITRY .GT. TRYMAX  ) GOTO 400       !  no more jets to be found
C
      NJET=NJET+1
      IF(NJET.GT.NPMAX) THEN
        WRITE(MESG,99) NJET
   99   FORMAT(' ERROR IN CJANGL...NJET > ',I5)
        CALL D0_ABORT(MESG)
      ENDIF
C
C
C ****  LOOP OVER ITERATIONS THROUGH ALGORITHM
C
    2 IPASS=IPASS+1
C
      IF(IPASS.GT. NP)  THEN
        WRITE(MESG,88) IPASS
   88   FORMAT(' ERROR IN CJANGL...IPASS > ',I6)
        CALL D0_ABORT(MESG)
      ENDIF
C
C **** START A NEW JET
C
      ET_JET(NJET) = 0
      CALL VZERO(P_JET(1,NJET),4)
      IP = 0
  300 IP = IP + 1                       ! loop over unused cells
      IF( IP.LE.NP ) THEN
        IF( JETMAP( IP ) .EQ. 0) THEN
          CALL ISPETA(P_AXIS,TH,PHI1,ETA1)
          CALL ISPETA(P_PART(1,IP),TH,PHI2,ETA2)
          DETA = ETA2 - ETA1
          X1 = COS(PHI2-PHI1)
          Y1 = SIN(PHI2-PHI1)
          IF(X1.EQ.0.0) THEN
            DPHI = HALFPI
          ELSE
            DPHI = ATAN2(Y1,X1)
          END IF
          DR = SQRT(DETA**2+DPHI**2)
          IF( DR  .LT. DR_CUT ) THEN  ! add cell to jet
            ET_JET(NJET) = ET_JET(NJET) + P_PART(4,IP)*SIN(TH)
            DO I = 1, 4
              P_JET(I,NJET)= P_JET(I,NJET) + P_PART(I,IP)
            END DO
            JETMAP(IP) = NJET
          ENDIF
C
        ENDIF
C
        GOTO 300
      ENDIF
C
C ****  CHECK IF NEED FURTHER ITERATIONS
C
      LX = P_JET(1,NJET).EQ. P_AXIS(1)
      LY = P_JET(2,NJET).EQ. P_AXIS(2)
      LZ = P_JET(3,NJET).EQ. P_AXIS(3)
      IF ( LX .AND. LY. AND. LZ .OR. IPASS.GE.MAX_IT) GOTO 3
C
C ****  SET AXIS TO JET MOMENTA, RESET MAP AND GO THROUGH ANOTHER ITERATION
C
      CALL UCOPY(P_JET(1,NJET),P_AXIS(1),4)
      DO IP = 1, NP
        IF ( JETMAP(IP) .EQ. NJET) JETMAP(IP) = 0
      END DO
      GOTO 2
C
C **** DISCARD JET IF Et < ETCUT, FILL COMMON OTHERWISE.
C
    3 PT_JET (NJET) =SQRT(P_JET(1,NJET)**2+P_JET(2,NJET)**2)
      IF(PT_JET(NJET) .LT. ET_CUT) THEN
        ITRY = ITRY + 1
        DO IP = 1, NP
          IF ( JETMAP(IP) .EQ. NJET) JETMAP(IP) = 0
        END DO
        DO I = 1, 4
          P_JET(I,NJET) = 0.0
        END DO
        NJET=NJET-1
      ELSE
        ITRY = 0
      ENDIF
      GO TO 1
C
C ****  FOUND ALL JETS - FILL PJET BANK
C
  400 CONTINUE
C
      DO 295 IJ = 1, NJET
        PT_JET(IJ) = SQRT( P_JET(1,IJ)*P_JET(1,IJ) +
     &    P_JET(2,IJ)*P_JET(2,IJ) )

C
C ****  FINAL CHECK OF JET PT
C
        IF ( PT_JET(IJ).LT.ET_CUT ) GOTO 295
C
C ****  BOOK AND FILL JET BANK FOR THIS JET
C
        CALL BKPJET(LPJHD,LPJET)
C
C ****  BANK VERSION NUMBER ( VERSION 1 HAS PT AS SECOND WORD
C ****                              >=2     ET               )
C
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
     &    + P_JET(2,IJ)*P_JET(2,IJ)
     &    + P_JET(3,IJ)*P_JET(3,IJ) )
        Q(LPJET+7) = SQRT ( (EM+PM)* ABS( (EM-PM) ) )  ! SMALL MASS PROBLEM
        IQ(LPJET+11) = 0
        IQ(LPJET+12) = 0
C
C ****  BOOK AND FILL JET POINTER BANK FOR ALL PJETS
C
        NPART = 0
        DO IP = 1, NP
          JP = JETMAP( IP )
          IF( JP .EQ.IJ) THEN
            NPART =  NPART + 1
          END IF
        END DO
        CALL BKPJPT(LPJET,NPART,LPJPT)
        NPART = 0
        DO 210 IP = 1, NP
          JP = JETMAP( IP )
          IF( JP .EQ.IJ) THEN
            NPART = NPART + 1
            LSUPV=GZISV1()
            IF ( LSUPV .LE. 0 ) THEN
              CALL ERRMSG('PJPART Bad ISV1 LINK','PJET','LSUPV','W')
              LQ(LPJPT-NPART-1) = 0
              GOTO 210
            END IF
            LISV1  = LZFIND(IXMAIN,LSUPV,NV1(IP),-5) ! Get ISV1 address
            IF ( LISV1 .LE. 0 ) THEN
              CALL ERRMSG('PJPART Bad ISV1 LINK','PJET','LISV1','W')
              LQ(LPJPT-NPART-1) = 0
              GOTO 210
            END IF
            LSUPP  = LQ(LISV1-IZISP1)
            IF ( LSUPP .LE. 0 ) THEN
              CALL ERRMSG('PJPART Bad ISP1 LINK','PJET','LSUPP','W')
              LQ(LPJPT-NPART-1) = 0
              GOTO 210
            END IF
            LISP1  = LZFIND(IXMAIN,LSUPP,NP1(IP),-5) ! Get ISP1 address
            IF ( LISP1 .GT. 0 ) THEN
              LQ(LPJPT-NPART-1) = LISP1
            ELSE
              LQ(LPJPT-NPART-1) = 0
              CALL ERRMSG('PJPART Bad ISP1 LINK','PJET','LISP1','W')
            ENDIF
          END IF
  210   CONTINUE
  295 CONTINUE
C
  999 RETURN
      END
