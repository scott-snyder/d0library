      SUBROUTINE GM_MUON(SWITCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : COMIS routine to histogram some EM quantities.
C-   NOTE: In PAW this routine will be called once per object as follows
C-
C-   PAW > NT/LOOP 1 GM_MUON.FOR(0)
C-
C-   after the histograms have been booked or reset.
C-
C-   HISTOGRAMS
C-
C ****  1100:  ET OF MUONS
C ****  1110:  ET OF CLEAN MUONS
C-
C ****  1300: MISSING ET (PNUT(3)) OF EVENTS WITH AT LEAST ONE CLEAN MUON
C ****  1400: TRANSVERSE MASS OF (mu,nu) PAIR WITH AT LEAST ONE CLEAN MUON
C ****  1500: MASS OF (mu,mu) PAIR WITH TWO CLEAN MUONS
C-
C-   Inputs  : SWITCH   [R]   0.0 -- ALL TRIGGERS
C-                            1.0 -- MUON TRIGGERS ONLY
C-   Outputs : None
C-
C-   Created  20-Jan-1993   K. Wyatt Merritt 
C-   Updated   1-MAR-1993   Harrison B. Prosper
C-    Equivalence IMUON and MUON; correct mass calculation (factor 2) 
C-   Updated  10-MAR-1993   K. Wyatt Merritt  add eta-phi hists; lower
C-                          cuts for clean muons and have separate cuts
C-                          for W muons 
C-   Updated   3-JAN-1994   Harrison B. Prosper
C-    Use GMPAW.INC
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    SWITCH
      REAL BOOK
C-----------------------------------------------------------------------
      INCLUDE 'GMPAW.INC'
C--------------------------------------------------------------------------
      INTEGER I,J,JCLEAN,JCLEAN1,JCLEAN2,IDOFF,JBIT,IFW1
      INTEGER NCMUON
      INTEGER IFW4,IFW2
      INTEGER IMUON(NSIZ,NOBJ)
      EQUIVALENCE (IMUON(1,1),MUON(1,1))
C
      REAL    MIN_INTEG_BDL,MAX_IMPACT
      REAL    DPHI_MAX
      REAL    DTHETA_MAX
      REAL    MAX_MUON_ETA,MIN_IONIZING
      REAL    MUON_PT_CUT, NU_PT_CUT
      REAL    MASS,PX1,PX2,PY1,PY2,PZ1,PZ2,E1,E2,P1,P2
      REAL    MET,W_MUON_PT_CUT
      REAL    IBDL,NCDTRK,DPHI,DTHETA,IMPACT,CALEN
C
      LOGICAL CLMUON(NOBJ)
      LOGICAL FIRST, MUON_OBJECT
C--------------------------------------------------------------------------
      DATA IDOFF /1000/
C
      DATA MIN_INTEG_BDL  /0.6/
      DATA DPHI_MAX       /100.0/
      DATA DTHETA_MAX     /100.0/
      DATA MAX_IMPACT     /25.0/
      DATA MIN_IONIZING   /0.5/
      DATA MAX_MUON_ETA   /3.2/
      DATA MUON_PT_CUT    /3.0/
      DATA W_MUON_PT_CUT  /10.0/
      DATA NU_PT_CUT      /10.0/
C--------------------------------------------------------------------------
      DATA FIRST  /.TRUE./
C--------------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        PRINT *, ' GM_MUON: Started'
      ENDIF
C
C *********************************************************************
C ****  FILL OBJECT BUFFERS
C *********************************************************************
C
      IOBJECT = IFIX(OBJECT)
C
      IF     ( IOBJECT .EQ. ID_VERT ) THEN
C
C ****  INITIALIZE
C
        NVERT  = ICOUNT
        NPHOT  = 0
        NELEC  = 0
        NMUON  = 0
        NJET   = 0
        NTAU   = 0
        NNU    = 0
        NCMUON = 0
C
      ELSEIF ( IOBJECT .EQ. ID_MUON ) THEN
        INUMBER = MIN(IFIX(NUMBER),NOBJ)
        ICOUNT  = IFIX(COUNT)
        NMUON   = ICOUNT
        CALL UCOPY(PX,MUON(1,INUMBER),NSIZ)
        CLMUON(INUMBER) = .FALSE.
C
      ELSEIF ( IOBJECT .EQ. ID_NU   ) THEN
        INUMBER = MIN(IFIX(NUMBER),NOBJ)
        ICOUNT  = IFIX(COUNT)
        NNU     = ICOUNT
        CALL UCOPY(PX,NU(1,INUMBER),NSIZ)
C
      ENDIF
C
C *********************************************************************
C ****  FLAG CLEAN MUON OBJECTS
C *********************************************************************
C
      MUON_OBJECT = IOBJECT .EQ. ID_MUON
C
      IF ( MUON_OBJECT ) THEN
C
C **** REQUIRE IFW4 = 0 (TRACK QUALITY GOOD)
C
        IF (IMUON(16,INUMBER).NE.0) GO TO 99
C
C **** COSMIC RAY REJECTION (BITS IN IFW2)
C
        IF (JBIT(IQUALITY,7).NE.0) GO TO 99
        IF (JBIT(IQUALITY,8).NE.0) GO TO 99
        IF (JBIT(IQUALITY,9).NE.0) GO TO 99
C
C **** REQUIRE HITS IN A LAYER (IFW1 = 1 OR 4)        
C
C        IFW1 = IMUON(17,INUMBER)
C        IF (IFW1 .GT. 10) IFW1 = IFW1 - 10
C        IF (IFW1 .EQ. 1) GO TO 99
C        IF (IFW1 .EQ. 4) GO TO 99
C
C **** REQUIRE MINIMUM INTEGRAL B.DL
C
        IF (X9 .LT. MIN_INTEG_BDL) GO TO 99
C
C **** REQUIRE CD TRACK MATCH WITH GOOD QUALITY *OR* CALORIMETER ENERGY
C
        IF (X4 .LT. 1) GO TO 88
C        IF (X10 .GT. DPHI_MAX) GO TO 88
C        IF (X11 .GT. DTHETA_MAX) GO TO 88
        GO TO 90
   88   IF (X12 .LT. MIN_IONIZING) GO TO 99
   90   CONTINUE
C
C **** REQUIRE MAXIMUM IMPACT PARAMETER AT VERTEX
C
        IF (X6 .GT. MAX_IMPACT) GO TO 99
C
C
C **** REQUIRE ETA CUT (CURRENTLY ABS(ETA) .LT. 1.7)
C
        IF (ABS(ETA) .GT. MAX_MUON_ETA) GO TO 99
C
C **** REQUIRE PT CUT
C
        IF (ET .LT. MUON_PT_CUT) GO TO 99
C
C ====>  CLEAN MUON HERE !!!!!
C
        CLMUON(INUMBER) = .TRUE.
C
      ENDIF
   99 CONTINUE
C
C *********************************************************************
C ****  RETURN UNLESS THIS IS END-OF-EVENT OBJECT
C *********************************************************************
C
      IF ( IOBJECT .NE. ID_END  ) GOTO 999
C
C *********************************************************************
C ****  WE NOW HAVE ALL THE OBJECTS OF INTEREST
C *********************************************************************
C
C ****  1100:  ET OF MUONS
C ****  1101:  ETA OF MUONS
C ****  1102:  PHI OF MUONS
C ****  1110:  ET OF CLEAN MUONS
C ****  1111:  ETA OF CLEAN MUONS
C ****  1112:  PHI OF CLEAN MUONS
C
      IF ( NMUON .GT. 0 ) THEN
        DO I =  1, NMUON
          ET    = MUON(IET,I)
          ETA   = MUON(IETA,I)
          PHI   = MUON(IPHI,I)
          IFW4  = IMUON(16,I)
          IFW2  = IMUON(IQUAL,I)/64
          IFW1  = IMUON(17,I)
          IF (IFW1.GT.10) IFW1 = IFW1 - 10
          IBDL  = MUON(IBASE+9,I)
          NCDTRK = MUON(IBASE+4,I)
          DPHI   = MUON(IBASE+10,I)
          DTHETA = MUON(IBASE+11,I)
          IMPACT = MUON(IBASE+6,I)
          CALEN  = MUON(IBASE+12,I)
C
          CALL HF1(IDOFF+100,ET,1.0)
          CALL HF1(IDOFF+101,ETA,1.0)
          CALL HF1(IDOFF+102,PHI,1.0)
          CALL HF1(IDOFF+1,FLOAT(IFW4),1.0)
          CALL HF1(IDOFF+2,FLOAT(IFW2),1.0)
          CALL HF1(IDOFF+3,FLOAT(IFW1),1.0)
          CALL HF1(IDOFF+4,IBDL,1.)
          CALL HF1(IDOFF+5,NCDTRK,1.)
          CALL HF1(IDOFF+6,DPHI,1.)
          CALL HF1(IDOFF+7,DTHETA,1.)
          CALL HF1(IDOFF+8,IMPACT,1.)
          CALL HF1(IDOFF+9,CALEN,1.)
C
C ****  CLEAN MUONS ONLY
C
          IF ( CLMUON(I) ) THEN
            NCMUON = NCMUON + 1
            CALL HF1(IDOFF+110,ET,1.0)
            CALL HF1(IDOFF+111,ETA,1.0)
            CALL HF1(IDOFF+112,PHI,1.0)
            CALL HF1(IDOFF+115,NCDTRK,1.)
            CALL HF1(IDOFF+119,CALEN,1.)
          ENDIF
        ENDDO
      ENDIF
      CALL HF1(IDOFF+200,FLOAT(NCMUON),1.)
C
C
C ****  300: MISSING ET (PNUT(2)) OF EVENTS WITH AT LEAST ONE CLEAN MUON
C ****  400: TRANSVERSE MASS OF (e,nu) PAIR
C
      IF ( NCMUON .GT. 0 ) THEN
        IF ( NNU .GE. 3 ) THEN
C
C ****  MISSING ET
C
          MET = NU(IET,3)
C
C ****  TRANSVERSE MASS
C
C ****  APPLY MISSING ET CUT
C
          IF ( MET .GT. NU_PT_CUT ) THEN
C
C ****  PICK FIRST CLEAN MUON ABOVE PT CUT
C
            JCLEAN = 0
            DO I = 1, NMUON
              IF ( CLMUON(I) ) THEN
                ET = MUON(IET,I)
                IF ( ET .GT. W_MUON_PT_CUT ) THEN
                  JCLEAN = I
                ENDIF
              ENDIF
            ENDDO
C
C ****  Check if we have a candidate
C
            IF ( JCLEAN .GT. 0 ) THEN
C
              PX1 = MUON(IPX,JCLEAN)
              PY1 = MUON(IPY,JCLEAN)
              E1  = MUON(IE,JCLEAN)
C
              PX2 = NU(IPX,3)
              PY2 = NU(IPY,3)
              E2  = NU(IE,3)
C
              P1 = SQRT(PX1*PX1+PY1*PY1)
              P2 = SQRT(PX2*PX2+PY2*PY2)
C
              MASS = 2.0*(P1*P2 - PX1*PX2 - PY1*PY2)
              IF ( MASS .GT. 0.0 ) THEN
                MASS = SQRT(MASS)
              ELSE
                MASS = 0.0
              ENDIF
C
              CALL HF1(IDOFF+300,MET,1.0)
              CALL HF1(IDOFF+400,MASS,1.0)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
C ****  500: MASS OF (mu,mu) PAIR
C
      IF ( NCMUON .GE. 2 ) THEN
C
C ****  MU MU MASS (WITH TWO CLEAN MUONS)
C
        JCLEAN1 = 0
        JCLEAN2 = 0
C
C ****  PICK FIRST CLEAN MUON
C
        DO I = 1,NMUON
          IF ( CLMUON(I) ) THEN
            JCLEAN1 = I
          ENDIF
        ENDDO
        DO I = JCLEAN1,NMUON
          IF ( CLMUON(I) ) THEN
            JCLEAN2 = I
          ENDIF
        ENDDO
C
C ****  COMPUTE MASS
C
        PX1 = MUON(IPX,JCLEAN1)
        PY1 = MUON(IPY,JCLEAN1)
        PZ1 = MUON(IPZ,JCLEAN1)
        E1  = MUON(IE,JCLEAN1)
C
        PX2 = MUON(IPX,JCLEAN2)
        PY2 = MUON(IPY,JCLEAN2)
        PZ2 = MUON(IPZ,JCLEAN2)
        E2  = MUON(IE,JCLEAN2)
C
        MASS = 2.0*(E1*E2 - (PX1*PX2+PY1*PY2+PZ1*PZ2))
        IF ( MASS .GT. 0.0 ) THEN
          MASS = SQRT(MASS)
        ELSE
          MASS = 0.0
        ENDIF
C
        CALL HF1(IDOFF+500,MASS,1.0)
      ENDIF
C
  999 RETURN
      END
