      SUBROUTINE GM_ELECTRON(SWITCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : COMIS routine to histogram some EM quantities.
C-   NOTE: In PAW this routine will be called once per object as follows
C-
C-   PAW > NT/LOOP 1 GM_ELECTRON.FOR(0)
C-
C-   after the histograms have been booked or reset.
C-
C-   HISTOGRAMS
C-
C ****  100:  ET OF ELECTRONS
C ****  110:  ET OF CLEAN ELECTRONS
C-
C ****  200:  ET OF PHOTONS
C ****  210:  ET OF CLEAN PHOTONS
C-
C ****  300: MISSING ET (PNUT(2)) OF EVENTS WITH AT LEAST ONE CLEAN ELECTRON
C ****  400: TRANSVERSE MASS OF (e,nu) PAIR WITH AT LEAST ONE CLEAN ELECTRON
C ****  500: MASS OF (e,(e,gamma)) PAIR WITH AT LEAST ONE CLEAN ELECTRON
C ****  600: MASS OF (gamma,gamma) PAIR WITH AT LEAST ONE CLEAN GAMMA
C-
C-   Inputs  : SWITCH   [R]   0.0 -- ALL TRIGGERS
C-                            1.0 -- EM TRIGGERS ONLY
C-   Outputs : None
C-
C-   Created   4-Dec-1992   Boaz Klima, Harrison B. Prosper
C-   Updated   5-DEC-1992   Harrison B. Prosper
C-   Updated   1-MAR-1993   Harrison B. Prosper
C-    Correct mass calculation
C-   Updated   3-JAN-1994   Harrison B. Prosper
C-    Use GMPAW.INC
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    SWITCH
C--------------------------------------------------------------------------
      INCLUDE 'GM$PAW:GMPAW.INC'
C--------------------------------------------------------------------------
      INTEGER I,J,ICUT,JCLEAN,JOTHER,IDOFF
      INTEGER NCPHOT, NCELEC
C
      REAL    ISOL, CHISQ, DIST, ECONE, EMCORE, ABSETA
      REAL    ISOL_CUT, CHISQ_CUT(3), DIST_CUT, E_PT_CUT, NU_PT_CUT
      REAL    MASS,PX1,PX2,PY1,PY2,PZ1,PZ2,E1,E2,P1,P2
C
      LOGICAL CLPHOT(NOBJ), CLELEC(NOBJ)
      LOGICAL FIRST, EM_OBJECT
C--------------------------------------------------------------------------
      DATA CHISQ_CUT    /200.0,60.0,35.0/
      DATA ISOL_CUT     /0.1/
      DATA DIST_CUT     /6.0/
      DATA E_PT_CUT     /20.0/
      DATA NU_PT_CUT    /20.0/
C--------------------------------------------------------------------------
      DATA FIRST  /.TRUE./
C--------------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        IDOFF = 0
        PRINT *, ' GM_ELECTRON: Started'
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
        NCELEC = 0
        NCPHOT = 0
C
      ELSEIF ( IOBJECT .EQ. ID_PHOT ) THEN
        INUMBER = MIN(IFIX(NUMBER),NOBJ)
        ICOUNT  = IFIX(COUNT)
        NPHOT   = ICOUNT
        CALL UCOPY(PX,PHOTON(1,INUMBER),NSIZ)
        CLPHOT(INUMBER) = .FALSE.
C
      ELSEIF ( IOBJECT .EQ. ID_ELEC ) THEN
        INUMBER = MIN(IFIX(NUMBER),NOBJ)
        ICOUNT  = IFIX(COUNT)
        NELEC   = ICOUNT
        CALL UCOPY(PX,ELECTRON(1,INUMBER),NSIZ)
        CLELEC(INUMBER) = .FALSE.
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
C ****  FLAG CLEAN EM OBJECTS
C *********************************************************************
C
      EM_OBJECT = (IOBJECT .EQ. ID_ELEC) .OR.
     &            (IOBJECT .EQ. ID_PHOT)
C
      IF ( EM_OBJECT ) THEN
C
C ****  CUT:  CHISQ
C
        CHISQ = X9
        ABSETA = ABS(ETA)
        IF     ( ABSETA .LT. 2.6 ) THEN
          ICUT = 1
        ELSEIF ( ABSETA .LT. 3.2 ) THEN
          ICUT = 2
        ELSE
          ICUT = 3
        ENDIF
C
        IF ( CHISQ .LT. CHISQ_CUT(ICUT) ) THEN
C
C ****  CUT:  ISOLATION
C
          ECONE = X5
          EMCORE= X6
          IF ( EMCORE .GT. 0.0 ) THEN
            ISOL  = (ECONE-EMCORE)/EMCORE
          ELSE
            ISOL  = 999.0
          ENDIF
C
          IF ( ISOL .LT. ISOL_CUT ) THEN
C
C ****  Now distinguish between photons and electrons
C
            IF ( IOBJECT .EQ. ID_PHOT ) THEN
              NCPHOT = NCPHOT + 1
              CLPHOT(INUMBER) = .TRUE.
            ELSE
C
C ****  CUT:  DISTANCE OF CLOSEST APPROACH
C
              DIST  = X11
              IF ( DIST .LT. DIST_CUT ) THEN
                NCELEC = NCELEC + 1
                CLELEC(INUMBER) = .TRUE.
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
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
C ****  100:  ET OF ELECTRONS
C ****  110:  ET OF CLEAN ELECTRONS
C
      IF ( NELEC .GT. 0 ) THEN
        DO I =  1, NELEC
          ET    = ELECTRON(IET,I)
          CHISQ = ELECTRON(IBASE+9,I)
          DIST  = ELECTRON(IBASE+11,I)
C
          CALL HF1(IDOFF+100,ET,1.0)
C
C ****  CLEAN ELECTRONS ONLY
C
          IF ( CLELEC(I) ) THEN
            CALL HF1(IDOFF+110,ET,1.0)
          ENDIF
        ENDDO
      ENDIF
C
C ****  200:  ET OF PHOTONS
C ****  210:  ET OF CLEAN PHOTONS
C
      IF ( NPHOT .GT. 0 ) THEN
        DO I =  1, NPHOT
          ET = PHOTON(IET,I)
C
          CALL HF1(IDOFF+200,ET,1.0)
C
C ****  CLEAN PHOTONS ONLY
C
          IF ( CLPHOT(I) ) THEN
            CALL HF1(IDOFF+210,ET,1.0)
          ENDIF
        ENDDO
      ENDIF
C
C
C ****  300: MISSING ET (PNUT(2)) OF EVENTS WITH AT LEAST ONE CLEAN ELECTRON
C ****  400: TRANSVERSE MASS OF (e,nu) PAIR
C
      IF ( NCELEC .GT. 0 ) THEN
        IF ( NNU .GE. 2 ) THEN
C
C ****  MISSING ET
C
          ET = NU(IET,2)
          CALL HF1(IDOFF+300,ET,1.0)
C
C ****  TRANSVERSE MASS
C
C ****  APPLY MISSING ET CUT
C
          IF ( ET .GT. NU_PT_CUT ) THEN
C
C ****  PICK FIRST CLEAN ELECTRON ABOVE PT CUT
C
            JCLEAN = 0
            DO I = 1, NELEC
              IF ( JCLEAN .LE. 0 ) THEN
                IF ( CLELEC(I) ) THEN
                  ET = ELECTRON(IET,I)
                  IF ( ET .GT. E_PT_CUT ) THEN
                    JCLEAN = I
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
C
C ****  Check if we have a candidate
C
            IF ( JCLEAN .GT. 0 ) THEN
C
              PX1 = ELECTRON(IPX,JCLEAN)
              PY1 = ELECTRON(IPY,JCLEAN)
              E1  = ELECTRON(IE,JCLEAN)
C
              PX2 = NU(IPX,2)
              PY2 = NU(IPY,2)
              E2  = NU(IE,2)
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
              CALL HF1(IDOFF+400,MASS,1.0)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
C ****  500: MASS OF (e,e) PAIR
C
      IF ( NCELEC .GE. 1 ) THEN
        IF ( NELEC .GE. 2 ) THEN
C
C ****  E, E MASS (WITH ONE CLEAN E)
C
          JCLEAN = 0
          JOTHER = 0
          IF ( CLELEC(1) .EQ. 1 ) THEN
            JCLEAN = 1
            JOTHER = 2
          ELSE
            JOTHER = 1
C
C ****  PICK FIRST CLEAN ELECTRON
C
            DO I = 2, NELEC
              IF ( JCLEAN .LE. 0 ) THEN
                IF ( CLELEC(I) ) THEN
                  JCLEAN = I
                ENDIF
              ENDIF
            ENDDO
          ENDIF
C
C ****  COMPUTE MASS
C
          PX1 = ELECTRON(IPX,JCLEAN)
          PY1 = ELECTRON(IPY,JCLEAN)
          PZ1 = ELECTRON(IPZ,JCLEAN)
          E1  = ELECTRON(IE,JCLEAN)
C
          PX2 = ELECTRON(IPX,JOTHER)
          PY2 = ELECTRON(IPY,JOTHER)
          PZ2 = ELECTRON(IPZ,JOTHER)
          E2  = ELECTRON(IE,JOTHER)
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
      ENDIF
C
C
C ****  500: MASS OF (e,gamma) PAIR
C
      IF ( NCELEC .GE. 1 ) THEN
        IF ( NPHOT .GE. 1 ) THEN
C
C ****  E, GAMMA MASS (WITH A CLEAN E)
C
          JCLEAN = 0
          IF ( CLELEC(1) ) THEN
            JCLEAN = 1
          ELSE
C
C ****  PICK FIRST CLEAN ELECTRON
C
            DO I = 2, NELEC
              IF ( JCLEAN .LE. 0 ) THEN
                IF ( CLELEC(I) ) THEN
                  JCLEAN = I
                ENDIF
              ENDIF
            ENDDO
          ENDIF
C
C ****  COMPUTE MASS
C
          PX1 = ELECTRON(IPX,JCLEAN)
          PY1 = ELECTRON(IPY,JCLEAN)
          PZ1 = ELECTRON(IPZ,JCLEAN)
          E1  = ELECTRON(IE,JCLEAN)
C
          PX2 = PHOTON(IPX,1)
          PY2 = PHOTON(IPY,1)
          PZ2 = PHOTON(IPZ,1)
          E2  = PHOTON(IE,1)
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
      ENDIF
C
C ****  600: MASS OF (gamma,gamma) PAIR
C
      IF ( NCPHOT .GE. 1 ) THEN
        IF ( NPHOT .GE. 2 ) THEN
C
C ****  GAMMA, GAMMA MASS (WITH ONE CLEAN GAMMA)
C
          JCLEAN = 0
          JOTHER = 0
          IF ( CLPHOT(1) .EQ. 1 ) THEN
            JCLEAN = 1
            JOTHER = 2
          ELSE
            JOTHER = 1
C
C ****  PICK FIRST CLEAN PHOTON
C
            DO I = 2, NPHOT
              IF ( JCLEAN .LE. 0 ) THEN
                IF ( CLPHOT(I) ) THEN
                  JCLEAN = I
                ENDIF
              ENDIF
            ENDDO
          ENDIF
C
C ****  COMPUTE MASS
C
          PX1 = PHOTON(IPX,JCLEAN)
          PY1 = PHOTON(IPY,JCLEAN)
          PZ1 = PHOTON(IPZ,JCLEAN)
          E1  = PHOTON(IE,JCLEAN)
C
          PX2 = PHOTON(IPX,JOTHER)
          PY2 = PHOTON(IPY,JOTHER)
          PZ2 = PHOTON(IPZ,JOTHER)
          E2  = PHOTON(IE,JOTHER)
C
          MASS = 2.0*(E1*E2 - (PX1*PX2+PY1*PY2+PZ1*PZ2))
          IF ( MASS .GT. 0.0 ) THEN
            MASS = SQRT(MASS)
          ELSE
            MASS = 0.0
          ENDIF
C
          CALL HF1(IDOFF+600,MASS,1.0)
        ENDIF
      ENDIF
C
  999 RETURN
      END
