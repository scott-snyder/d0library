      SUBROUTINE CPHANL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CAPHEL analysis package for D0RECO
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Rewritten 24-Apr-1990  N. A. Graf (old CPHANL is now CCLANL)
C-   Updated  12-FEB-1991   John Womersley  photons and pizeros
C-   Updated   2-OCT-1992   Rajendran Raja  removed LZTRK declaration 
C-   Updated   1-NOV-1995   Meenakshi Narain protect linnks used from
C-                            being negative or zero.
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
C
      INCLUDE 'D0$INC:CEMPRF.INC'
C
      INCLUDE 'D0$LINKS:IZCACH.LINK'
      INCLUDE 'D0$LINKS:IZPELC.LINK'
      INCLUDE 'D0$LINKS:IZPPHO.LINK'
      INCLUDE 'D0$LINKS:IZHMTE.LINK'
      INCLUDE 'D0$LINKS:IZHMTP.LINK'
C
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER GZPELC,GZPPHO,GZPNUT,I,J
      INTEGER IDEPTH,SSUNIT,DMPUNI
      REAL    FACTOR,YDUM,DELI,DELJ,PZN
C
      REAL DCLA,NZTRAKS
C
      INTEGER NELEC,NPHOT
      REAL ET_ELEC,ETA_ELEC,PHI_ELEC
      REAL ET_PHOT,ETA_PHOT,PHI_PHOT
      REAL ET_MISS,PHI_MISS,MT_ELEC
      REAL ECLUS,ECORE,EISOLATION
      REAL ZETA
      INTEGER NCELL, CENTRAL_TOWER, IETA
C
      INTEGER IER
      LOGICAL FIRST,LMONTE,VERIFY,FLGVAL
      LOGICAL DO_ANALYSIS,DO_CPHANL1,DO_CPHANL2
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      VERIFY=FLGVAL('VERIFY')
C
      CALL DHDIR('CAPHEL_RCP','HBOOK_DIRECTORY',IER,' ')
C         ! Create/Set HBOOK directory
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('CALORIMETER','CPHANL',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      IF(FIRST)THEN
        FIRST = .FALSE.
        CALL EZPICK('CAPHEL_RCP')
        CALL EZGET('DO_CPHANL',DO_ANALYSIS,IER)
        CALL EZGET('DO_CPHANL1',DO_CPHANL1,IER)
        CALL EZGET('DO_CPHANL2',DO_CPHANL2,IER)
        CALL EZRSET
        CALL EZPICK('CAHITS_RCP')
        CALL EZGET('MONTE_CARLO_DATA',LMONTE,IER)
        CALL EZRSET
C
        IF(DO_ANALYSIS .OR. VERIFY) THEN
          CALL HBOOK1(100,'No. of electron clusters',20,-0.5,19.5,0.)
          CALL HBOOK1(101,'Distance of closest approach of ZTRAK',
     &                    50,0.,5.,0.)
          CALL HBOOK1(102,'No. of ZTRAKS/cluster',20,-0.5,19.5,0.)
          CALL HBOOK1(103,'Longitudinal Zeta (electrons)',100,0.,100.,
     &      0.)
          CALL HBOOK1(104,'Etrans/Ecluster (electrons)',50,-0.05,1.05,
     &      0.)
          CALL HBOOK1(105,'Ecore/Ecluster(electrons)',50,-0.05,1.05,0.)
          CALL HBOOK1(106,'(Eisolation-Ecore)/Ecore (electrons)',
     &      50,-0.05,1.05,0.)
          CALL HBOOK1(110,'Et of electron clusters',60,0.,120.,0.)
          CALL HBOOK2(111,'Et(electron) vs Eta(electron)',40,0.,120.,
     &                                           40,-4.,4.,0.)
          CALL HBOOK1(112,'E of electron clusters',100,0.,200.,0.)
          CALL HBOOK2(113,'E(electron) vs Eta(electron)',100,0.,200.,
     &                                           40,-4.,4.,0.)
          CALL HBOOK2(120,'Eta vs phi of electrons',40,-4.,4.,
     &                                             32,0.,6.4,0.)
          CALL HBOOK2(121,'CALETA vs phi of electrons',80,-40.,40.,
     &                                             32,0.,6.4,0.)

          CALL HBOOK2(150,'Et(electron) vs Et(PNUT)',40,0.,120.,
     &                                           40,0.,120.,0.)

          CALL HBOOK1(151,'Mt  of electron clusters',60,0.,120.,0.)
C
C :::   PHOTONS...
C
          CALL HBOOK1(200,'No. of photon clusters',60,0.,12.,0.)
          CALL HBOOK1(203,'Longitudinal Zeta (photons)',100,0.,100.,
     &      0.)
          CALL HBOOK1(204,'Etrans/Ecluster (photons)',50,-0.05,1.05,
     &      0.)
          CALL HBOOK1(205,'Ecore/Ecluster(photons)',50,-0.05,1.05,0.)
          CALL HBOOK1(206,'(Eisolation-Ecore)/Ecore (photons)',
     &      50,-0.05,1.05,0.)
          CALL HBOOK1(210,'Et  of photon clusters',60,0.,120.,0.)
          CALL HBOOK2(211,'Et(photon) vs Eta(photon)',40,0.,120.,
     &                                           40,-4.,4.,0.)
          CALL HBOOK1(212,'E of photon clusters',100,0.,200.,0.)
          CALL HBOOK2(213,'E(photon) vs Eta(photon)',100,0.,200.,
     &                                           40,-4.,4.,0.)
          CALL HBOOK2(220,'Eta vs phi of photons',40,-4.,4.,
     &                                             32,0.,6.4,0.)
          CALL HBOOK2(221,'CALETA vs phi of electrons',80,-40.,40.,
     &                                             32,0.,6.4,0.)
C        CALL HBOOK1(611,'E depth 1/E cluster',50,0.,0.5,0.)
C        CALL HBOOK1(612,'E depth 2/E cluster',50,0.,0.5,0.)
C        CALL HBOOK1(613,'E depth 3/E cluster',50,0.,1.0,0.)
C        CALL HBOOK1(614,'E depth 4/E cluster',50,0.,1.0,0.)
C        CALL HBOOK1(615,'E depth 5/E cluster',50,0.,0.5,0.)
C        CALL HBOOK1(621,'E trans 1/E cluster',50,0.,0.25,0.)
C        CALL HBOOK1(622,'E trans 2/E cluster',50,0.,0.25,0.)
C        CALL HBOOK1(623,'E trans 3/E cluster',50,0.,0.50,0.)
C        CALL HBOOK1(624,'E trans 4/E cluster',50,0.,0.50,0.)
C        CALL HBOOK1(625,'E trans 5/E cluster',50,0.,0.25,0.)
C          CALL HBOOK1(630,'Pizeroness',50,0.,5.,0.)
        ENDIF
      ENDIF
C
      IF(DO_ANALYSIS .OR. VERIFY) THEN
C
C ****  Missing energy...
C
        LPNUT = GZPNUT(2)
        IF(LPNUT.GT.0) THEN
          ET_MISS = Q(LPNUT+7)
          PHI_MISS = Q(LPNUT+10)
        END IF
C
C ****  Electrons first...
C
        NELEC = 0
        LPELC = GZPELC()                  ! Top of linear structure
        DO WHILE ( LPELC .NE. 0)          ! Loop on electrons
          NELEC = NELEC + 1                 ! Number of electrons
          LHMTE = LQ(LPELC-IZHMTE)
          LCACL = LQ(LPELC-2)               ! Link to associated CACL bank
          LZTRK = LQ(LCACL-6)               ! Link to associated ZTRAK bank
C
          ECLUS    = Q(LPELC+6)
          ET_ELEC  = Q(LPELC+7)
          ETA_ELEC = Q(LPELC+9)
          PHI_ELEC = Q(LPELC+10)
          ETRANS   = Q(LPELC+14)
          ECORE    = Q(LPELC+15)
          EISOLATION=Q(LPELC+16)
          NZTRAKS  = Q(LPELC+21)
          DCLA     = Q(LPELC+22)
          IF(LHMTE.GT.0) THEN
            IF(IQ(LHMTE+2) .GE. 1) THEN
              ZETA = Q(LHMTE+5)
              CALL HFILL(103,ZETA,YDUM,1.)
            ENDIF
          ENDIF
C
          IF(LCACL.GT.0) THEN
            LCACH = LQ(LCACL-1)
            IF(LCACH.GT.0) THEN
              NCELL = IQ(LCACH+2)
              CENTRAL_TOWER = IQ(LCACH+NCELL+3)
              IETA = IQ(LCATE + (CENTRAL_TOWER-1)*14 + 12)
              CALL HFILL(121,FLOAT(IETA),PHI_ELEC,1.)
            ENDIF
          ENDIF
C
          IF(ET_ELEC .GT. 20 .AND. ET_MISS .GT. 20) THEN
            FACTOR = 1 - COS(PHI_ELEC - PHI_MISS)
            MT_ELEC = SQRT(2*ET_MISS*ET_ELEC*FACTOR)
            CALL HFILL(151,MT_ELEC,YDUM,1.)
          ENDIF
C
          CALL HFILL(101,DCLA,YDUM,1.)
          CALL HFILL(102,NZTRAKS,YDUM,1.)
          CALL HFILL(104,ETRANS/ECLUS,YDUM,1.)
          CALL HFILL(105,ECORE/ECLUS,YDUM,1.)
          IF(ECORE.NE.0)CALL HFILL(106,(ECORE-EISOLATION)/ECORE,YDUM,1.)
          CALL HFILL(110,ET_ELEC,YDUM,1.)
          CALL HFILL(111,ET_ELEC,ETA_ELEC,1.)
          CALL HFILL(112,ECLUS,YDUM,1.)
          CALL HFILL(113,ECLUS,ETA_ELEC,1.)
          CALL HFILL(120,ETA_ELEC,PHI_ELEC,1.)
          CALL HFILL(150,ET_ELEC,ET_MISS,1.)
C
          IF (DO_CPHANL1) CALL CPHANL1      ! User analysis routine
C
          LPELC = LQ(LPELC)
        ENDDO
C
        CALL HFILL(100,FLOAT(NELEC),YDUM,1.)
C
C ****  Now photons...
C
        NPHOT = 0
        LPPHO = GZPPHO()                  ! Top of linear structure
        DO WHILE (LPPHO .NE. 0)
          NPHOT = NPHOT + 1                 ! Number of photons
          LCACL = LQ(LPPHO-2)               ! Link to associated CACL bank
          LHMTP = LQ(LPPHO-IZHMTP)
          IF(LHMTP.GT.0) THEN
            IF(IQ(LHMTP+2) .GE. 1) THEN
              ZETA = Q(LHMTP+5)
              CALL HFILL(203,ZETA,YDUM,1.)
            ENDIF
          ENDIF
          IF(LCACL.GT.0) THEN
            LCACH = LQ(LCACL-1)
            IF(LCACH.GT.0) THEN
              NCELL = IQ(LCACH+2)
              CENTRAL_TOWER = IQ(LCACH+NCELL+3)
              IETA = IQ(LCATE + (CENTRAL_TOWER-1)*14 + 12)
              CALL HFILL(221,FLOAT(IETA),PHI_ELEC,1.)
            ENDIF
          ENDIF
C
          ET_PHOT  = Q(LPPHO+7)
          ETA_PHOT = Q(LPPHO+9)
          PHI_PHOT = Q(LPPHO+10)
          ECLUS=Q(LPPHO+6)
          ETRANS=Q(LPPHO+14)
          ECORE=Q(LPPHO+15)
          EISOLATION=Q(LPPHO+16)
C
          CALL HFILL(210,ET_PHOT,YDUM,1.)
          CALL HFILL(211,ET_PHOT,ETA_PHOT,1.)
          CALL HFILL(212,ECLUS,ETA_PHOT,1.)
          CALL HFILL(213,ECLUS,ETA_PHOT,1.)
          CALL HFILL(220,ETA_PHOT,PHI_PHOT,1.)
          IF(ECLUS.NE.0.)THEN
            CALL HFILL(204,ETRANS/ECLUS,YDUM,1.)
            CALL HFILL(205,ECORE/ECLUS,YDUM,1.)
            IF(ECORE.NE.0)
     &        CALL HFILL(206,(ECORE-EISOLATION)/ECORE,YDUM,1.)
C        DO I=1,5
C          CALL HFILL(210+I,ENDPTH(I)/ECLUS,YDUM,1.)
C          CALL HFILL(220+I,(ENDPTH(I)-EMAX(I))/ECLUS,YDUM,1.)
C        ENDDO
C          PZN=(ENDPTH(3)-EMAX(3))*ENDPTH(2)
C          PZN=100.*PZN/(ECLUS**2)
C          CALL HFILL(230,PZN,YDUM,1.)
          ENDIF
C
          IF (DO_CPHANL2) CALL CPHANL2      ! User analysis routine
C
          LPPHO = LQ(LPPHO)
        ENDDO
C
        CALL HFILL(200,FLOAT(NPHOT),YDUM,1.)
      ENDIF
C
  999 RETURN
      END
