      SUBROUTINE GET_W_OBJECTS(NMAX,ESIZ,P24_ELECTRON,NELEC,
     &  PSIZ,P18_PHOTON,NPHO,
     &  MSIZ,P25_MUON,NMUO,P25_MUON_TAGGED,NTAG,
     &  ELECTRON,PHOTON,MUON,TAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GET ELECTRON,PHOTON OR MUON
C-    FROM PROSPECTIVE W FROM  RECO PATH.
C-
C-   Inputs  :  NMAX = maximum number of objects in each category
C-              ESIZ = number of words buffered for each electron
C-              PSIZ = number of words buffered for each photon
C-              MSIZ = number of words buffered for each muon
C-
C-   Outputs :
C-              P24_ELECTRON(1..9)
C-                            PX, PY, PZ, E, ET, ETA, PHI, DET_ETA, QUALITY
C-                    I = 9
C-              P24_ELECTRON(I+1)    (SigEx)**2
C-              P24_ELECTRON(I+2)    (SigEy)**2
C-              P24_ELECTRON(I+3)    EM energy in cluster outside central tower
C-              P24_ELECTRON(I+4)    Total energy in core cone
C-              P24_ELECTRON(I+5)    Total energy in isolation cone
C-              P24_ELECTRON(I+6)    EM energy in core cone
C-              P24_ELECTRON(I+7)    EM energy in isolation cone
C-              P24_ELECTRON(I+8)    CHISQ
C-              P24_ELECTRON(I+9)    TRUNCATED CHISQ
C-              P24_ELECTRON(I+10)   Number of central tracks in cluster road
C-              P24_ELECTRON(I+11)   Distance of closest approach of  track
C-              P24_ELECTRON(I+12)   CDC MIP
C-              P24_ELECTRON(I+13)   FDC MIP
C-              P24_ELECTRON(I+14)   VTX MIP
C-              P24_ELECTRON(I+15)   TRD anode likelihood efficiency
C-
C-             NELEC = NUMBER OF ELECTRONS FOUND
C-
C-             P18_PHOTON = SAME AS P24_ELECTRON TILL TRUNCATED CHISQ
C-             NPHO = NUMBER OF PHOTONS FOUND
C-
C              P25_MUON(1)         px
C              P25_MUON(2)         py
C              P25_MUON(3)         pz
C              P25_MUON(4)         p
C              P25_MUON(5)         pt
C              P25_MUON(6)         eta
C              P25_MUON(7)         phi
C              P25_MUON(8)         eta_detector (eta for now)
C              P25_MUON(9)         quality flag
C              P25_MUON(10)         isolation_1
C              P25_MUON(11)         isolation_2
C              P25_MUON(12)         isolation_4
C              P25_MUON(13)         No of CD tracks
C              P25_MUON(14)         angle between muon and CD (degrees)
C              P25_MUON(15)         impact parameter
C              P25_MUON(16)         quality flag 2
C              P25_MUON(17)         IFW1
C              P25_MUON(18)         integral B.dl
C              P25_MUON(19)         ZTRAK_DPHI
C              P25_MUON(20)         ZTRAK_DTHETA
C              P25_MUON(21)         Calorimeter energy in tight cone
C              P25_MUON(22)         Hits on track
C              P25_MUON(23)         Fitted hits on track
C              P25_MUON(24)         Delta R to nearest jet
C              P25_MUON(25)         Dediff betweeen 2x2 neighborhood and .4 cone
C-             NMUO    = NUMBER OF MUONS FOUND
C-             P25_MUON_TAGGED     - same as P25_muon except
C-             P25_MUON_TAGGED(25)  ptrel to nearest jet
C-
C-             ELECTRON = .TRUE. IF ELECTRON WITH ET .GT. ET_CUT_EL
C-             PHOTON = .TRUE. IF PHOTON WITH ET.GT. ET_CUT_PH
C-             MUON = .TRUE. IF MUON WITH ET .GT. ET_CUT_MU.
C-             TAG  = .TRUE. IF BTAGGED MUON PRESENT
C-   Controls:
C-
C-   Created   5-JAN-1991   Rajendran Raja
C-   Updated  10-APR-1993   Rajendran Raja  Added object code
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMAX
C
      INTEGER ESIZ,PSIZ,MSIZ
C
      REAL    P24_ELECTRON(ESIZ,*),P18_PHOTON(PSIZ,*),P25_MUON(MSIZ,*)
      REAL    P25_MUON_TAGGED(MSIZ,*)
      INTEGER IER
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      REAL    ET_CUT_EL,ET_CUT_PH,ET_CUT_MU
      INTEGER LPMUO
      EQUIVALENCE (LPMUO,LBANK)
      LOGICAL ELECTRON,PHOTON,MUON,TAG
      INTEGER NELEC,NPHO,NMUO,NTAG
C
      INTEGER NSIZEL,NSIZEP,NSIZEM
      INTEGER I,IP
C
      CHARACTER*16 ELE_TYPE,PHO_TYPE,ELET,PHOT
C
      LOGICAL MONTE_CARLO_DATA
      INTEGER LEN3
      INTEGER NCHR
C----------------------------------------------------------------------
      ELECTRON = .FALSE.
      MUON = .FALSE.
      PHOTON = .FALSE.
      NELEC = 0
      NPHO = 0
      NMUO = 0
C
      IF ( FIRST  ) THEN
        FIRST = .FALSE.
        CALL EZPICK('LJTOP_HMATRIX_RCP')
        CALL EZGET('ET_CUT_W_ELECTRON',ET_CUT_EL,IER)
        CALL EZGET('ET_CUT_W_PHOTON',ET_CUT_PH,IER)
        CALL EZGET('ET_CUT_W_MUON',ET_CUT_MU,IER)
        CALL EZ_GET_CHARS('ELECTRON_TYPE',NCHR,ELET,IER)
        CALL EZ_GET_CHARS('PHOTON_TYPE',NCHR,PHOT,IER)
        CALL EZRSET
        IF ( MONTE_CARLO_DATA() ) THEN
C ADDING THE APPROPRIATE TAG
          CALL ADDSTR('MCE_',ELET,ELE_TYPE,LEN3)
          CALL ADDSTR('MCG_',PHOT,PHO_TYPE,LEN3)
        ELSE
          CALL ADDSTR('ELE_',ELET,ELE_TYPE,LEN3)
          CALL ADDSTR('PHO_',PHOT,PHO_TYPE,LEN3)
        ENDIF
      ENDIF
C
C ****  get electrons
C
      CALL NOBJ_ELECTRONS1(ELE_TYPE,NELEC,NSIZEL)
      IF ( NSIZEL.GT.ESIZ ) THEN
        CALL ERRMSG('CALORIMETER','GET_W_OBJECTS',
     &    ' electron allocation too small in GET_W_OBJECTS','W')
      ENDIF
C
      NELEC = MIN(NELEC,NMAX)
      IP = 0
      DO I = 1 , NELEC
        IP = IP + 1
        CALL OBJECT_ELECTRON1(ELE_TYPE,I,ESIZ,P24_ELECTRON(1,IP))
        IF ( P24_ELECTRON(5,IP).LT.ET_CUT_EL ) THEN
        IP = IP - 1   !ELECTRON BELOW ET CUT
        ENDIF
      ENDDO
      NELEC = IP
      IF ( NELEC.GT.0 ) ELECTRON=.TRUE.
C
C ****  get photons
C
      CALL NOBJ_PHOTONS1(PHO_TYPE,NPHO,NSIZEP)
      IF ( NSIZEP.GT.PSIZ ) THEN
        CALL ERRMSG('CALORIMETER','GET_W_OBJECTS',
     &    ' photon allocation too small in GET_W_OBJECTS','W')
      ENDIF
C
      NPHO = MIN(NPHO,NMAX)
      IP = 0
      DO I = 1 , NPHO
        IP = IP + 1
        CALL OBJECT_PHOTON1(PHO_TYPE,I,PSIZ,P18_PHOTON(1,IP))
        IF ( P18_PHOTON(5,IP).LT.ET_CUT_PH ) THEN
          IP = IP -1   !PHOTON BELOW ET CUT
        ENDIF
      ENDDO
      NPHO = IP
      IF ( NPHO.GT.0 ) PHOTON=.TRUE.
C
      CALL NOBJ_MUONS1('ISOLMUON',NMUO,NSIZEM)
      IF ( NSIZEM.GT.MSIZ ) THEN
        CALL ERRMSG('CALORIMETER','GET_W_OBJECTS',
     &    ' muon allocation too small in GET_W_OBJECTS','W')
      ENDIF
C
      NMUO = MIN(NMUO,NMAX)
      IP = 0
      DO I = 1 , NMUO
        IP = IP + 1
        CALL OBJECT_MUON1('ISOLMUON',I,MSIZ,P25_MUON(1,IP))
        IF ( P25_MUON(5,IP).LT.ET_CUT_MU ) THEN
          IP = IP -1   !MUON BELOW ET CUT
        ENDIF
      ENDDO
      NMUO = IP
      IF ( NMUO.GT.0 ) MUON=.TRUE.
C
      CALL NOBJ_MUONS1('BTAGMUON',NTAG,NSIZEM)
      IF ( NSIZEM.GT.MSIZ ) THEN
        CALL ERRMSG('CALORIMETER','GET_W_OBJECTS',
     &    ' muon allocation too small in GET_W_OBJECTS','W')
      ENDIF
C
      NTAG = MIN(NTAG,NMAX)
      IP = 0
      DO I = 1 , NTAG
        IP = IP + 1
        CALL OBJECT_MUON1('BTAGMUON',I,MSIZ,P25_MUON_TAGGED(1,IP))
      ENDDO
      NTAG = IP
      IF ( NTAG.GT.0 ) TAG=.TRUE.
C
  999 RETURN
      END
