
      SUBROUTINE TOP_FIT_GET_EVENT(MORE,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GET EVENT QUANTITIES
C-                         FROM SPECIAL RCP FILE
C-
C-   Inputs  :
C-   Outputs :MORE IF TRUE, PROCESS THIS COMBINATION. IF FALSE, NO MORE
C-   COMBINATIONS LEFT
C-   IER NON ZERO, DO NOT USE EVENT
C-   Controls:
C-
C-   Created   10-JAN-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:KINE_FIT.INC'
      INCLUDE 'D0$INC:EVENT_QUAN1.INC'
      INCLUDE 'D0$INC:EVENT_QUAN_2C.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INTEGER IER
      LOGICAL MORE
      LOGICAL READ_RCP
C
      LOGICAL first
      SAVE first
      DATA first / .true. /

C
      CHARACTER*32 JET_ALG_NAME
      INTEGER NCHR
      LOGICAL JET
C
      REAL JET_ET_CUT
      REAL JET_TEMPLATE(3)
C
      INTEGER IND_EL,IND_MU
      INTEGER I,J
      INTEGER GZPNUT
      INTEGER IPNUT
      INTEGER MIN_JET_NUM,MIN_EL_NUM,MIN_MU_NUM
      INTEGER LTO
      LOGICAL JET_ADD
C
      REAL PMAX_MUON
      LOGICAL MONTE_CARLO_DATA
C
      INTEGER ISER
      DATA ISER/0/
      SAVE ISER
      CHARACTER*20 RCP
C
      LOGICAL EZERR
      REAL    BMASS_S
      REAL    LEPTON1_S(4),BJET_LEPTON_S(4),BJET_HADRON_S(4)
      REAL    JET1_W_S(4),JET2_W_S(4),NEUTRINO_S(2)
C
      INTEGER NTOT_COMBS
      INTEGER NCMBMX
      PARAMETER( NCMBMX = 120 )
      REAL    JETS(7,4,NCMBMX),ISR(NCMBMX),FSR(NCMBMX)
      SAVE JETS,ISR,FSR,NTOT_COMBS
C----------------------------------------------------------------------
      IER = 0
      IF( first ) THEN
        first = .false.
        CALL EZPICK('TOP_FIT_RCP')
        CALL EZGET('READ_EVENT_FROM_RCP',READ_RCP,IER)
        CALL EZGET('JET_ET_CUT',JET_ET_CUT,IER)
        CALL EZ_GET_CHARS('JET_ALGORITHM_NAME',NCHR,
     &    JET_ALG_NAME,IER)
        CALL EZGET('JET_TEMPLATE',JET_TEMPLATE,IER)
        CALL GET_JETS_SELECT(JET_ALG_NAME,JET_TEMPLATE)
        CALL EZGET('LEPTON_TYPE',LEPTON_TYPE,IER)  !This is obtained
                                                   ! from TOP_FIT_rcp as default
                                                   ! and used when data is zebra.
                                                   ! overridden when data is rcp
        CALL EZGET('MIN_JET_NUM',MIN_JET_NUM,IER)
        CALL EZGET('MIN_EL_NUM',MIN_EL_NUM,IER)
        CALL EZGET('MIN_MU_NUM',MIN_MU_NUM,IER)
        CALL EZGET('ADD_JETS',JET_ADD,IER)
        CALL EZGET('MAXIMUM_MUON_MOMENTUM',PMAX_MUON,IER)
        CALL EZGET('BQUARK_MASS',BMASS_S,IER)
        BMASS = BMASS_S
        CALL EZRSET
C
        CALL EZCOPY('TOP_FIT_RCP','LJTOP_HMATRIX_RCP'
     &    ,LTO,0,IER) !FOR USE IN GET_W_OBJECTS
        IF ( READ_RCP ) THEN
          CALL INRCP('TOP_EVENT_RCP',IER)       ! read in RCP file
          IF ( IER.NE.0 ) THEN
            CALL ERRMSG('TOP_FIT','TOP_FIT_GET_EVENT',
     &        ' CANNOT READ IN RCP FILE ','W')
            GO TO 999
          ENDIF
        ENDIF
C
      ENDIF
C
      COMBNUM = COMBNUM + 1
      IF ( COMBNUM.EQ.1 ) THEN
C
C ****  read new event . Load buffers
C
        IF ( READ_RCP ) THEN
          ISER = ISER + 1
          WRITE(RCP,1)ISER
    1     FORMAT('TOP_EVENT_',I4.4)
          CALL EZPICK(RCP)
          IF ( EZERR(IER) ) THEN
C rcp file not there.
            RETURN
          ENDIF
          CALL EZGET('RUN',RUNC,IER)
          CALL EZGET('EVENT',EVENTC,IER)
          CALL EZGET('LEPTON1',LEPTON1_S,IER)
          CALL EZGET('BJET_LEPTON',BJET_LEPTON_S,IER)
          CALL EZGET('BJET_HADRON',BJET_HADRON_S,IER)
          CALL EZGET('JET1_W',JET1_W_S,IER)
          CALL EZGET('JET2_W',JET2_W_S,IER)
          CALL EZGET('PNUT',NEUTRINO_S,IER)
          CALL EZGET('LEPTON_TYPE',LEPTON_TYPE,IER)  !THIS TELLS IT IF IT
                                                     ! IS AN EE OR EMU EVENT
C
          NJETS = 4
          CALL UCOPYSD(LEPTON1_S,LEPTON1,4)
          CALL UCOPYSD(BJET_LEPTON_S,BJET_LEPTON,4)
          CALL UCOPYSD(BJET_HADRON_S,BJET_HADRON,4)
          CALL UCOPYSD(JET1_W_S,JET1_W,4)
          CALL UCOPYSD(JET2_W_S,JET2_W,4)
          CALL UCOPYSD(NEUTRINO_S,NEUTRINO,2)
C
          CALL GET_ET_ETA_PHI(LEPTON1)
          CALL GET_ET_ETA_PHI(BJET_LEPTON)
          CALL GET_ET_ETA_PHI(BJET_HADRON)
          CALL GET_ET_ETA_PHI(JET1_W)
          CALL GET_ET_ETA_PHI(JET2_W)
          CALL UCOPYDS(BJET_LEPTON,P25_JETS(1,1),7)
          CALL UCOPYDS(BJET_HADRON,P25_JETS(1,2),7)
          CALL UCOPYDS(JET1_W,P25_JETS(1,3),7)
          CALL UCOPYDS(JET2_W,P25_JETS(1,4),7)
          CALL EZRSET
C
          IF (LHEAD.EQ.0  ) THEN
            CALL BKHEAD  !BOOK HEAD BEANK
          ENDIF
          IQ(LHEAD+6)= RUNC
          IQ(LHEAD+9)= EVENTC
        ELSE
C
C ****  load buffers from ZEBCOM
C
C
          RUNC = IQ(LHEAD+6)
          EVENTC=IQ(LHEAD+9)
          CALL GET_W_OBJECTS(MAX_LEPHOT,NE,P24_ELECTRON,NELEC,
     &      NP,P18_PHOTON,NPHO,
     &      NM,P23_MUON,NMUO,
     &      ELECTRON,PHOTON,MUON)       ! GET FROM RECO
C
C
          IF ( LEPTON_TYPE.EQ.1 ) THEN
            IF ( NELEC.LT.MIN_EL_NUM ) THEN
              IER = 2
              RETURN
            ELSE
              CALL UCOPYSD(P24_ELECTRON,LEPTON1,7)
            ENDIF
          ELSEIF ( LEPTON_TYPE.EQ.2 ) THEN
            IF ( NMUO.LT.MIN_MU_NUM ) THEN
              IER = 3
              RETURN
            ELSE
              CALL UCOPYSD(P23_MUON,LEPTON1,7)
            ENDIF
          ENDIF
C
          CALL GET_JETS(NJETS_MAX,JET_ET_CUT,NJ,P25_JETS,NJETS,JET)
C
          IF ( NJETS.LT.MIN_JET_NUM ) THEN
            IER = 1
            RETURN
          ENDIF
C
          LPNUT = GZPNUT(IPNUT)               !GET THE FIRST IN CHAIN (LAST BOOKED)
C
          IF(LPNUT.EQ.0)THEN
            CALL ERRMSG('TOP_FIT','TOP_FIT_GET_EVENT',
     &        'NO PNUT VECTOR FOR THIS EVENT ','W')
            GO TO 999
          ENDIF
C
          CALL UCOPYSD(Q(LPNUT+3),NEUTRINO,2)
C
          DO I = 1 , NJETS
            DO J = 1 , 2
              NEUTRINO(J) = NEUTRINO(J) + P25_JETS(NJ-2+J,I)
C
C Correcting the neutrino ET with changes in Jets.
C This does not correct scalar ET. Nor does it correct for jets below threshold.
C
            ENDDO
          ENDDO
        ENDIF
C
        CALL TOP_FIT_COMBS(JETS,ISR,FSR,NTOT_COMBS)
C
      ENDIF
C
      IF ( COMBNUM.EQ.NTOT_COMBS ) THEN
        MORE = .FALSE.
      ELSE
        MORE = .TRUE.
      ENDIF
C
      CALL UCOPYSD(JETS(1,1,COMBNUM),BJET_LEPTON,7)
      CALL UCOPYSD(JETS(1,2,COMBNUM),BJET_HADRON,7)
      CALL UCOPYSD(JETS(1,3,COMBNUM),JET1_W,7)
      CALL UCOPYSD(JETS(1,4,COMBNUM),JET2_W,7)
      ISR_LIKE=ISR(COMBNUM)
      FSR_LIKE=FSR(COMBNUM)
C
      CALL MAKE_ON_SHELL(BJET_LEPTON,BMASS,BJET_LEPTON,1)
      CALL MAKE_ON_SHELL(BJET_HADRON,BMASS,BJET_HADRON,1)
C
  999 RETURN
      END
