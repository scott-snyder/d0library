      SUBROUTINE TOP_MASS_GET_EVENT(MORE,IER)
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
C-   Created   7-APR-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:KINEQ.INC'
      INCLUDE 'D0$INC:TOP_SOLNSE.INC'
      INCLUDE 'D0$INC:EVENT_QUAN1.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:BTAG_ISAJ.INC'
C
      REAL LEPTON1S(4),LEPTON2S(4),JET1S(4),JET2S(4),JET3S(4),PNUTS(2)
      INTEGER IER
      LOGICAL MORE
      LOGICAL READ_RCP
C
      LOGICAL first
      SAVE first
      DATA first / .true. /

      REAL    JET3_DUMMY(4), JET3_DUMMYS(4),JETA(4)
      DATA JET3_DUMMYS/3*.05,0.1/  !DUMMY JET3 FOR SMEARING SAKE
C
      CHARACTER*32 JET_ALG_NAME
      INTEGER NCHR
      LOGICAL JET
C
      REAL    JET_ET_CUT
      REAL    JET_TEMPLATE(3)
C
      INTEGER IND_EL,IND_MU
      INTEGER I,J
      INTEGER GZPNUT
      INTEGER IPNUT
      INTEGER MIN_JET_NUM,MIN_EL_NUM,MIN_MU_NUM
      INTEGER LTO
      LOGICAL JET_ADD
C
      REAL    PMAX_MUON
      LOGICAL MONTE_CARLO_DATA
C
      INTEGER LISAE,LISAQ,LISAL,LISAJ
      INTEGER GZISAL,GZISAQ,GZISAJ
      EQUIVALENCE (LISAE,CSTLNK(LNKMX)),(LISAQ,CSTLNK(LNKMX-1))
      EQUIVALENCE (LISAL,CSTLNK(LNKMX-2)),(LISAJ,CSTLNK(LNKMX-3))
      INTEGER LISAQ_BT,LISAQ_BB
      EQUIVALENCE (LISAQ_BT,CSTLNK(LNKMX-4))
      EQUIVALENCE (LISAQ_BB,CSTLNK(LNKMX-5))
      INTEGER LISAL_BT,LISAL_BB,LISAQ_REF
      EQUIVALENCE (LISAL_BT,CSTLNK(LNKMX-6))
      EQUIVALENCE (LISAL_BB,CSTLNK(LNKMX-7))
      INTEGER LISAL_REF
      EQUIVALENCE (LISAL_REF,CSTLNK(LNKMX-8))
C
      INTEGER IPISAJ,IPISAQ
      REAL    MASS_ISAJ,MASS_ISAQ,NEUTRINO1(4),NEUTRINO2(4)
C
      INTEGER NCNFGE_SAVE
      SAVE NCNFGE_SAVE
      LOGICAL BPART
      INTEGER IGOT
C----------------------------------------------------------------------
      IER = 0
      IF( first ) THEN
        first = .false.
        CALL EZPICK('TOP_MASS_RCP')
        CALL EZGET('READ_EVENT_FROM_RCP',READ_RCP,IER)
        CALL EZGET('JET_ET_CUT',JET_ET_CUT,IER)
        CALL EZ_GET_CHARS('JET_ALGORITHM_NAME',NCHR,
     &    JET_ALG_NAME,IER)
        CALL EZGET('JET_TEMPLATE',JET_TEMPLATE,IER)
        CALL GET_JETS_SELECT(JET_ALG_NAME,JET_TEMPLATE)
        CALL EZGET('LEPTON_TYPE',LEPTON_TYPE,IER)  !This is obtained
                                                   ! from top_mass_rcp as default
                                                   ! and used when data is zebra.
                                                   ! overridden when data is rcp
        CALL EZGET('MIN_JET_NUM',MIN_JET_NUM,IER)
        CALL EZGET('MIN_EL_NUM',MIN_EL_NUM,IER)
        CALL EZGET('MIN_MU_NUM',MIN_MU_NUM,IER)
        CALL EZGET('ADD_JETS',JET_ADD,IER)
        CALL EZGET('MAXIMUM_MUON_MOMENTUM',PMAX_MUON,IER)
        NCNFGE_SAVE = NCNFGE
        CALL EZRSET
C
        CALL EZCOPY('TOP_MASS_RCP','LJTOP_HMATRIX_RCP'
     &    ,LTO,0,IER) !FOR USE IN GET_W_OBJECTS
      ENDIF
C
      IF ( READ_RCP ) THEN
        CALL INRCP('TOP_EVENT_RCP',IER)       ! read in RCP file
        IF ( IER.NE.0 ) THEN
          CALL ERRMSG('TOP_MASS','TOP_MASS_GET_EVENT',
     &      ' CANNOT READ IN RCP FILE ','W')
          GO TO 999
        ENDIF
      ENDIF
C
      COMBNUM = COMBNUM + 1.0
      IF ( COMBNUM.EQ.1.0 ) THEN
C
C ****  read new event . Load buffers
C
        NCNFGE = NCNFGE_SAVE
        IF ( READ_RCP ) THEN
          CALL EZPICK('TOP_EVENT_RCP')
          CALL EZGET('RUN',RUNC,IER)
          CALL EZGET('EVENT',EVENTC,IER)
          CALL EZGET('LEPTON1',LEPTON1S,IER)
          CALL EZGET('LEPTON2',LEPTON2S,IER)
          CALL EZGET('JET1',JET1S,IER)
          CALL EZGET('JET2',JET2S,IER)
          CALL EZGET('JET3',JET3S,IER)
          CALL EZGET('PNUT',PNUTS,IER)
          CALL EZGET('LEPTON_TYPE',LEPTON_TYPE,IER)  !THIS TELLS IT IF IT
                                                     ! IS AN EE OR EMU EVENT
          CALL EZRSET
          IF ( JET3S(4).LT.1.0 ) THEN
            NJETS=2  !Dummy 3rd jet
          ENDIF
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
          IF ( NELEC.LT.MIN_EL_NUM ) THEN
            IER = 2
            RETURN
          ELSEIF ( NMUO.LT.MIN_MU_NUM ) THEN
            IER = 3
            RETURN
          ENDIF
C
          IND_EL =1
          IND_MU = 1
          IF ( LEPTON_TYPE(1).EQ.1 ) THEN

            CALL UCOPY(P24_ELECTRON(1,IND_EL),LEPTON1S,4)
C electron needed
            CALL UCOPY(P24_ELECTRON(6,IND_EL),ETA_PHI_LEPTON(1,1),2)
            IND_EL = IND_EL + 1
          ELSE
C muon needed
            CALL UCOPY(P23_MUON(1,IND_MU),LEPTON1S,4)
            CALL UCOPY(P23_MUON(6,IND_MU),ETA_PHI_LEPTON(1,1),2)
            IND_MU = IND_MU + 1
          ENDIF
C
          IF ( LEPTON_TYPE(2).EQ.1 ) THEN

            CALL UCOPY(P24_ELECTRON(1,IND_EL),LEPTON2S,4)
C electron needed
            CALL UCOPY(P24_ELECTRON(6,IND_EL),ETA_PHI_LEPTON(1,2),2)
            IND_EL = IND_EL + 1
          ELSE
C muon needed
            CALL UCOPY(P23_MUON(1,IND_MU),LEPTON2S,4)
            CALL UCOPY(P23_MUON(6,IND_MU),ETA_PHI_LEPTON(1,2),2)
            IND_MU = IND_MU + 1
          ENDIF
C
          CALL GET_JETS(NJETS_MAX,JET_ET_CUT,NJ,P25_JETS,NJETS,JET)
C
          CALL UZERO(JET1S,1,4)
          CALL UZERO(JET2S,1,4)
          CALL UZERO(JET3S,1,4)
          CALL UZERO(JET3_DUMMY,1,4)
C
          IF ( NJETS.LT.MIN_JET_NUM ) THEN
            IER = 1
            RETURN
          ELSEIF ( NJETS.EQ.2 ) THEN
            CALL UCOPY(P25_JETS(1,1),JET1S,4)
            CALL UCOPY(P25_JETS(1,2),JET2S,4)
            CALL UCOPY(JET3_DUMMYS,JET3S,4)  !DUMMY JET3
          ELSEIF ( NJETS.EQ.3 ) THEN
            CALL UCOPY(P25_JETS(1,1),JET1S,4)
            CALL UCOPY(P25_JETS(1,2),JET2S,4)
            CALL UCOPY(P25_JETS(1,3),JET3S,4)
          ELSEIF ( NJETS.GT.3 ) THEN
            CALL UCOPY(P25_JETS(1,1),JET1S,4)
            CALL UCOPY(P25_JETS(1,2),JET2S,4)
            CALL UCOPY(P25_JETS(1,3),JET3S,4)
            DO I = 3 , NJETS
C
C ****  THIS IS TO PROVIDE A BACKGOUND EVENT FLUCTUATION WHEN COMBINING
C ****  JETS 1 + 3 FOR EXAMPLE.
C
              DO J = 1 , 4
                JET3_DUMMY(J) = JET3_DUMMY(J) + P25_JETS(J,I)
              ENDDO
            ENDDO
          ENDIF
C
          LPNUT = GZPNUT(IPNUT)               !GET THE FIRST IN CHAIN (LAST BOOKED)
C
          IF(LPNUT.EQ.0)THEN
            CALL ERRMSG('HMATRIX','LJTOP_HMATRIX_FILL_QUAN',
     &        'NO PNUT VECTOR FOR THIS EVENT ','W')
            GO TO 999
          ENDIF
C
          CALL UCOPY(Q(LPNUT+3),P2_NEUT,2)
C
          DO I = 1 , NJETS
            DO J = 1 , 2
              P2_NEUT(J) = P2_NEUT(J) + P25_JETS(NJ-2+J,I)
C
C Correcting the neutrino ET with changes in Jets.
C This does not correct scalar ET. Nor does it correct for jets below threshold.
C
            ENDDO
          ENDDO
          CALL UCOPY(P2_NEUT,PNUTS,2)
        ENDIF
C
        CALL UCOPYSD(LEPTON1S,LEPTON1,4)  !DO THIS ONLY ONCE
        CALL UCOPYSD(LEPTON2S,LEPTON2,4)  !DITTO
        CALL UCOPYSD(PNUTS,PNUT,4)        !DITTO
C
        CALL UCOPYSD(JET1S,JET1,4)
        CALL UCOPYSD(JET2S,JET2,4)
        CALL UCOPYSD(JET3S,JET3,4)
C
        IF ( JET3_DUMMY(4).EQ.0.0 ) THEN
          CALL UCOPY(JET3_DUMMYS,JET3_DUMMY,4)
        ENDIF
C
        MORE = .TRUE.
      ELSEIF ( COMBNUM.EQ.2.0 ) THEN
C JET 1 =1 JET 3= 2 JET2 =3
        CALL UCOPYSD(JET1S,JET1,4)
        CALL UCOPYSD(JET3S,JET2,4)
        CALL UCOPYSD(JET2S,JET3,4)
      ELSEIF ( COMBNUM.EQ.3.0 ) THEN
C JET 2 =1 JET 3= 2 JET1 =3
        CALL UCOPYSD(JET2S,JET1,4)
        CALL UCOPYSD(JET3S,JET2,4)
        CALL UCOPYSD(JET1S,JET3,4)
        IF(.NOT.JET_ADD)THEN
          MORE = .FALSE.    !CUTTING OFF AFTER 3. NO ADDING JETS
        ENDIF
      ELSEIF ( COMBNUM.EQ.4.0 ) THEN
C JET1+JET2 =1 JET3 = 2 DUMMY =3
        CALL ADD_JETS(JET1S,JET2S,JETA)
        CALL UCOPYSD(JETA,JET1,4)
        CALL UCOPYSD(JET3S,JET2,4)
        CALL UCOPYSD(JET3_DUMMY,JET3,4)
      ELSEIF ( COMBNUM.EQ.5.0 ) THEN
C JET1+JET3 =1 JET2 = 2 DUMMY =3
        CALL ADD_JETS(JET1S,JET3S,JETA)
        CALL UCOPYSD(JETA,JET1,4)
        CALL UCOPYSD(JET2S,JET2,4)
        CALL UCOPYSD(JET3_DUMMY,JET3,4)
      ELSEIF ( COMBNUM.EQ.6.0 ) THEN
C JET2+JET3 =1 JET1 = 2 DUMMY =3
        CALL ADD_JETS(JET2S,JET3S,JETA)
        CALL UCOPYSD(JETA,JET1,4)
        CALL UCOPYSD(JET1S,JET2,4)
        CALL UCOPYSD(JET3_DUMMY,JET3,4)
        IF(.NOT.MONTE_CARLO_DATA()) MORE = .FALSE.
      ELSEIF ( COMBNUM.EQ.7.0 ) THEN
C
C ****  feed in MC ISAJET unsmeared quantities
C
        CALL UZEROD(JET1,1,4)
        CALL UZEROD(JET2,1,4)
        CALL UZEROD(JET3,1,4)
        CALL UZEROD(LEPTON1,1,4)
        CALL UZEROD(LEPTON2,1,4)
        IFSRA = 0
        IFSRT = 0   !FINAL STATE RADIATION FROM TOP AND ANTI-TOP
        IGOT = 0
C
        IF ( NCNFGE_SAVE.NE.0 ) THEN
          NCNFGE = 1   !ONLY DO THIS CONFIGURATION
        ELSE
          NCNFGE=0   !RETURN
        ENDIF
C
        LISAQ = GZISAQ()
        DO WHILE (LISAQ.NE.0)
          MASS_ISAQ = Q(LISAQ+6)
          IPISAQ = IQ(LISAQ+1)
          LISAJ = LQ(LISAQ-1)  !REFERENCE LINK
          IF ( LISAJ.NE.0 ) THEN
C THERE IS A PARENT ISAJ BANK
            IPISAJ = IQ(LISAJ+1)
            IF ( IPISAJ.EQ.6 ) THEN
C THIS MEANS THIS IS THE DECAY PRODUCT OF TOP QUARK. I.E. A B
              IF ( BPART(IPISAQ,5) ) THEN
                CALL UCOPYSD(Q(LISAQ+2),JET1,4)
                IGOT = IGOT + 1
              ELSE
                IFSRT = IFSRT + 1
                CALL UCOPYSD(Q(LISAQ+2),TOP_FSR(1,IFSRT),4)
              ENDIF
            ELSEIF ( IPISAJ.EQ.-6 ) THEN
C THIS MEANS THIS IS THE DECAY PRODUCT OF ANTI-TOP QUARK. I.E. A B-BAR
              IF ( BPART(IPISAQ,-5) ) THEN
                CALL UCOPYSD(Q(LISAQ+2),JET2,4)
                IGOT = IGOT + 1
              ELSE
                IFSRA = IFSRA + 1
                CALL UCOPYSD(Q(LISAQ+2),ATOP_FSR(1,IFSRA),4)
              ENDIF
            ELSEIF ( IPISAJ.EQ.80 ) THEN
C THIS MEANS THIS IS A DECAY PRODUCT OF W+ (EITHER A +CHARGED LEPTON OR NEUTRINO)
              IF ( MASS_ISAQ.EQ.0.0 ) THEN
                CALL UCOPY(Q(LISAQ+2),NEUTRINO1,4)
                IGOT = IGOT + 1
              ELSE
                CALL UCOPYSD(Q(LISAQ+2),LEPTON1,4)
                IGOT = IGOT + 1
                IF ( IABS(IPISAQ).EQ.12 ) THEN
                  LEPTON_TYPE(1) = 1  !ELECTRON
                ELSEIF ( IABS(IPISAQ).EQ.14 ) THEN
                  LEPTON_TYPE(1) = 2  !MUON
                ENDIF
              ENDIF
            ELSEIF ( IPISAJ.EQ.-80 ) THEN
C THIS MEANS THIS IS A DECAY PRODUCT OF W- (EITHER A -CHARGED LEPTON OR NEUTRINO)
              IF ( MASS_ISAQ.EQ.0.0 ) THEN
                CALL UCOPY(Q(LISAQ+2),NEUTRINO2,4)
                IGOT = IGOT + 1
              ELSE
                CALL UCOPYSD(Q(LISAQ+2),LEPTON2,4)
                IGOT = IGOT + 1
                IF ( IABS(IPISAQ).EQ.12 ) THEN
                  LEPTON_TYPE(2) = 1  !ELECTRON
                ELSEIF ( IABS(IPISAQ).EQ.14 ) THEN
                  LEPTON_TYPE(2) = 2  !MUON
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          LISAQ=LQ(LISAQ)
        ENDDO
C
        DO I = 1 , 2
          P2_NEUT(I) = NEUTRINO1(I)+NEUTRINO2(I)
        ENDDO
C
        CALL UCOPYSD(JET3_DUMMYS,JET3,4)  !DUMMY JET3
C
        MORE =.FALSE.
C
        IF ( IGOT.NE.6 ) THEN
          IER = 4  !PROBABLY NOT TTBAR
        ENDIF
      ENDIF
C
C ****  SAVE KINEMATIC QUANTITIES
C
      CALL SET_MAX_MOMENTUM(LEPTON1,PMAX_MUON)
      CALL SET_MAX_MOMENTUM(LEPTON2,PMAX_MUON)
C
      CALL UCOPYDD(LEPTON1,LEPTON1_S,4)
      CALL UCOPYDD(LEPTON2,LEPTON2_S,4)
      CALL UCOPYDD(JET1,JET1_S,4)
      CALL UCOPYDD(JET2,JET2_S,4)
      CALL UCOPYDD(JET3,JET3_S,4)
      CALL UCOPYDD(PNUT,PNUT_S,2)
      CALL UCOPY(LEPTON_TYPE,LEPTON_TYPE_S,2)
C
  999 RETURN
      END
