      SUBROUTINE QCD_FAKE_ANAL(JSMEAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ANALYZE QCD FAKE
C-
C-   Inputs  :JSMEAR = SMEAR NUMBER
C-   Outputs :
C-   Controls:
C-
C-   Created  26-MAR-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QCD_FAKE_NTUPLE.INC'
      INCLUDE 'D0$INC:QCD_SMEAR_EVENT.INC'
      INTEGER I
      INTEGER IER
      INTEGER JSMEAR
      LOGICAL first
      SAVE first
      DATA first / .true. /
C----------------------------------------------------------------------
      CALL DHDIR_DECLARE_FILE('FAKES')
C
      CALL DHDIR('QCD_FAKE_RCP','HBOOK_DIRECTORY',IER,' ')
C         ! Create/Set HBOOK directory
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('QCD_FAKE','QCD_FAKE_ANAL',
     &      ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
      IF( first ) THEN
        first = .false.
        CALL EZPICK('QCD_FAKE_RCP')
        CALL DO_HBOOK('QCD_FAKE_HISTS')
        CALL EZRSET
        CALL DHSHOW
        CALL DHSHOWRZ
      ENDIF
C
      IF ( JSMEAR.EQ.1 ) THEN
C MAKE DATA HISTOGRAMS
        CALL DO_HF1(101,METC1,1.0) !CORRECTED MISSING ET
        CALL DO_HF1(102,WMTC,1.0)  !CORRECTED TRANSVERSE MASS
        DO I = 1 , NELE
          CALL DO_HF1D(103,ELEC(5,I),1.0)
        ENDDO
C
        DO I = 1 , NJETS
          CALL DO_HF1D(104,JETS(5,I),1.0)
        ENDDO
        CALL DO_HF1(111,FLOAT(NJETS),1.0)
        CALL DO_HF1(112,NPELC,1.0)
C
        CALL DO_HF1D(105,REST_PT,1.0)
        IF ( BAD_ELE.EQ.0 ) THEN
          CALL DO_HF1D(602,ZMASS,1.0)  !ZMASS OF EVENTS SMEARED
        ENDIF
      ENDIF
C
      CALL DO_HF1D(201,METC1_SMEAR,1.0)
      CALL DO_HF1D(202,WMTC_SMEAR,1.0)
      DO I = 1 , NELE
        CALL DO_HF1D(203,ELEC_SMEAR(5,I),1.0)
      ENDDO
C
      DO I = 1 , NJETS
        CALL DO_HF1D(204,JETS_SMEAR(5,I),1.0)
      ENDDO
C
      CALL DO_HF1D(205,REST_PT_SMEAR,1.0)
C
  999 RETURN
      END
