      SUBROUTINE QCD_WRITE_FAKE_NTUPLE_CWN(JSMEAR,TOP_DIR2,ID2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : WRITE FAKE NTUPLE
C-
C-   Inputs  : JSMEAR = SMEAR NUMBER OF EVENT,ID2=NTUPLE ID
C-   Outputs :
C-   Controls:
C-
C-   Created  28-MAR-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER JSMEAR,ID2
      INCLUDE 'D0$INC:QCD_SMEAR_EVENT.INC'
      INCLUDE 'D0$INC:QCD_FAKE_NTUPLE_CWN.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER IER,I
      CHARACTER*(*) TOP_DIR2
C
      LOGICAL first
      SAVE first
      DATA first / .true. /
C
      REAL EVENTL
      SAVE EVENTL
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL EZPICK('QCD_FAKE_RCP')
        CALL EZGET('MET_NTUPLE_CUT',MET_CUT,IER)
        CALL EZGET('TRMS_NTUPLE_CUT',TRMS_CUT,IER)
        EVENTL=0
        CALL EZRSET
      ENDIF
      IF ( METC1_SMEAR.LT.MET_CUT ) THEN
        RETURN
      ENDIF
      IF ( WMTC_SMEAR.LT.TRMS_CUT ) THEN
        RETURN
      ENDIF
C
C      CALL DHDIR_DECLARE_FILE('FAKES')
C
C      CALL DHDIR('QCD_FAKE_RCP','HBOOK_DIRECTORY',IER,' ')
C         ! Create/Set HBOOK directory
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('QCD_FAKE','QCD_WRITE_NTUPLE',
     &      ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
C      CALL DO_HF1D(401,METC1_SMEAR,1.0)
C      CALL DO_HF1D(402,WMTC_SMEAR,1.0)
      DO I = 1 , NELE
C        CALL DO_HF1D(403,ELEC_SMEAR(5,I),1.0)
      ENDDO
C
      DO I = 1 , NJETS
C        CALL DO_HF1D(404,JETS_SMEAR(5,I),1.0)
      ENDDO
C
C      CALL DO_HF1D(405,REST_PT_SMEAR,1.0)
C      CALL DO_HF1(411,FLOAT(NJETS),1.0)
C      CALL DO_HF1(412,NPELC,1.0)
C
C      IF ( EVENT.LT.EVENTL ) THEN
C        CALL QCD_PLOT_DATA(500)  !NEW EVENT. PUT OUT DATA
C      ELSE
C        EVENTL = EVENT
C      ENDIF
C
C LOAD COMMON BLOCK.
C
C      CALL UCOPY(XDUMMY,XDUMMYS,NTUP_SIZE)   !STORE DATA AWAY
      EE1 = ELEC_SMEAR(4,1)
      ETE1 = ELEC_SMEAR(5,1)
      ETAE1 = ELEC_SMEAR(6,1)
      PHIE1 = ELEC_SMEAR(7,1)
C
C      EE2 = ELEC_SMEAR(4,2)
C      ETE2 = ELEC_SMEAR(5,2)
C      ETAE2 = ELEC_SMEAR(6,2)
C      PHIE2 = ELEC_SMEAR(7,2)
C
      RMETC1 = METC1_SMEAR
      RMETPHIC1 = ATAN2(NEUT_SMEAR(2),NEUT_SMEAR(1))
      IF ( RMETPHIC1.LT.0.0 ) THEN
        RMETPHIC1 = RMETPHIC1 + TWOPI
      ENDIF
C
      WMTC = WMTC_SMEAR
C
      UNJETS = NJETS
      RNPELC = NELE
      EJC51 = JET_SMEAR(1,1)
      ETAJC51 = JET_SMEAR(2,1)
      PHIJC51 = JET_SMEAR(3,1)
      EJC52 = JET_SMEAR(1,2)
      ETAJC52 = JET_SMEAR(2,2)
      PHIJC52 = JET_SMEAR(3,2)
      EJC53 = JET_SMEAR(1,3)
      ETAJC53 = JET_SMEAR(2,3)
      PHIJC53 = JET_SMEAR(3,3)
      EJC54 = JET_SMEAR(1,4)
      ETAJC54 = JET_SMEAR(2,4)
      PHIJC54 = JET_SMEAR(3,4)
      EJC55 = JET_SMEAR(1,5)
      ETAJC55 = JET_SMEAR(2,5)
      PHIJC55 = JET_SMEAR(3,5)
C
C      XDUMMY(NTUP_SIZE) = JSMEAR
C
C      CALL DO_HFN('FAKES',1000,XDUMMY)
C      CALL UCOPY(XDUMMYS,XDUMMY,NTUP_SIZE)   !RESTORE DATA
C
      CALL HCDIR(TOP_DIR2,' ')
      CALL HFNT(ID2)
  999 RETURN
      END
