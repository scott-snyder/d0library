      SUBROUTINE QCD_WRITE_FAKE_NTUPLE(JSMEAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : WRITE FAKE NTUPLE
C-
C-   Inputs  : JSMEAR = SMEAR NUMBER OF EVENT
C-   Outputs :
C-   Controls:
C-
C-   Created  28-MAR-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER JSMEAR
      INCLUDE 'D0$INC:QCD_SMEAR_EVENT.INC'
      INCLUDE 'D0$INC:QCD_FAKE_NTUPLE.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER IER,I
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
      CALL DHDIR_DECLARE_FILE('FAKES')
C
      CALL DHDIR('QCD_FAKE_RCP','HBOOK_DIRECTORY',IER,' ')
C         ! Create/Set HBOOK directory
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('QCD_FAKE','QCD_WRITE_NTUPLE',
     &      ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      CALL DO_HF1D(401,METC1_SMEAR,1.0)
      CALL DO_HF1D(402,WMTC_SMEAR,1.0)
      DO I = 1 , NELE
        CALL DO_HF1D(403,ELEC_SMEAR(5,I),1.0)
      ENDDO
C
      DO I = 1 , NJETS
        CALL DO_HF1D(404,JETS_SMEAR(5,I),1.0)
      ENDDO
C
      CALL DO_HF1D(405,REST_PT_SMEAR,1.0)
      CALL DO_HF1(411,FLOAT(NJETS),1.0)
      CALL DO_HF1(412,NPELC,1.0)
C
      IF ( EVENT.LT.EVENTL ) THEN
        CALL QCD_PLOT_DATA(500)  !NEW EVENT. PUT OUT DATA
      ELSE
        EVENTL = EVENT
      ENDIF
C
C LOAD COMMON BLOCK.
C
      CALL UCOPY(XDUMMY,XDUMMYS,NTUP_SIZE)   !STORE DATA AWAY
      EE1 = ELEC_SMEAR(4,1)
      ETE1 = ELEC_SMEAR(5,1)
      ETAE1 = ELEC_SMEAR(6,1)
      PHIE1 = ELEC_SMEAR(7,1)
C
      EE2 = ELEC_SMEAR(4,2)
      ETE2 = ELEC_SMEAR(5,2)
      ETAE2 = ELEC_SMEAR(6,2)
      PHIE2 = ELEC_SMEAR(7,2)
C
      METC1 = METC1_SMEAR
      METPHIC1 = ATAN2(NEUT_SMEAR(2),NEUT_SMEAR(1))
      IF ( METPHIC1.LT.0.0 ) THEN
        METPHIC1 = METPHIC1 + TWOPI
      ENDIF
C
      WMTC = WMTC_SMEAR
C
      EJ51 = JET_SMEAR(1,1)
      ETAJ51 = JET_SMEAR(2,1)
      PHIJ51 = JET_SMEAR(3,1)
      EJ52 = JET_SMEAR(1,2)
      ETAJ52 = JET_SMEAR(2,2)
      PHIJ52 = JET_SMEAR(3,2)
      EJ53 = JET_SMEAR(1,3)
      ETAJ53 = JET_SMEAR(2,3)
      PHIJ53 = JET_SMEAR(3,3)
      EJ54 = JET_SMEAR(1,4)
      ETAJ54 = JET_SMEAR(2,4)
      PHIJ54 = JET_SMEAR(3,4)
      EJ55 = JET_SMEAR(1,5)
      ETAJ55 = JET_SMEAR(2,5)
      PHIJ55 = JET_SMEAR(3,5)
C
      XDUMMY(NTUP_SIZE) = JSMEAR
C
      CALL DO_HFN('FAKES',1000,XDUMMY)
      CALL UCOPY(XDUMMYS,XDUMMY,NTUP_SIZE)   !RESTORE DATA
C
  999 RETURN
      END
