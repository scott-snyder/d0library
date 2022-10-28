      PROGRAM QCD_FAKE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fluctuate low mising ET QCD from ELF stream
C-                         to get high missing ET QCD
C-
C-   Updated  24-MAR-1994   Rajendran Raja
C-   Updated  23-MAR-2004   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,J,K,STATUS,ID,NEVENTS,NID
      INTEGER MAXDIM, MAXID,USERID
      LOGICAL EOF
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:TOP_FIT_SMEAR.INC'
      INCLUDE 'D0$INC:QCD_SMEAR_EVENT.INC'
      INCLUDE 'D0$INC:QCD_FAKE_NTUPLE_CWN.INC'
      REAL    JET_ET_CUT
      INTEGER IER,SSUNIT,NEV
      SAVE NEV
C-
      INTEGER IYN,LUN0,ID1,ID2,LUN,LREC,NLOOP,ISTAT,LUN1,ICYCLE
      CHARACTER*10 B_NAME1,B_NAME2
      CHARACTER*100 LIST_FILE,INFILE
      CHARACTER*1 CID
      REAL X
      LOGICAL FIRST/.TRUE./
      LOGICAL COLUMN_WISE
C
C ****  Histogram IDs
C
C----------------------------------------------------------------------
C
C ********************************************
C ****  Setup ZEBRA
C ********************************************
C
      CALL QCD_FAKE_INIT
C
C ********************************************
C ****  Read RCP file
C ********************************************
C
      CALL INRCP('QCD_FAKE_RCP',STATUS)
      IF ( STATUS .NE. 0 ) THEN
        STOP 'Cannot open QCD_FAKE_RCP'
      ENDIF
      CALL INRCP('TOP_FIT_RCP',STATUS)
      IF ( STATUS .NE. 0 ) THEN
        STOP 'Cannot open TOP_FIT_RCP'
      ENDIF
C
      CALL EZPICK('QCD_FAKE_RCP')
      CALL EZGET_i('NUMBER_OF_EVENTS',NEVENTS,STATUS)
      IF ( NEVENTS .LE. 0 ) THEN
        NEVENTS = 1000
      ENDIF
*
      CALL EZGET_l('COLUMN_WISE',COLUMN_WISE,IER)
*
      CALL EZGET('JET_ET_CUT',JET_ET_CUT,IER)
      CALL EZGET_rarr('ELECTRON_RESOLN',ELECTRON_RESOLN_S,IER)
      CALL EZGET_rarr('MUON_RESOLN',MUON_RESOLN_S,IER)
      CALL EZGET_rarr('JET_RESOLN',JET_RESOLN_S,IER)
      CALL EZGET_rarr('REST_RESOLN',REST_RESOLN_S,IER)
C
      CALL EZGET_rarr('ELEC_ETA_RESOLN',ELEC_ETA_RESOLN_S,IER)
      CALL EZGET_rarr('MUON_ETA_RESOLN',MUON_ETA_RESOLN_S,IER)
      CALL EZGET_rarr('JET_ETA_RESOLN',JET_ETA_RESOLN_S,IER)
C
      CALL EZGET_rarr('ELEC_PHI_RESOLN',ELEC_PHI_RESOLN_S,IER)
      CALL EZGET_rarr('MUON_PHI_RESOLN',MUON_PHI_RESOLN_S,IER)
      CALL EZGET_rarr('JET_PHI_RESOLN',JET_PHI_RESOLN_S,IER)
C
      CALL EZGET_i('NUMBER_OF_SMEARS',NSMEAR,IER)
      CALL EZGET_rarr('MET_SMEAR_RANGE',MET_SMEAR_RANGE,IER)
      CALL EZGET_l('ZERO_MISSING_ET',ZERO_MET,IER)
      CALL EZRSET
C
      CALL EZPICK('TOP_FIT_RCP')
      CALL EZSET('JET_ET_CUT',JET_ET_CUT,IER)
      CALL EZSET_rarr('ELECTRON_RESOLN',ELECTRON_RESOLN_S,IER)
      CALL EZSET_rarr('MUON_RESOLN',MUON_RESOLN_S,IER)
      CALL EZSET_rarr('JET_RESOLN',JET_RESOLN_S,IER)
      CALL EZSET_rarr('REST_RESOLN',REST_RESOLN_S,IER)
C
      CALL EZSET_rarr('ELEC_ETA_RESOLN',ELEC_ETA_RESOLN_S,IER)
      CALL EZSET_rarr('MUON_ETA_RESOLN',MUON_ETA_RESOLN_S,IER)
      CALL EZSET_rarr('JET_ETA_RESOLN',JET_ETA_RESOLN_S,IER)
C
      CALL EZSET_rarr('ELEC_PHI_RESOLN',ELEC_PHI_RESOLN_S,IER)
      CALL EZSET_rarr('MUON_PHI_RESOLN',MUON_PHI_RESOLN_S,IER)
      CALL EZSET_rarr('JET_PHI_RESOLN',JET_PHI_RESOLN_S,IER)
      CALL EZRSET
C
C ********************************************
C ****  Open HBOOK files
C ********************************************
C
      IF ( COLUMN_WISE ) THEN
        CALL DO_HBOOK_OPEN_CWN('HBOOK_OPEN',STATUS) !WRITE THIS ROUTINE
        IF ( STATUS .NE. 0 ) THEN
          STOP 'Cannot open HBOOK files'
        ENDIF
      ELSE
        CALL DO_HBOOK_OPEN('HBOOK_OPEN',STATUS)
        IF ( STATUS .NE. 0 ) THEN
          STOP 'Cannot open HBOOK files'
        ENDIF
      ENDIF
      CALL EZRSET
C
      NEV = 0
      DO I = 1 , NEVENTS
        CALL DHDIR_DECLARE_FILE('ELF')
        CALL DHSETDIR('//PAWC',STATUS)
        IF ( COLUMN_WISE ) THEN
          CALL QCD_READ_NTUPLE_CWN('ELF',11,I,EOF)
        ELSE
          CALL QCD_READ_NTUPLE('ELF',11,I,EOF)
        ENDIF
        IF ( EOF ) THEN
          GOTO 999
        ELSE
          NEV = NEV + 1
          CALL QCD_TEST_FOR_Z(IER)
          CALL QCD_PLOT_DATA(300)  !PLOT ALL OF DATA WITHOUT CUTS
          IF ( IER.EQ.0 ) THEN
            IF (RMETC1.GT.MET_SMEAR_RANGE(1).AND.RMETC1.LT.
     &        MET_SMEAR_RANGE(2)  ) THEN
              IF(ZERO_MET)CALL QCD_FAKE_ZERO_MET !SETS MET TO ZERO BEFORE SMEARING
              DO J = 1 , NSMEAR
                CALL QCD_SMEAR_EVENT
                CALL QCD_FAKE_ANAL(J)
                IF ( COLUMN_WISE ) THEN
                  CALL QCD_WRITE_FAKE_NTUPLE_CWN(J)
                ELSE
                  CALL QCD_WRITE_FAKE_NTUPLE(J)
                ENDIF
              ENDDO
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C
  999 CONTINUE
      WRITE(6,111)NEV
  111 FORMAT(' NUMBER OF EVENTS READ FROM NTUPLE = ',I7)
      CALL NTUPLE_CLOSE('FAKES',STATUS)
      CALL DO_HBOOK_CLOSE
  997 CONTINUE
      END
