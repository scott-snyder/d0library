      LOGICAL FUNCTION MDST_INIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize Micro DST generation
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  24-SEP-1991   Andrew J. Milder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*40 FILE_OUT
      INTEGER IER
      LOGICAL OK,CJTINI,DO_TRGR
C----------------------------------------------------------------------
C
      MDST_INIT = .TRUE.
      CALL EVSTRM_STOR('QCD',3)
      CALL FLGSET('WRITE_STREAM_QCD',.TRUE.)
C
      CALL INRCP('MDST_RCP',IER)
      IF (IER. NE. 0) THEN
        CALL ERRMSG('MDST','MDST_INIT',
     &        ' NO MDST_RCP','F')
      ENDIF
      CALL EZPICK('MDST_RCP')
      CALL EZERR(IER)
      IF (IER .EQ. 0) THEN
        CALL EZGET('DO_TRGR',DO_TRGR,IER)
      ENDIF
C
C ****  open the file for the output
C
      WRITE(6,12)
   12 FORMAT('  WHAT IS THE NAME OF THE OUTPUT FILE?')
      READ(5,13) FILE_OUT
   13 FORMAT(A80)
      CALL EVOPWO('QCD',FILE_OUT,'N',OK)
C
C ****  SETUP DST OUTPUT FLAG
C
      IF ( OK ) THEN
        WRITE(6,14)
   14   FORMAT('  OUTPUT FILE OPENED OK')
      ELSE
        WRITE(6,16)
   16   FORMAT('  OUTPUT FILE PROBLEMS! STOPPING PROGRAM')
        STOP
      ENDIF

      CALL FLGSET('WRITE_STREAM_QCD',.TRUE.)
C
C ****  FLAG THE BANKS TO BE DROPPED IN THE QCD STREAM
C
      IF (.NOT.DO_TRGR) THEN
        CALL EVDROP('QCD','TRGR')
      ENDIF
      CALL EVDROP('QCD','MUD1')
      CALL EVDROP('QCD','CDD1')
      CALL EVDROP('QCD','CDD2')
      CALL EVDROP('QCD','CDD3')
      CALL EVDROP('QCD','CDD4')
      CALL EVDROP('QCD','CAD1')
      CALL EVDROP('QCD','CAD2')
      CALL EVDROP('QCD','FILT')
      CALL EVDROP('QCD','RECO')
      CALL EVDROP('QCD','HSUM')
      CALL EVDROP('QCD','FAKE')
      CALL EVDROP('QCD','GEAN')
      CALL EVDROP('QCD','ISAE')
      CALL EVDROP('QCD','HSTR')
      CALL EVDROP('QCD','FLTR')
C
  999 RETURN
      END
