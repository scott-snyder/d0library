      SUBROUTINE CAL_READ_NTUPLE(IEVENT,IOS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : READS NTUPLE FILE INTO COMMON/PAWID/
C-
C-   Inputs  : IEVENT = event number to read
C-   Outputs : IOS = 3 when EOF
C-   Controls: 
C-
C-   Created  14-APR-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NTMX
      PARAMETER( NTMX = 1000 )
      INTEGER IDNEVT,VIDN1,VIDN2,VIDN3,VIDN
      COMMON/PAWIDN/IDNEVT,VIDN1,VIDN2,VIDN3,VIDN(10),XDUMMY(NTMX)
      REAL    XDUMMY
C
      INTEGER NTUPLE_ID
      CHARACTER*32 NTUPLE_FILES(200)
      INTEGER NCHRS
      CHARACTER*32 TOP_DIR,FILE
      SAVE TOP_DIR,FILE
      INTEGER STATUS,IOS
C
      LOGICAL         CHAIN
      CHARACTER*128   CFILE
C
      INTEGER NCHEVT,ICHEVT
      COMMON /PAWCHN/ CHAIN, NCHEVT, ICHEVT
      COMMON /PAWCHC/ CFILE
C
      INTEGER IER,IEVENT
      INTEGER NTUPLE_PROC_EVENT,IEVENTN
      SAVE IEVENTN
C
      LOGICAL first
      SAVE first
      DATA first / .true. /
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL EZPICK('CALFRAME_RCP')
        CALL EZGET('NTUPLE_ID',NTUPLE_ID,IER)
        CALL EZ_GET_CHARS('NTUPLE_FILES',NCHRS,NTUPLE_FILES,IER)
        NTUPLE_PROC_EVENT = 0
        CALL EZGET('NTUPLE_PROC_EVENT',NTUPLE_PROC_EVENT,IER)
        FILE    = NTUPLE_FILES(1)
        TOP_DIR = NTUPLE_FILES(2)  !ASSUME 1ST LINE IS READ FILE
        CALL EZRSET
        IEVENTN = 0
      ENDIF
C
      CALL DHDIR_SAVE_FILE            !SAVE PREVIOUS TOPDIR
      CALL DHDIR_DECLARE_FILE(FILE)
      CALL DHSETDIR('//PAWC',STATUS)
C
      CALL DHDIR('CALFRAME_RCP','NTUPLE_DIRECTORY',IER,' ')
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('CALORIMETER','CAL_READ_NTUPLE',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
      IF ( NTUPLE_PROC_EVENT.GT.1 ) THEN
        IEVENTN = IEVENTN + NTUPLE_PROC_EVENT !PROCESS EVERY NTH EVENT
      ELSE
        IEVENTN = IEVENT
      ENDIF
      CALL NTUPLE_GET(TOP_DIR,NTUPLE_ID,IEVENTN,XDUMMY,STATUS)
      IF ( STATUS.NE.0 ) THEN
        IOS = 3  !EOF
      ENDIF
      IF ( LHEAD.EQ.0 ) THEN
        CALL BKHEAD    !For further booking along reco path
      ENDIF
      CALL DHDIR_RESTORE_FILE        !RESTORE PREVIOUS TOPDIR
  999 RETURN
      END
