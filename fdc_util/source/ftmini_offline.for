      SUBROUTINE FTMINI_OFFLINE(CRUN,DB_OPENED,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialzation routine for offline times
C-                         in logical format.
C-                         Called by FDBINI.
C-
C-   Inputs  : CRUN = Current Run Number (I6 format)
C-             DB_OPENED = .TRUE. if DBCLB_INITIALIZE has already been called.
C-   Outputs : DB_OPENED = .TRUE. if DBCLB_INITIALIZE was called.
C-   Controls: IOK = .TRUE. if everything goes well.
C-
C-   Created  25-AUG-1992   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$LINKS:IZFTMH.LINK'
C
C Input:
C
      INTEGER CRUN
      LOGICAL IOK,DB_OPENED
C
C Local variables:
C
      INTEGER LENGTH,IER
      INTEGER LINKH
      INTEGER LORUN,HIRUN
      INTEGER NMPARW 
      INTEGER LDATA,LKEY
C
      CHARACTER*25 PATH
      CHARACTER*80 DBCALIB
C
      LOGICAL FIRST
C
      SAVE FIRST,LORUN,HIRUN,DBCALIB
      DATA FIRST /.TRUE./
      DATA LORUN/-1/
      DATA HIRUN/-1/
C
C----------------------------------------------------------------------
C
      IOK = .TRUE.
      IF (FIRST) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGETS('DBCALIB$FDC',1,DBCALIB,LENGTH,IER)
        CALL EZRSET
        CALL INTMSG(' Reading FDC Offline Time-to-Distance banks.')
        FIRST = .FALSE.
      ENDIF
C
C Check to see if time calibration has changed since last run. 
C If so, read new logical tree structure.
C
      IF (CRUN.GE.LORUN .AND.CRUN.LT.HIRUN ) THEN
C Doesn't need updating.
        GO TO 999
      ELSE
        IF (.NOT.DB_OPENED) THEN
          CALL DBCLB_INITIALIZE(DBCALIB,'S',IOK)
          IF (.NOT.IOK) THEN
            CALL ERRDB(' FDBINI - Error in DBL3 intialization ')
            GO TO 999
          END IF
          DB_OPENED = .TRUE.
        END IF
        CALL DBCLB_PATH('PROC_TIMES','FDC',PATH)
      END IF
C
C  Get time banks.
C
      LDATA = 0
      CALL DBCLB_FETCH_OFFLINE(PATH,CRUN,0,LDATA,LKEY)
      IF ( (LDATA.LE.0) .OR. (LKEY.LE.0) ) THEN
        CALL ERRDB(' FTMINI_OFFLINE - Error in finding requested data ')
        IOK = .FALSE.
        GO TO 999
      ENDIF
      LORUN = IC(LKEY+3)      ! Start Validity
      HIRUN = IC(LKEY+4)      ! End validity
C
      LFTMH = LC(LSFDC-IZFTMH)
      IF ( LFTMH.NE.0 ) THEN
        CALL MZDROP(IXSTP,LFTMH ,' ')
      ENDIF
C
      CALL ZSHUNT(IXSTP,LDATA,LSFDC,-IZFTMH,0)
      LFTMH = LC(LSFDC-IZFTMH)
C----------------------------------------------------------------------
  999 RETURN
      END
