      SUBROUTINE ERRMSG_SETUP (RCP_BANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To initialize the ERRMSG utility. Fetches 
C-                         from RCP_BANK maximum number of warnings, 
C-                         messages etc. to be logged  and opens a file 
C-                         specified in RCP to write into.
C-
C-   Inputs  : RCP_BANK
C-   Outputs : none
C-   Controls: RCP_BANK
C-
C-   Created  29-JUL-1990   Chip Stewart
C-   Updated  11-Mar-1992   Herbert Greenlee
C-      UNIX compatible version
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCP_BANK
      INTEGER I,J,K,L,II,JJ,KK,LL
      INTEGER IER,ERRLOG_UNIT,ERRSUM_UNIT,MAXLOG,MAXWRN
      LOGICAL EZERR,ERRWARN,OK,SUM
      CHARACTER*80 ERRLOG_FILE,ERRSUM_FILE,COMMAND
C----------------------------------------------------------------------
      CALL EZPICK(RCP_BANK)
      IF(EZERR(IER)) THEN
        CALL ERRMSG('NORCPBNK','ERRMSG_SETUP',
     &      ' RCP bank NOT found','W')
        MAXLOG = 20
        MAXWRN = 3
        ERRWARN = .TRUE.
        CALL GTUNIT(77,ERRLOG_UNIT,IER)
        ERRLOG_FILE = 'USR$OUT:ERRMSG.LOG'
        CALL D0OPEN (ERRLOG_UNIT,ERRLOG_FILE,'OF',OK)
      ELSE
        CALL EZGET_i('ERRMSG_MAXLOG',MAXLOG,IER)
        CALL EZGET_i('ERRMSG_MAXWARN',MAXWRN,IER)
        CALL EZGET_l('ERRMSG_WARN_SWITCH',ERRWARN,IER)
        CALL EZ_FILE_OPEN(77,'ERRMSG_LOG_FILE','OF',ERRLOG_UNIT,
     &      ERRLOG_FILE,IER)
      END IF
      CALL EZRSET
      CALL ERRINI(ERRLOG_UNIT,ERRWARN)
      CALL ERRMAX(' ',MAXLOG,MAXWRN)
  999 RETURN
      ENTRY ERRMSG_END(RCP_BANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Opens summary file for ERRMSG summary.
C-
C-   Created  29-JUL-1990   Chip Stewart
C-
C----------------------------------------------------------------------
      CALL EZPICK(RCP_BANK)
      IF(EZERR(IER)) THEN
        CALL ERRMSG('NORCPBNK','ERRMSG_END',
     &      ' RCP bank NOT found','W')
        CALL GTUNIT(77,ERRSUM_UNIT,IER)
        ERRSUM_FILE = 'USR$OUT:ERRMSG.LOG'
        CALL D0OPEN (ERRSUM_UNIT,ERRSUM_FILE,'OF',OK)
      ELSE
        CALL EZ_FILE_OPEN(77,'ERRMSG_SUMMARY_FILE','OF',ERRSUM_UNIT,
     &      ERRSUM_FILE,IER)
        SUM = (IER.EQ.0) .AND. .NOT.(INDEX(ERRSUM_FILE,'NO').GT.0) 
      END IF
      CALL EZRSET
      IF(SUM) THEN
        CALL ERRSUM(ERRSUM_UNIT)
        CLOSE (UNIT = ERRLOG_UNIT)
        CLOSE (UNIT = ERRSUM_UNIT)
        CALL SWORDS(ERRLOG_FILE,I,J,K)
C&IF VAXVMS
        IF(ERRSUM_FILE(I:J).EQ.ERRLOG_FILE(I:J)) THEN
          CALL SWORDS(ERRSUM_FILE,I,J,L)
          COMMAND = 'APPEND  '//ERRSUM_FILE(I:J)//';-1 '
     &    //ERRSUM_FILE(I:J) 
          CALL SWORDS(COMMAND,II,JJ,L)
          CALL LIB$SPAWN(COMMAND(II:JJ))
          COMMAND = 'DELETE/NOCONF  '//ERRSUM_FILE(I:J)//';-1 '
          CALL SWORDS(COMMAND,II,JJ,L)
          CALL LIB$SPAWN(COMMAND(II:JJ))
        END IF
C&ELSE
C&ENDIF
      END IF
 1999 RETURN
      END
