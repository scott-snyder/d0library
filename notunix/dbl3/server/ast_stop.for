      SUBROUTINE AST_STOP (NUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Activated by a user through XX_STOP_CACHE
C-                         cache to stops the server
C-
C-   Inputs  :
C-   Outputs : NUM      Number associated with cache
C-   Controls:
C-
C-   Created   1-OCT-1991   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C      INCLUDE       'D0$INC:DB_EFS.INC'
C
      INCLUDE       'D0$PARAMS:INFOCACHE.PARAMS'
      INCLUDE       'D0$INC:DB_SRVR_NAMES.INC'
      INCLUDE       'D0$INC:DB_SRVR.INC'
      INCLUDE       'D0$INC:DB_SRVR_PARAM.INC'
      INCLUDE       'D0$INC:DRCCOM.INC'
      INCLUDE       'D0$INC:QUEST.INC'
C
      CHARACTER*13 CACHENAME
      CHARACTER*27 FILENAME
      CHARACTER*11 DIRNAME
      INTEGER       MODTRANS,IERR,NUM,NTRANSACT
      INTEGER       IQUEST_SAVE_1,IQUEST_SAVE_2
      PARAMETER    (MODTRANS=50)
      INTEGER*4    B_OPTION
C
      INTEGER*4    ICACHE(4)
      BYTE         BCACHE(16)
      EQUIVALENCE  (BCACHE,ICACHE)
      INTEGER      STATUS
C
      CALL IC_NUM_ID(NUM,CACHENAME,' ',IERR)
      IF(IERR.NE.0)  THEN
        PRINT *,'Server- AST_STOP: error from IC_NUM_ID, cachename '
     +         ,cachename,' ',ierr
        GO TO 999
      ENDIF
      IF( CACHENAME(1:2).NE.DB_CURRENT_SNAME(1:2) ) THEN
        PRINT *,'Server- cache inconsistent with server'
        PRINT *,'      - cachename, dbname ',cachename
     +         ,' ',db_current_sname
        GO TO 999
      ENDIF
      CALL IC_DELIVER(CACHENAME,ICACHE,' ',IERR)
      IF(IERR.NE.0) THEN
        PRINT *,'Server- error in delivery from cache '
     +         ,cachename,' ',ierr
        GO TO 999
      ENDIF
      IF(ICACHE(3) .NE. -9) THEN
        PRINT *,'Server- error in delivery during STOP from cache '
     +         ,cachename,' ',ierr
      ELSE
        IQUEST(100) = ICACHE(3)
      ENDIF
C
      CALL IC_SEND(CACHENAME,ICACHE,' ',IERR)
C
      CALL LIB$DATE_TIME (DB_DATETIME)
      PRINT *,'Server- Stop requested at ', db_datetime
  999 RETURN
      END
