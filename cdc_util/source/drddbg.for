      SUBROUTINE DRDDBG(OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : read debug control information into a common
C-                         block and dump all CDC control parameters if
C-                         requested
C-
C-   Inputs  : 
C-   Outputs : 
C-             OK = error status (true: succesful, false: failure)
C-            /DDEBUG/ are filled
C-   Controls: 
C-
C-   Created  29-JUN-1989   Qizhong Li-Demarteau
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:DDEBUG.INC'
      LOGICAL OK
      LOGICAL EZERROR
      INTEGER ERR, IER
C----------------------------------------------------------------------
C
C access debug control parameters from SRCP bank 
C
      CALL EZPICK('DTRAKS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('DTRAKS','DRDDBG',
     &    'Unable to find bank DTRAKS_RCP','W')
        GOTO 999
      ENDIF
      CALL EZGET('LUNDBG',LUNDBG,ERR)
      IF (ERR .EQ. -2) 
     &   CALL INTMSG('  *****   Bank DTRAKS_RCP does not exist')
      OK = ERR .EQ. 0
      CALL EZGET('NEVDBG',NEVDBG,ERR)
      OK = (ERR .EQ. 0) .AND. OK
      CALL EZGET('LVLDBG(1)',LVLDBG(1),ERR)
      OK = (ERR .EQ. 0) .AND. OK
C
C dump the SRCP bank if needed
C
      IF ((LVLDBG(8) .NE. 0) .AND. OK) THEN
        CALL EZDUMP(LUNDBG,0,0)
        CALL INTMSG(' CDC run control parameters in SRCP bank are
     & dumpped on debug file')
      ENDIF
C
      CALL EZRSET
  999 RETURN
      END
