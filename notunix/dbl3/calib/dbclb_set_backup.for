      SUBROUTINE DBCLB_SET_BACKUP(TOPNAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To setup FZ file for special backup
C-
C-   Inputs  :    TOPNAME  ; Top directory name
C-
C-   Outputs :
C-   Controls:
C-
C-   Created  25-FEB-1991   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TOPNAME
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:DBSTP.INC'
      CHARACTER*40 BNAME
      INTEGER IOS, ERR
C
      IF(OPTB) THEN
        CALL INTMSG( ' Already one backup file open. Please close.')
        GOTO 999
      ENDIF
C
      IF(.NOT. CALL_DBEND) THEN
        CALL INTMSG( ' Database is not initialized. Procedure aborted.')
        GOTO 999
      ENDIF
C
      CALL GETPAR(1, ' Enter backup file name >  ', 'C', BNAME)
      IF(BNAME .EQ. ' ') THEN
        CALL INTMSG(' No file name selected. Procedure aborted')
        GOTO 999
      ENDIF
C
      CALL GTUNIT(15,BUNIT,ERR)
        OPEN(UNIT=BUNIT, STATUS='NEW', FILE=BNAME,
     &      FORM='UNFORMATTED',IOSTAT=IOS)
      IF(IOS .NE. 0) THEN
        IQUEST(1)= IOS
        CALL ERRDB('ERROR OPENING FILE')
        GOTO 999
      ENDIF
C
      CALL FZFILE(BUNIT,0,'O')
C
      CALL DBFZOP(BUNIT, TOPNAME, ' ')
      IF(IQUEST(1) .NE. 0) THEN
        CALL ERRDB('ERROR OPENING BACKUP FILE')
        GOTO 998
      ENDIF
C
      OPTB = .TRUE.
C
      GOTO 999
C----------------------------------------------------------------------
  998 CONTINUE
      CLOSE(BUNIT)
      CALL RLUNIT(15,BUNIT,ERR)
C
  999 RETURN
      END
