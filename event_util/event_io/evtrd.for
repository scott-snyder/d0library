      SUBROUTINE EVTRD(INUNIT,IOS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-           Read event file checking for bad records. Call DMPRAW
C-           for event records.
C-
C-   Inputs  : 
C-   INUNIT = unit number for event file
C-   Outputs : 
C-   IOS = IO status
C-
C-   Created   9-AUG-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER INUNIT,IOS,NEVT,EVTCNT
C----------------------------------------------------------------------
C
    1 CALL EVTIN(INUNIT,IOS)
      IF(IOS.LT.0.AND.IOS.GT.-8) GOTO 1    ! bad read
C
      IF ( IOS.LT.-8 ) 
     & CALL ERRMSG('INPUT FILE','EVTRD',
     & 'More than 4 consecutive errors in input file','F')
C
      IF(IOS.EQ.0) THEN
        NEVT=EVTCNT()+1
        CALL EVTSET(NEVT)
        CALL DMPRAW
      ENDIF
C
  999 RETURN
      END
