      LOGICAL FUNCTION MUGT_EVT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Steering routine for muon reconstruction 
C-                         package, MURECO in event phase.
C-
C-   Returned value  : 
C-   Inputs  : (none)
C-   Outputs : (none)
C-   Controls: (none)
C-
C-      CREATED FROM MURECO_EVT DEC-31-1990    SHAHRIAR ABACHI
C-
C-   Created   8-OCT-1989   Shuichi Kunori
C-   Modified 
C-   17-MAY-1990   S.Kunori   
C-       1) change to CALL MUANLZ from function call.
C-       2) return status is always .TRUE.
C-   12-JUN-1990   S.Kunori   
C-       1) add MULINK,MUFITS,MUPMUO
C-       2) add ERRMSG
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      INTEGER IERR
      CHARACTER*32 MESSID,CALLER
      CHARACTER*80 MESSAG
      LOGICAL MURECO_HST
      EXTERNAL MURECO_HST
C
      MUGT_EVT=.TRUE.
C
C  Set RCP bank to MUON_RCP for now.   (This shoud be MURECO_RCP.)
C  =================================
C
      CALL EZPICK('MURECO_RCP')
C
C     -- Link muon tracks between the Muon system and the Central 
C        tracker.
C
      CALL MULINK(IERR)
      IF(IERR.NE.0) THEN
         MESSID='Error in MULINK'
         CALLER='MUGT_EVT'
         WRITE(MESSAG,61) IERR
         CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
         GO TO 800
      ENDIF
C
C     -- Global fit... 
C
      CALL MUFITS(IERR)
      IF(IERR.NE.0) THEN
         MESSID='Error in MUFITS'
         CALLER='MUGT_EVT'
         WRITE(MESSAG,61) IERR
         CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
         GO TO 800
      ENDIF
C
C     -- Certify muon tracks...              
C
      CALL MUPMUO(IERR)
      IF(IERR.NE.0) THEN
         MESSID='Error in MUPMUO'
         CALLER='MUGT_EVT'
         WRITE(MESSAG,61) IERR
         CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
         GO TO 800
      ENDIF
C
  800 CONTINUE
C
C     -- Histograms...
C
      IF(.NOT. MURECO_HST()) THEN
         MESSID='Error in MURECO_HST'
         CALLER='MUGT_EVT'
         MESSAG='This should not happen.  Check MURECO_HST.'
         CALL ERRMSG(MESSID,CALLER,MESSAG,'W')
         CALL EZRSET
         GO TO 999
      ENDIF
C
C  Reset RCP bank.
C  ===============
C
      CALL EZRSET
C
   61 FORMAT('Error code=',I10,' ')
  999 RETURN
      END
