      SUBROUTINE SCAN_SAVE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Asks questions for event scanning, writes
C-                         SCAN.RCP for each event
C-   Returned value  : .TRUE. if all OK
C-   Inputs  :
C-   Outputs :
C-   Controls: None
C-
C-   Created  07-AUG-1992   Andrew Brandt
C-   Modified 15-DEC-1992   Nobuaki Oshima
C-     Use SCAN_SAVE_INFO to get Saving/Replacing answers from SACN_DO.
C-   Modified 22-DEC-1992   Vipin Bhatnagar
C-     Call to EZPICK to activate PX_SCANNING_RCP
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$LINKS:IZZFIT.LINK'
      INCLUDE 'D0$LINKS:IZZTMP.LINK'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER IER, GZPROC, LENGTH, I, IDS
      PARAMETER(IDS=2)
      INTEGER IZLINK
C
      CHARACTER*30 SETIME
C
C Variables for DATIME routine
C
      INTEGER ID, IH
      CHARACTER*30 TIME
C
C----------------------------------------------------------------------
      INTEGER NQUES
      INTEGER NPROM,ALLOWQ
      PARAMETER ( ALLOWQ=55 )
      CHARACTER*40 PROMPT(ALLOWQ)
      CHARACTER*20 OUTSTR(ALLOWQ)
      CHARACTER*10 QUES(ALLOWQ)
      LOGICAL EZERROR
C
C----------------------------------------------------------------------
C-
C--Get RCP file
C-
      CALL EZPICK('PX_SCANNING_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','SCAN_SAVE',
     &        'Can NOT find PX_SCANNING_RCP','W')
        GOTO 999
      ENDIF
      CALL EZGETS('SCAN_ETIME',1,SETIME,LENGTH,IER)
      CALL EZ_GET_CHARS('QUESTION_LIST',NQUES,QUES,IER)
      NPROM=NQUES
C
C------------------------------------------------
C--- Get Saving/Replacing answers from SACN_DO
C-
      CALL SCAN_SAVE_INFO(QUES,NPROM,PROMPT,OUTSTR)
C-
C Time of end of scanning of event
C
      CALL DATIME(ID,IH)
      CALL PXITOC(IH,4,TIME)
      CALL EZSETS('SCAN_ETIME',1,TIME,30,IER)
C
C Replace answers in RCP file (Answer IDS=2)
C
      DO I=1,NPROM
        CALL EZSETS(QUES(I),IDS,OUTSTR(I),20,IER)
      END DO
C
C Write RCP file to zebcom under PROC
C
      CALL BKPROC(LPROC)
      IF(LPROC.EQ.0) THEN
        CALL ERRMSG('SCANNING','SCAN_SAVE', 'PROC BANK NOT SET UP','W')
        RETURN
      ENDIF
      IZLINK=8
      CALL EZMOVE_ZEBCOM ('PX_SCANNING_RCP',LPROC,IZLINK)
C
C- Reset the RCP
C
      CALL EZRSET
C
  999 RETURN
      END
