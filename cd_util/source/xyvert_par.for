      LOGICAL FUNCTION XYVERT_PAR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : run initialization for XYVERT package
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  22-OCT-1992   Alexandre Zinchenko
C-   Updated  08-MAR-1993   A. Zinchenko - add "SEPARATE RUNS" and
C-                          time information
C-   Updated  25-FEB-1994   Liang-Ping Chen move CALL VTRPAR to 
C-                                          VTX_DYNADJ, similar to ZTRPAR
C-   Updated   7-MAR-1994   Susan Blessing  Reverse order of setting memory
C-    and RZ directory before booking.
C-   Updated  20-MAR-2004   sss - use idate2k instead of idate.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL IDOVTX, OK
      LOGICAL EZERROR, FIRST, SEPARATE_RUNS
      INTEGER IER, LUN, XYVERT_LUN, MONTH, JDATE, IYEAR
      COMPLEX*8 XYVERT_DAT, XYVERT_TIM, DTIME, DDATE
      CHARACTER CDATE*8, CTIME*8
      EQUIVALENCE (CDATE, DDATE), (CTIME, DTIME)
      CHARACTER*8 VARS(6)
      DATA VARS /'X', 'Y', 'CHI2', 'PHI', 'ZVERT', 'NVERT'/
      DATA SEPARATE_RUNS /.FALSE./
      DATA FIRST /.TRUE./
      DATA CDATE /'  /  /  '/
C
      XYVERT_PAR = .TRUE.
      IF (FIRST) THEN
        CALL EZPICK('XYVERT_RCP',IER)
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('XYVERT','XYVERT_PAR',
     &      'Unable to find bank XYVERT_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('SEPARATE_RUNS',SEPARATE_RUNS,IER)
        CALL EZGET('DO_VTX_TRACK',IDOVTX,IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG('INRCP ERROR','XYVERT_PAR',
     &      'Attempt to read XYVERT_RCP failed','F')
          XYVERT_PAR = .FALSE.
        ENDIF
        CALL EZRSET
      ENDIF
C
C
      IF (FIRST.OR.SEPARATE_RUNS) THEN
C
C ****  Open RZ-file
C Separate file for each run if requested
C
        CALL GTUNIT(600,LUN,IER)
        CALL D0RZOPEN(LUN,'XYVERT.HST','UO',4096,OK)
        IF (.NOT.OK) THEN
          CALL INTMSG('XYVERT - UNABLE TO OPEN NTUPLE FILE')
          XYVERT_PAR = OK
          GO TO 999
        END IF
C
        CALL HRFILE(LUN,'AAABBB','N')
        CALL DHDIR('XYVERT_RCP','HBOOK_DIRECTORY',IER,' ')
        CALL HBOOKN(1,'LAYER 0 SEGMENTS',6,'AAABBB',1000,VARS)
        CALL HBOOKN(2,'LAYER 1 SEGMENTS',6,'AAABBB',1000,VARS)
        CALL HBOOKN(3,'LAYER 2 SEGMENTS',6,'AAABBB',1000,VARS)
        CALL HBOOKN(4,'TRACKS          ',6,'AAABBB',1000,VARS)
      ENDIF
      CALL IDATE2k(MONTH,JDATE,IYEAR)
      WRITE(CDATE(1:2),'(I2.2)') MONTH
      WRITE(CDATE(4:5),'(I2.2)') JDATE
      WRITE(CDATE(7:8),'(I2.2)') IYEAR
      CALL TIME(CTIME) 
      FIRST = .FALSE.
  999 RETURN
C----------------------------------------------------------------------
      ENTRY XYVERT_LUN()
      XYVERT_LUN = LUN
      RETURN
C
      ENTRY XYVERT_DAT()
      XYVERT_DAT = DDATE
      RETURN
C
      ENTRY XYVERT_TIM()
      XYVERT_TIM = DTIME
      RETURN
C
      END
