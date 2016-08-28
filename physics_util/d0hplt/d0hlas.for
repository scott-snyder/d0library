      SUBROUTINE D0HLAS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print a hardcopy file on laser printer
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  26-FEB-1990   Jan S. Hoftun (from old D0HLAS)
C-   Call to D0HFORM added 8-MAY-1991 S. Hagopian
C-   Updated  18-JUN-1991  Lupe Howell Do not print if file name not found
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*3 CDRV
      INTEGER L1,L2,L3,TRULEN,ISTAT,QPRINT
      CHARACTER*32 FILE,FORM,QUE
C----------------------------------------------------------------------
C
C  First, find out what printer driver one is using....
C  =======================================================
C
      FILE = ' '
      CALL D0HDRV(2,CDRV)
      IF( CDRV.EQ. '   ' .OR. CDRV .EQ. 'DRV' ) THEN
        CALL INTMSG('0Hardcopy can not be done without a driver '//
     &               'for DI3000 device 2' //CHAR(7))
        RETURN
      ELSEIF (CDRV .EQ. 'LN3') THEN
        FILE = 'LN03P.DAT'
      ELSEIF (CDRV.EQ.'TLL'.OR.CDRV.EQ.'TLP'.OR.CDRV.EQ.'TL8')THEN
        FILE = 'TALARIS.DAT'
      ELSEIF (CDRV .EQ. 'Q12') THEN
        FILE = 'QMS.DAT'
      ELSEIF (CDRV .EQ. 'PST') THEN
        CALL D0HPOST(FILE)
      ELSE
        CALL INTMSG('0Device 2 NOT a valid laser printer device!'//
     &    CHAR(7))
        RETURN
      ENDIF
C
C *** If the file name was not set do not attempt to
C *** print to printer
C
      IF ( FILE .EQ. ' ' ) 
     &  GOTO 999
C
      CALL D0HFORM(CDRV,QUE,FORM)
      L1 = TRULEN(FILE)
      L2 = TRULEN(FORM)
      L3 = TRULEN(QUE)
      IF(QUE.EQ.'DUMMY')THEN
C save file. Do not print it.
        CALL INTMSG(' FILE '//FILE(1:L1)//' kept, not PRINTED')
        GO TO 999
      ELSE
        ISTAT=QPRINT(FILE(1:L1),QUE(1:L3),FORM(1:L2),.TRUE.)
        IF(ISTAT.ne.0) THEN
          CALL INTMSG(' FILE '//FILE(1:L1)//' PRINTED ON QUEUE '//QUE)
        ELSE
          CALL MSGSCR(ISTAT,' Error printing-->')
        ENDIF
      ENDIF
  999 RETURN
      END
