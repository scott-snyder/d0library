      SUBROUTINE SCAN_START(NPROM,PROMPT,OUTSTR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Gets date and times and previously scanned
C-                         questions if they exist
C-   Inputs  : NPROM = Number of questions from PX_SCANNING_RCP (50 max)
C-             PROMPT = Array of questions
C-   Outputs : OUTSTR= Previous answer to questions if available
C-   Controls: None
C-
C-   Created  07-AUG-1992   Andrew Brandt
C-   Updated  17-jan-1993   Vipin Bhatnagar
C-    Drop complete CRCP linear bank structure for a rescan case
C-   UPDATED  28-jan-1993   Lupe Howell Put error checck after EZPICK
C-   Updated  23-MAY-1993   Vipin Bhatnagar ALLOWQ=55
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$LINKS:IZZFIT.LINK'
      INCLUDE 'D0$LINKS:IZZTMP.LINK'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER IER, GZPROC, LENGTH, I, J
      INTEGER IZLINK, LADDR
C
      INTEGER NQUESO,ALLOWQ
      LOGICAL EZERROR
C
C  Number of quesions allowed
C
      PARAMETER ( ALLOWQ=55 )
      CHARACTER*10 QUESO(ALLOWQ)
      CHARACTER*4 BANK
      CHARACTER*32 BNKRCP
      CHARACTER*30 SDATE,SBTIME
      CHARACTER*76 MESG
      LOGICAL QSAME,RESCAN
      INTEGER IALONE,LALONE,IQUES,JQUES,IANS(ALLOWQ),CROSS(ALLOWQ)
      PARAMETER ( IALONE=0 )
      CHARACTER*40 PROMPO(ALLOWQ)
      CHARACTER*20 OLDANS(ALLOWQ),NOWORD
      DATA NOWORD /'                    '/
      INTEGER IDSTRQ,IDSTRA
      PARAMETER(IDSTRQ=1)
      PARAMETER(IDSTRA=2)
C
C Variables for DATIME routine
C
      INTEGER ID, IH
      CHARACTER*30 DATE, TIME
      CHARACTER*2  YR
C
C----------------------------------------------------------------------
C
      INTEGER NPROM
      CHARACTER*(*) PROMPT(*)
      CHARACTER*(*) OUTSTR(*)
      DATA YR/'19'/
C
C----------------------------------------------------------------------
C
C  Get date and time
C
      CALL DATIME(ID,IH)
C-
      CALL PXITOC(IH,4,TIME)
      CALL PXITOC(ID,6,DATE)
C-
      DATE=YR//DATE
C
      CALL EZPICK('PX_SCANNING_RCP')
      IF ( EZERROR(IER) ) THEN
         CALL ERRMSG('PIXIE','SCAN_START',
     &        'Can NOT find PX_SCANNING_RCP','W')
       ELSE
         CALL EZSETS('SCAN_DATE',1,DATE,30,IER)
         CALL EZSETS('SCAN_BTIME',1,TIME,30,IER)
         CALL EZRSET
       ENDIF
C
C Intialize IANS which will be set to 1 for questions that have answers
C From previous scan
C
      CALL FLGSET('ACTIVE_SCAN',.TRUE.)
      CALL VZERO(IANS,ALLOWQ)
C
C
C ****  Check for previous scanning bank, if none bail out
C
      LPROC=GZPROC()
      IF(LPROC.EQ.0) THEN
        CALL ERRMSG('SCANNING','SCAN_START', 'PROC BANK NOT SET UP','W')
        RETURN
      ENDIF
      IF (IQ(LPROC-2) .LE. 7) THEN
        LADDR=0
      ELSE
        IZLINK=8
        LADDR=LQ(LPROC-IZLINK)
      ENDIF
      IF(LADDR.EQ.0) GO TO 900
C
C-------Marcel Demarteau's modification---
C ****  Is this a complete rescan or a second scan
C
      RESCAN=.FALSE.
      MESG=
     &' If this is a rescan, all old scanning information will be '
      CALL OUTMSG(MESG)
      MESG=
     &' deleted. If it is a second scan the answers of the previous '
      CALL OUTMSG(MESG)
      MESG=
     &' scanners are kept'
      CALL OUTMSG(MESG)
      CALL GETPAR(1,'Is this a rescan? [N]','L',RESCAN)
C-
C---Dropping the complete linear bank structure for rescan case
C-
      IF(RESCAN) THEN
        CALL MZDROP(IXCOM,LADDR,'L')
      ENDIF
C
C
  900 CONTINUE
      DO 200 I=1,NPROM
        OUTSTR(I)=NOWORD
  200 CONTINUE
C
  999 RETURN
      END
