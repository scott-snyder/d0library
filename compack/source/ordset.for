      SUBROUTINE ORDSET(FILNAM,USELEV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read OLD (pre V4.00) setup information
C-
C-   Inputs  : FILNAM: Logical name of file to read from
C-             USELEV: Number of level to put info in.
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-   Modified   16-MAY-1991   Scott Snyder
C-    Store help with STRSTO.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) FILNAM
      INTEGER USELEV
C
C     Get definitions for COMPACK
C
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
C
      CHARACTER*80 TITLE(4),MSGLIN
      CHARACTER*2048 TXT
      INTEGER NUM,I,ISTAT,LENG,IUNI,IERR,IOS
      INTEGER*4 STRSTO
C----------------------------------------------------------------------
C
C     Open setup input file
C
      CALL GTUNIT(555,IUNI,IERR)
      IF(IERR.EQ.0) THEN
C&IF VAXVMS
        OPEN (IUNI,FILE=FILNAM,STATUS='OLD',FORM='UNFORMATTED',
     *         READONLY,IOSTAT=IOS)
C&ELSE
C&        OPEN (IUNI,FILE=FILNAM,STATUS='OLD',FORM='UNFORMATTED',
C&     *         IOSTAT=IOS)
C&ENDIF
        IF(IOS.NE.0) THEN
          CALL ABOMEN(2,FILNAM)
        ELSE
C
C     Read information
C
          MAXLIN(USELEV)=0
          DO 100 I=1,MAXPOS
            READ(IUNI,IOSTAT=ISTAT,ERR=101)NUM,TITLE,LENG,TXT(1:LENG)
            IF(NUM.GT.0.AND.NUM.LE.MAXPOS) THEN
              TOPLIN(NUM,USELEV)=TITLE(1)
              MENLIN(NUM,USELEV)=TITLE(2)
              COMLIN(NUM,USELEV)=TITLE(3)
              HELP_COOKIES(NUM, USELEV) = STRSTO(TXT(1:LENG))
              IF(NUM.GT.MAXLIN(USELEV)) MAXLIN(USELEV)=NUM
            ENDIF
  100     CONTINUE
  101     CONTINUE
C
C     IOSTAT=0 when successful and =-1 when EOF found before end of loop
C
          IF(ISTAT.NE.0.AND.ISTAT.NE.-1) THEN
            WRITE(MSGLIN,99) ISTAT
   99       FORMAT(' READ_SET IOSTAT=',I8)
            CALL OUTMSG(MSGLIN)
          ENDIF
          CLOSE(IUNI)
          CALL RLUNIT(555,IUNI,IERR)
        ENDIF
      ELSE
        WRITE(MSGLIN,98) IERR
   98   FORMAT(' READ_SET GTUNIT/IERR=',I3)
        CALL OUTMSG(MSGLIN)
      ENDIF
      RETURN
      END
