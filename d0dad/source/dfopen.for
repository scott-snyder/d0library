      SUBROUTINE DFOPEN(LWRITE,LUN,IKEY,FNAME,CSTAT,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Open a d0dad file.
C-      Called from D0DAD_OPEN.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  14-NOV-1993   John D Hobbs
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      LOGICAL LWRITE,OK
      INTEGER LUN,IRECL,IERR,IKEY,OUT_REC,NRECS
      CHARACTER*(*) FNAME,CSTAT
      CHARACTER*10  CACCES
C----------------------------------------------------------------------
C
C  Setup...
C
      IERR=0
      IRECL=JRECDF
      CACCES='ISU'
      IF( CSTAT.EQ.'OLD' .AND. LWRITE ) CACCES='MSU'
      IF( CSTAT.NE.'OLD' .AND. LWRITE ) CACCES='OSU'
C
C  Do the open...
C
      CALL D0RZOPEN(LUN,FNAME,CACCES,JPRECDF*4,OK)
      IF( .NOT.OK ) GOTO 902
C
C  Setup the Zebra bank to hold file info...
C
      CALL DFBANK(IERR)
      IF( IERR.NE.0 ) GOTO 904
      IQ(LDFHD+JLUN)=LUN
      IF(LWRITE)IQ(LDFHD+JRW)=1
      IQ(LDFHD+JKEY)=IKEY
C
C  Header stuff...
C
      IF( CSTAT.EQ.'OLD' ) THEN
        IERR=0
        CALL DFHRD(LUN,IERR)
        IF( IERR.NE.0 ) GOTO 903
        IF( LWRITE ) THEN
          CALL VZERO(DF_IOREC,JPRECDF)
          IQ(LDFHD+JDFIOFF) = IQ(LDFHD+NDDF+JDFLREC)
          NRECS = IQ(LDFHD+JDFIOFF)
          IF( NRECS.GT.0 ) THEN
            OUT_REC = (JRECDF*(NRECS-1))/JPRECDF+1 ! Skip the header record
            IF( NRECS.GT.0 ) READ(LUN,REC=OUT_REC+1,ERR=903) DF_IOREC
            CALL UCOPY(DF_IOREC,IQ(LDFHD+NDDF+JDFHED+1),JPRECDF)
          ENDIF
        ENDIF
      ELSE
        CALL DFHWRT(LUN,IERR)
        IF( IERR.NE.0 ) GOTO 903
      ENDIF
C
C  If reading, check for older versions which use CERN CF routines...
C
      IF( .NOT.LWRITE .AND. IQ(LDFHD+NDDF+JXXVER).LT.106) THEN
        CLOSE(LUN)
        IRECL=JRECDF
        CACCES='r'
        IF( CSTAT.EQ.'OLD' .AND. LWRITE ) CACCES='a'
        IF( CSTAT.NE.'OLD' .AND. LWRITE ) CACCES='w'
        CALL CFOPEN(LUN,0,JRECDF,CACCES,0,FNAME,IERR)
        IQ(LDFHD+JLUN)=LUN
        CALL DFHRD_OLD(LUN,IERR)
        IF( IERR.NE.0 ) GOTO 903
      ENDIF
C
  999 CONTINUE
      RETURN

 901  CONTINUE
      WRITE(D0DAD_ERRTXT,1001) FNAME,IERR
 1001 FORMAT('File "',A40,'", Open(write) error, Error=',I4)
      IF(LDDBG.GT.0) CALL ERRMSG(' ','DFOPEN',D0DAD_ERRTXT,'W')
      IERR = -1
      RETURN

 902  CONTINUE
      WRITE(D0DAD_ERRTXT,1002) FNAME,IERR
 1002 FORMAT('File "',A40,'", Open(read) error, Error=',I4)
      IF(LDDBG.GT.0) CALL ERRMSG(' ','DFOPEN',D0DAD_ERRTXT,'W')
      IERR = -2
      RETURN

 903  CONTINUE
      WRITE(D0DAD_ERRTXT,1003) FNAME,IERR
 1003 FORMAT('File "',A40,'", Header r/w error, Error=',I4)
      IF(LDDBG.GT.0) CALL ERRMSG(' ','DFOPEN',D0DAD_ERRTXT,'W')
      IERR = -3
      RETURN

 904  CONTINUE
      WRITE(D0DAD_ERRTXT,1004) FNAME,IERR
 1004 FORMAT('File "',A40,'", Cannot create Zebra bank.')
      IF(LDDBG.GT.0) CALL ERRMSG(' ','DFOPEN',D0DAD_ERRTXT,'W')
      IERR = -4
      RETURN
      END
