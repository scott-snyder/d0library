      SUBROUTINE D0DAD_READ_EVENT(IR,IE,IDDZRN,IDDZBO,IDDFID,IER)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Read the event specified by IRUN,IEVENT...
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-May-1994   John D. Hobbs
C-   Modified 02-Dec-1994   JDH - Add RCP parameters to control error
C-                             reporting.
C-            19-DEC-1994   JDH - Add second file translation if first
C-                             fails and a pretranslated catalog is 
C-                             being used.
C-   Updated  13-FEB-1995   JDH/Chip Stewart  - added D0DAD_FZFILENAME 
C-   Updated  05-MAY-1995   JDH - Remove CFxxxx calls.  Modify output
C-                             messages
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:quest.inc'
      INCLUDE 'D0$INC:d0dadcom.inc'
      CHARACTER FILE_FAIL_MESSAGE*(*),ROUTINE*(*)
      PARAMETER(FILE_FAIL_MESSAGE='OPEN_FAILURE')
      PARAMETER(ROUTINE='D0DAD_READ_EVENT')
C
      INTEGER IOLDFID,IER,IDDFID,NEV,NEVF,IR,IE,ICNTXT,IXWIPE,IUNIT
      INTEGER IDDZRN,IDDZBO,LLRECL,IKEY
      INTEGER IDADDF,IDADFC,IDADZB,FLEN
      LOGICAL ZEBFOPEN,ZEBFOK,OK,FIRST
      CHARACTER*200 FNZEB,FNFAT,FNTAPE*40,CHOPT*3,FNTMP,COMMENT*80
      CHARACTER*132 ERRTXT,TMPNAME
C
      INTEGER LENOCC
      LOGICAL LIB$FIND_FILE,EZERR
      EXTERNAL LIB$FIND_FILE,EZERR,LENOCC
C
      SAVE IOLDFID,ZEBFOPEN,ZEBFOK,IDADFC,IDADZB,FIRST,LLRECL,FNZEB
      DATA FIRST/.TRUE./
C-----------------------------------------------------------------------
C
      IF(FIRST) THEN
        LLRECL=8190
        IDADZB=0
        ZEBFOPEN=.FALSE.
        ZEBFOK=.FALSE.
        FIRST=.FALSE.
        CALL ERRMAX(FILE_FAIL_MESSAGE,0,10000000)
        CALL D0DAD_GTUNIT(IDADZB,IKEY,IER)
      ENDIF
C
C   Setup next input ZEBRA file if necessary.  If a pretranslated file
C   catalog is being used, overwrite generic form if initial open
C   fails...
C
      IF( IDDFID.NE.IOLDFID) THEN
        IOLDFID=IDDFID
        NEV=0
        IF( ZEBFOPEN ) THEN
          IF( ZEBFOK ) CALL FZENDI(IDADZB,'TX')
          CLOSE(IDADZB)
          ZEBFOPEN=.FALSE.
          ZEBFOK=.FALSE.
        ENDIF
        CALL FCGET(IDADFC,IDDFID,FNTMP,FNFAT,FNTAPE,COMMENT,IER)
        IF( IER.NE.0 ) THEN
          WRITE(ERRTXT,1001) IER,IDDFID 
 1001     FORMAT(' Error',I8,' getting file number ',I8,' from FC.',
     >       ' Notify Expert')
          CALL ERRMSG(FILE_FAIL_MESSAGE,ROUTINE,ERRTXT,'W')
          GOTO 901
        ENDIF
        IF( .NOT.LIB$FIND_FILE(FNTMP,FNZEB,ICNTXT) ) THEN
          CALL LIB$FIND_FILE_END(ICNTXT)
          ICNTXT=0
          CALL FILENAME_PARSE(FNTMP,'NAM+EXT',FNZEB,FLEN)
C    If it's a pretranslated file, try again using generic form
C    NB: This is risky, 'cause it assumes D0$DATA$DST is flag for
C    non-translated file catalog...
          FNTMP='D0$DATA$DST:'//FNZEB(1:FLEN)
          IF( .NOT.LIB$FIND_FILE(FNTMP,FNZEB,ICNTXT) ) THEN
            CALL LIB$FIND_FILE_END(ICNTXT)
            ICNTXT=0
            WRITE(ERRTXT,1002) 1,FNTMP(1:LENOCC(FNTMP)) 
 1002       FORMAT('FailedFile(',I1,'): ',A)
            CALL ERRMSG(FILE_FAIL_MESSAGE,ROUTINE,ERRTXT,'W')
            GOTO 902
          ENDIF
        ENDIF
        CALL LIB$FIND_FILE_END(ICNTXT)
        ICNTXT=0
        CALL D0RZOPEN(IDADZB,FNZEB,'ISU',4*LLRECL,OK)
        IF( .NOT.OK ) THEN
          WRITE(ERRTXT,1002) 2,FNTMP(1:LENOCC(FNTMP))
          CALL ERRMSG(FILE_FAIL_MESSAGE,ROUTINE,ERRTXT,'W')
          GOTO 903
        ENDIF 
        ZEBFOPEN=.TRUE.
        CALL FZFILE(IDADZB,LLRECL,'DXI')
        IF( IQUEST(1).NE.0 ) THEN
          WRITE(ERRTXT,1002) 3,FNTMP(1:LENOCC(FNTMP))
          CALL ERRMSG(FILE_FAIL_MESSAGE,ROUTINE,ERRTXT,'W')
          GOTO 905
        ENDIF
        ZEBFOK=.TRUE.
*JDH        WRITE(*,*) ' D0DAD: ProcessingFile: ',FNTMP(1:LENOCC(FNTMP))
      ENDIF
C
      IF( .NOT.ZEBFOK ) GOTO 908
C
C  Read in the event.  NB: This routine knows nothing about
C  start/end-of-run or end-of-files.  Treat these as errors.
C
      IXWIPE=IXCOM+IXMAIN
      CALL MZWIPE(IXWIPE)
      CALL FZINXT(IDADZB,IDDZRN,IDDZBO)
      CALL FZIN(IDADZB,IXMAIN,LHEAD,1,' ',0,0)

C  Hard Error...

      IF( IQUEST(1).LT.0 ) GOTO 906

C  Logic error...

      IF( IQUEST(1).GT.0 ) GOTO 907
C
C  Everything normal.  Continue...
C
 999  CONTINUE
      IER=0
      RETURN
C
 901  CONTINUE
      IER = -1
      RETURN

 902  CONTINUE
      IER = -2
      RETURN

 903  CONTINUE
      IER = -3
      RETURN

 904  CONTINUE
      IER = -4
      RETURN

 905  CONTINUE
      IER = -5
      RETURN

 906  CONTINUE
      IER = -6
      RETURN

 907  CONTINUE
      IER = -7

 908  CONTINUE
      IER = -8
      RETURN
C-----------------------------------------------------------------------
      ENTRY D0DAD_READEVENT_SET(IUNIT)
C-----------------------------------------------------------------------
C
C  Let DFREAD know the unit on which the FC has been opened.
C
C-----------------------------------------------------------------------
      IDADFC=IUNIT
      RETURN
C-----------------------------------------------------------------------
      ENTRY D0DAD_FZFILENAME(TMPNAME)
C-----------------------------------------------------------------------
C
C  Let anyone who asks D0DAD_FZFILENAME know the name of the current FZ file.
C
C-----------------------------------------------------------------------
      TMPNAME = FNZEB
      RETURN
      END
