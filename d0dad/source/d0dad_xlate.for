      SUBROUTINE D0DAD_XLATE(FCNAME,FCLOCAL,IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Build a local copy of a file catalog.  Filenames
C-     in the local copy have already had LIB$FIND_FILE called to
C-     allow faster filename lookup. 
C-
C-   Inputs  : FCNAME  - File catalog containing untranslated filenames.
C-             FNLOCAL - Local copy with translated filenames.
C-   Outputs : IERR - 0==>No errors
C-   Controls:
C-
C-   Created  22-Feb-1994   John D. Hobbs
C-   Modified 16-Aug-1994   John D. Hobbs - Fix bug in filename comparison
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:d0dad.inc'
      INCLUDE 'D0$INC:d0dadcom.inc'
      CHARACTER*(*) FCNAME,FCLOCAL
      CHARACTER*200 FCNIN,FCGIN,FCTAPE*20,FCCOM*40,FCNOUT,FCGTMP,
     + FCTTMP*20,FCCTMP*40,cinbas*160,coutbas*160
      INTEGER IERR,LUNIN,LUNOUT,I,NIN,NOUT,ICNTXT,ILEN,NBLOCK,NTRANS
      INTEGER NFOUND,NERROR,NRETRANS
      LOGICAL  OK,LEXIST
      INTEGER LENOCC
      LOGICAL LIB$FIND_FILE
      EXTERNAL LENOCC,LIB$FIND_FILE
C-----------------------------------------------------------------------
C
      IF( LDDBG.GT.5 ) WRITE(*,1003) FCNAME(1:LENOCC(FCNAME)),
     +  FCLOCAL(1:LENOCC(FCLOCAL))
 1003 FORMAT(' *********** In D0DAD_XLATE *************',/,
     +       '     Input file catalog: ',A,/,
     +       '     Output file catalog: ',A,/,/)
C
      CALL D0DAD_OPEN(JFFC,FCNAME,'R',LUNIN,IERR)
      IF( IERR.NE.0 ) THEN
        IF( LDDBG.GT.0 ) WRITE(*,1001) IERR,FCNAME(1:LENOCC(FCNAME))
 1001   FORMAT(' Error ',I5,' opening: ',A)
        IERR = -1
        GOTO 999
      ENDIF
C
      CALL D0DAD_OPEN(JFFC,FCLOCAL,'A',LUNOUT,IERR)
      IF( IERR.NE.0 ) THEN
        IF( LDDBG.GT.0 ) WRITE(*,1001) IERR,FCLOCAL(1:LENOCC(FCLOCAL))
        IERR = -2
        GOTO 999
      ENDIF
C
      NFOUND=0
      NBLOCK=0
      NTRANS=0
      NRETRANS=0
      CALL FCHRD(LUNIN,NIN,IERR)
      CALL FCHRD(LUNOUT,NOUT,IERR)
      IF( LDDBG.GT.10 ) WRITE(*,1002) NIN,NOUT
 1002 FORMAT(' Number of files in input: ',I10,/,
     +       ' Number of files in output: ',I10)
C
      DO 10 I=1,NIN
        IF( LDDBG.GT.4 .AND. MOD(I,50).EQ.0 ) WRITE(*,1101) I
 1101   FORMAT('     processing entry ',I8)
        CALL FCGET(LUNIN,I,FCNIN,FCGIN,FCTAPE,FCCOM,IERR)
C    Check for dead file...
        IF( FCNIN(1:LEN(FCDELF)).EQ.FCDELF) THEN
          NBLOCK=NBLOCK+1
          CALL FCPUT(LUNOUT,FCNIN,FCGIN,FCTAPE,FCCOM,I,IERR)
          GOTO 10
        ENDIF
C    Check for replaced file...
        IF( FCNIN(1:LEN(FCREPL)).EQ.FCREPL) THEN
          NBLOCK=NBLOCK+1
          CALL FCPUT(LUNOUT,FCNIN,FCGIN,FCTAPE,FCCOM,I,IERR)
          GOTO 10
        ENDIF
C    If this entry is already in output fc, check for already properly
C    translated.
        IF( I.LE.NOUT ) THEN
          CALL FCGET(LUNOUT,I,FCNOUT,FCGTMP,FCTTMP,FCCTMP,IERR)
          CALL FILENAME_PARSE(FCNIN,'NAM+EXT',CINBAS,ILEN)
          CALL FILENAME_PARSE(FCNOUT,'NAM+EXT',COUTBAS,ILEN)
          INQUIRE(FILE=FCNOUT,EXIST=LEXIST)
          CALL CUTOL(CINBAS)
          CALL CUTOL(COUTBAS)
          IF( LEXIST .AND. CINBAS.EQ.COUTBAS ) THEN
            NFOUND=NFOUND+1
            GOTO 10
          ENDIF
        ENDIF
C    This is definately a file needing translation.  Translate and add.
        ICNTXT=0
        OK = LIB$FIND_FILE(FCNIN,FCNOUT,ICNTXT)
        IF( OK ) THEN
          IF( I.LE.NOUT ) THEN
            NRETRANS=NRETRANS+1
          ELSE
            NTRANS=NTRANS+1
          ENDIF
          CALL FCPUT(LUNOUT,FCNOUT,FCGIN,FCTAPE,FCCOM,I,IERR)
        ELSE
          NERROR=NERROR+1
          CALL FCPUT(LUNOUT,FCNIN,FCGIN,FCTAPE,FCCOM,I,IERR)
        ENDIF
        CALL LIB$FIND_FILE_END(ICNTXT)
 10   CONTINUE
C
      CALL D0DAD_CLOSE(LUNIN,IERR)
      CALL D0DAD_CLOSE(LUNOUT,IERR)
      IF( LDDBG.GT.3 ) WRITE(*,8001) FCNAME(1:LENOCC(FCNAME)),NBLOCK,
     +  NFOUND,NRETRANS,NTRANS,NERROR
 8001 FORMAT(/,'  Translation finished for "',A,'"'/,
     +       '     Number of blocked files: ',I10,/,
     +       '     Number of found files: ',I10,/,
     +       '     Number of retranslated files: ',I10,/,
     +       '     Number of newly translated files: ',I10,/,
     +       '     Number of translation errors: ',I10,/)
C
  999 CONTINUE
      RETURN
      END
