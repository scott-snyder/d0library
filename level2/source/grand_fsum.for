      FUNCTION GRAND_FSUM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : make grand filter summary
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  12-JAN-1992   James T. Linnemann
C-   Updated   8-MAR-1994   R. J. Genik II  added error message when
C-   read_fsum fails and immediate translation of logical names. Also, will
C-   write to logical filename grand_fsum_out if defined; otherwise
C-   defaults to grand_fdiff.out of grand_fsum.out
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL GRAND_FSUM,GRAND_FILTER_SUMMARY,
     &  FILTER_INITIALIZ,FILTER_RUN
      INTEGER IP,NPARIN,IER,TYPE,LEN, trnlnm, trulen
      LOGICAL EZERROR,OK,GOTIT,SUMMARY
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      CHARACTER*128 FILENAME,ACTION, IER_CHR, fntrans, output_file
      INTEGER IFILE,NCHR,LUN,GRAND_FSUM_USER, log_or_not, fn_len
      REAL    TIME,TTIME,WEIGHT  !event weight
      PARAMETER( GRAND_FSUM_USER = 40809  )
      CHARACTER*20 output_logical
      data  Output_Logical/'GRAND_FSUM_OUT'/
C----------------------------------------------------------------------
C     INITIALIZE
C
      TTIME = 0.
C
C...set up vms_filter environment
C
      OK = FILTER_INITIALIZ()
      OK = FILTER_RUN()
C
C...first, carefully retrieve files to read from RCP
C
      CALL INRCP('GRAND_FSUM_RCP',IER)
      CALL EZPICK('GRAND_FSUM_RCP')
      OK = .NOT.EZERROR(IER)
      IF (.NOT.OK) THEN
        CALL ERRMSG('GRAND_FSUM','GRAND_FSUM','Couldn''t find bank','F')
      ENDIF
C
C     from RCP, get 'ACTION' - 'SUM' if summary, otherwise
C     if empty, do 'SUM', otherwise do 'DIFF'
C
      CALL EZGETS('ACTION',1,ACTION,LEN,IER)
      IF (IER .NE. 0) THEN      ! Error reading RCP
          CALL ERRMSG('GRAND_FSUM','GRAND_FSUM',
     &      '"ACTION" not found in GRAND_FSUM.RCP - SUM assumed','I')
        ACTION = 'SUM'
      ENDIF
      SUMMARY = ACTION(1:3).EQ.'SUM'
C
C     get 1st file name
C
      CALL EZGETS('FNAMES',IFILE,FILENAME,NCHR,IER)
      IF (IER .NE. 0) THEN      ! Error reading RCP
          CALL ERRMSG('GRAND_FSUM','GRAND_FSUM',
     &      'Could not find parameter','F')
      ENDIF
      log_or_not = trnlnm(filename, fntrans, fn_len)
      If (log_or_not) filename = fntrans
      IFILE  = 1
C
C     open output file
C
      CALL GTUNIT(GRAND_FSUM_USER,LUN,IER)
      Log_or_not = trnlnm(OUTPUT_LOGICAL,fntrans,fn_len)
      If (Log_or_not) then
        output_file=fntrans
      Else
        IF (SUMMARY) THEN
          output_file = 'GRAND_FSUM.OUT'
        ELSE
          output_file = 'Grand_Fdiff.out'
        ENDIF
      Endif
      CALL D0OPEN(LUN,output_file,'OF',GOTIT)
C
C     loop over files
C
      DO WHILE (IER.EQ.0)
C
C       check if we are trying to "DIFF" or "SUMM"
C
        IF (SUMMARY) THEN
C
C         read in the next file
C
          CALL READ_FSUM(FILENAME,WEIGHT,TIME,IER)
          IF (IER.EQ.0) THEN
  100     Format (X,'Reading File : ',/,6X,A,/,6X,' Weight = ', 1PG10.4,
     +      ' D0-Time = ',1PG10.4)
            WRITE(LUN,100) 
     &        FILENAME(1:trulen(filename)) ,WEIGHT,TIME
            CALL SUM_FSUM(TIME,TTIME)
            IFILE = IFILE + 1
            CALL EZGETS('FNAMES',IFILE,FILENAME,NCHR,IER)
            IF (NCHR.LE.0) IER = 1
            log_or_not = trnlnm(filename, fntrans, fn_len)
            If (log_or_not) filename = fntrans
          ENDIF
        ELSE
C
C         "DIFF" - are we trying to "DIFF" with too many files? (won't
C         make sense, this is just a check on the RCP used)
C  
          IF (IFILE.GT.2) THEN
            CALL ERRMSG('GRAND_FSUM','GRAND_FSUM',
     &      'Attempt to "DIFF" with more than 2 input files','F')
          ENDIF
C
C         read in the next file
C
          CALL READ_FSUM(FILENAME,WEIGHT,TIME,IER)
          IF (IER.EQ.0) THEN
            WRITE(LUN,100)
     &        FILENAME(1:trulen(filename))
     &        ,WEIGHT,TIME
            CALL DIFF_FSUM(IFILE,TIME)
            IFILE = IFILE + 1
            CALL EZGETS('FNAMES',IFILE,FILENAME,NCHR,IER)
            IF (NCHR.LE.0) IER = 1
            log_or_not = trnlnm(filename, fntrans, fn_len)
            If (log_or_not) filename = fntrans
          Else
            Write(IER_CHR,'(I3)') IER
            Call errmsg('Grand_Fsum', 'Grand_Fsum', 
     +        'Read_Fsum Returned non-zero error code = ' 
     +        //IER_CHR,'W')
          ENDIF
        ENDIF
      ENDDO
C
C     OK, now printout and be done with it!
C
      IF (OK) CALL EZRSET
C
      IF (SUMMARY) THEN
        GRAND_FSUM = GRAND_FILTER_SUMMARY(TTIME,IFILE-1,LUN)
      ELSE
        CALL GRAND_FILTER_DIFF(LUN)
      ENDIF
C
      CLOSE (LUN)
C
C     that's all folks
C
      IF (SUMMARY) THEN
        STOP ' GRAND_FSUM is finished with SUMMARY'
      ELSE
        STOP ' GRAND_FSUM is finished with DIFFERENCES'
      ENDIF
C
  999 RETURN
      END
