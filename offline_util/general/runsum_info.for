      subroutine Runsum_info(lout_unit,run_num,line_min,line_max,ierror)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : searches for run summary for run # run_num
C-                         and writes lines line_min to line_max in
C-                         lout_unit.
C      NOTE: Zebra Stores must have been initialized prior to calling
C-
C- ENTRY runsum_info_init gets the rcp info and sets the output device
C-
C-   Inputs  : run_num I5 integer
C-             lout_unit  integer unit for output
C-             line_min, line_max, integer line numbers to output
C-   Outputs : lines to lout_unit as decribed above
C-             ierror = 0 if runsum exists and was printed
C-   Controls: Runsum_info.rcp
C-
C-   Created   23-FEB-1993   R J Genik II
C-
C-   Modified  31-Mar-1993   R J Genik II - any integer now accepted,
C-   instead of just I5's.
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CHARACTER*50 BASE_RS_NAME(10)
      CHARACTER*16 EXT_RS_NAME(10)
      CHARACTER*80 ch_file_name
      CHARACTER*80 line
      LOGICAL run_file_exists,opened_ok
      INTEGER NAME_LEN,RUN_NUM,LINE_MIN,LINE_MAX,IERROR,nplaces
      INTEGER I_counter,lout_unit,gt_user_idn,infile_unit
      INTEGER eye,jay,kay,ierr
      SAVE BASE_RS_NAME,EXT_RS_NAME,gt_user_idn,nplaces
      INQUIRE(UNIT=lout_unit,OPENED=run_file_exists)
      IF (.NOT.RUN_FILE_EXISTS) THEN
        WRITE(6,FMT=10)
  10   format(1X,'Output file not opened yet, open and re-init')
      ENDIF
C
C... find file and make sure it exists
C
      DO 1 i_counter = 1,nplaces ! check in all places for file
        call swords(base_rs_name(i_counter),eye,jay,kay)
        WRITE(CH_FILE_NAME,fmt=20) base_rs_name(i_counter)(eye:jay),
     &    run_num,EXT_RS_NAME(i_counter)
        INQUIRE(FILE=CH_FILE_NAME,EXIST=run_file_exists)
        IF (run_file_exists) GOTO 2
    1 CONTINUE
    2 CONTINUE
   20 FORMAT(1X,A,I6,A)
C... if no file exists, bail out
      IF (.NOT.RUN_FILE_EXISTS) THEN
        WRITE(lout_unit,FMT=25) run_num
   25   FORMAT(3X,'Run Summary for run ',I7,' not found')
        IERROR = -1
        GOTO 999
      ENDIF
C... proceed with read/write
      CALL gtunit(GT_USER_IDN,infile_unit,IERROR)
      IF (ierror.NE.0) CALL errmsg('GTUNIT failed',
     +  'RUNSUM_INFO',
     +  'CANNOT get unit for input data file','W')
      CALL d0open (infile_unit,ch_file_name,'IF',opened_ok)
      IF (.NOT.opened_ok) CALL errmsg('D0OPEN FAILED',
     +  'RUNSUM_INFO',
     +  'Cannot open RUN_SUM FILE from RUNSUM_INFO_RCP','W')
C
C... start looping over lines, output requested block.
C
      DO 100 i_counter = 1,line_max
        READ (infile_unit,fmt = 50,end=200) line
   50   FORMAT(A80)
        IF ((i_counter.GE.line_min).AND.(i_counter.LE.line_max))
     +   WRITE(lout_unit,60) line
   60   format(4X,A80)
  100 CONTINUE
      GOTO 999
  200 CALL errmsg('READ FAILED','RUNSUM_INFO'
     +    ,'Reached <eof> before requested lines were found','W')
      GOTO 999
c
c
      ENTRY runsum_info_init
c      CALL mzebra(0)
c      CALL inzcom(i)
c      CALL INRCP('AUTOCOMPARE_I_FAILED_rcp',IERROR)
c      IF (IERROR.NE.0) THEN
c        CALL ERRMSG('INRCP FAILED','AUTOCOMPARE_I_FAILED'
c     +    ,'CANNOT FIND AUTOCOMPARE_I_FAILED_RCP','F')
c      ENDIF
      CALL INRCP('RUNSUM_INFO_RCP',IERR)
      IF (IERR.NE.0) THEN
        CALL ERRMSG('INRCP FAILED','RUNSUM_INFO'
     +    ,'CANNOT FIND RUNSUM_INFO_RCP','W')
      ENDIF
      CALL EZPICK('RUNSUM_INFO_RCP')
C----------------------------------------------------------------------
C
C   Read contents of RCP and check for errors.
C
C----------------------------------------------------------------------
      CALL EZGET( 'GT_USER_IDN', GT_USER_IDN, IERR)
      IF (IERR.NE.0)
     +   CALL ERRMSG('EZGET FAILED','RUNSUM_INFO'
     +   ,'CANNOT READ GT_USER_IDN', 'W')
      CALL EZGET( 'NPLACES', NPLACES, IERR)
      IF (IERR.NE.0)
     +   CALL ERRMSG('EZGET FAILED','RUNSUM_INFO'
     +   ,'CANNOT READ NPLACES', 'W')
C....
      DO 800 i_counter = 1,nplaces
        CALL EZGETS( 'BASE_RS_NAME', i_counter, BASE_RS_NAME(i_counter),
     &    name_len, IERR)
        IF (IERR.NE.0)
     +    CALL ERRMSG('EZGETS FAILED','RUNSUM_INFO'
     +    ,'CANNOT READ BASE_RS_NAME','W')
      CALL EZGETS( 'EXT_RS_NAME', I_COUNTER, EXT_RS_NAME(I_counter),
     +  name_len, IERR)
      IF (IERR.NE.0)
     +   CALL ERRMSG('EZGETS FAILED','RUNSUM_INFO'
     +   ,'CANNOT READ EXT_RS_NAME','W')
  800 CONTINUE
  999 RETURN
      END
