      SUBROUTINE MAC_READ_BIN_CODE_FILE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reads the bin codes from a text file instead of
C-   from the instruction file. the next instruction should be the
C-   filename.
C-
C-   Inputs  :
C-   Outputs : Fills the common block Bin_Code_Names
C-   Controls:
C-
C-   Created  18-APR-1993   R. J. Genik II
C-   Updated  29-NOV-1993   R. J. Genik II  Bug fix: Bin_Code_Filename,
C-   previously the filemane which was passed to the routine, was
C-   eliminated and the proper bin_code_file from he global common block
C-   was inserted in it's place.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      Include 'D0$INC:MAC_Global.inc'
      INTEGER BIN_CODE_FILE_UNIT,IERR,IOS,bin_code
      CHARACTER*32 Bin_name
      CHARACTER*1 Comment
      LOGICAL opened_ok
      INTEGER chr_beg,chr_end,chr_num,I_ERR
C----------------------------------------------------------------------
      Call MAC_Get_Next_Command(bin_code_file,I_ERR)
      IF (I_ERR.ne.0) then
        Call Errmsg('READ FAILED',
     +  'MAC_Compare',
     +  'Cannot READ bin code filename from MAC Instructions','W')
        goto 999
      Endif
      Call Swords(bin_code_file,chr_beg,chr_end,chr_num)
C
C... Read BIN_CODE_NAME ARRAY
C
      Call gtunit(Igtunit_userid,BIN_CODE_FILE_UNIT,IERR)
      if (ierr.ne.0) Call Errmsg('GTUNIT failed',
     +  'MAC_Compare',
     +  'CANNOT get unit for ifailed data file')
      Call d0open (BIN_CODE_FILE_UNIT,BIN_CODE_FILE(CHR_BEG:CHR_END),
     +  'IF',opened_ok)
      IF (.NOT.opened_ok) then
        Call Errmsg('D0OPEN FAILED',
     +  'MAC_Compare',
     +  'Cannot open BIN_CODE_FILE from MAC Instructions','W')
        goto 3
        endif
    1 READ (BIN_CODE_FILE_unit,FMT=2,END=3,IOSTAT=IOS)
     +  Comment,BIN_CODE,BIN_NAME
    2 FORMAT(A1,I3,A32)
      IF (COMMENT.EQ.'!') GOTO 1 !Skip Comment lines with ! as 1st character
      IF (IOS.GT.0) then
        Call Errmsg('READ FAILED',
     +  'MAC_Compare',
     +  'Cannot READ BIN_CODE_FILE from MAC Instructions','W')
        goto 3
      Endif
C
C  check that we are within the bounds of the array, then assign the code.
      IF (BIN_CODE.GT.nbins_max) then
        Call Errmsg('READ FAILED', 'MAC_Compare',
     +'INTEGER GT NBINS, BIN_CODE_FILE from MAC Instructions'
     +,'W')
      ELSEIF (BIN_CODE.LT.-1) then
      Call Errmsg('READ FAILED', 'MAC_Compare',
     +'INTEGER LT -1, BIN_CODE_FILE from MAC Instructions'
     +,'W')
      ELSE
        BIN_CODE_NAME(BIN_CODE) = BIN_NAME
      ENDIF
C
      GOTO 1  ! Go get the next line, READ will skip to line 3 on <eof>
    3 Continue
C  release unit
      Call RLUNIT(igtunit_userid,BIN_CODE_FILE_UNIT,ierr)
      if (ierr.ne.0) Call Errmsg('RLUNIT failed',
     +  'MAC_Compare',
     +  'CANNOT release unit for ifailed data file','W')
C
  999 RETURN
      END
