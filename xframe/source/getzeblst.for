      SUBROUTINE GETZEBLST(BANK,MWV,WRUP,LWV,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Gets the .ZEB file pointed BANK.ZEB
C-
C-   Inputs  : BANK- Character*4 BANK name
C-             MWV - Maximum number of lines to read from file
C-   Outputs : WRUP- Character array containing LWV lines of text
C-             IERR - not zero implies cannot find file
C-   Controls: None
C-
C-   Created  22-APR-1989   Rajendran Raja
C-   Updated   3-SEP-1991   Herbert Greenlee
C-   Bastardized Aug-1992   me
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c
      include 'D0$XFRAME$SOURCE:d0map.inc'
c
      CHARACTER*(*) BANK,WRUP(*)
      INTEGER LWV,MWV,LUN,LEN3,INDEX,IERR,dotpos
      CHARACTER*50 FILNAM,FZEB,FZEBA,FNUL,RES_FILE
      DATA FNUL/' '/
      EXTERNAL LIB$FIND_FILE
      logical ok
      INTEGER CONTEXT
      integer nl,null
      character*4 cnl,cnull
c
      equivalence (nl,cnl),(null,cnull)
c
      data nl/538976266/,null/0/
C----------------------------------------------------------------------
      IERR = 0
c
c     check if we are supposed to do this
c
      if (zstate.eq.0) then
        call xerrmsg('.ZEB not enabled...')
        ierr = 1
        return
      endif
c
c       get the \nl and \0 (NULL) characters
c
cccccc     call getnlnull(nl,null)
C
      LUN=13
      IF(BANK.EQ.' ') GO TO 103         ! error return
      CALL UPCASE(BANK,BANK)    !convert to upper case
C
      if (bank.eq.'HEAD') then
        filnam = d0zeblst(1:d0zeblst_length)//'event_head.ZEB'
      else
        filnam = d0zeblst(1:d0zeblst_length)//bank//'.ZEB'
      endif
ccccccccc      FILNAM='D0$ZEBLST:'//BANK//'.ZEB'
C
   31 LWV=0
      CONTEXT = 0
      dotpos = index(filnam,'.') + 1
      CALL LIB$FIND_FILE(FILNAM,RES_FILE,CONTEXT)  !PERMITS WILDCARD
c
c     see if res_file is the same as filnam - if so, no good
c
      if ( res_file(dotpos:dotpos).eq.filnam(dotpos:dotpos) ) then
        ierr = 1
        return
      endif
C                                        ! SEARCH LIST
      call lib$find_file_end(context)
      call d0open(lun,res_file,'IN',ok)
cccccc      OPEN(UNIT=LUN,FILE=RES_FILE,STATUS='OLD',READONLY,ERR=101)
      DO 50 LWV = 1, MWV
        READ(LUN,30,END=100) WRUP(LWV)
   30   FORMAT(A80)
        wrup(lwv)(81:84) = cnl
   50 CONTINUE
      CLOSE(UNIT=LUN)
      LWV=MWV
      wrup(lwv)(81:84) = cnull
      call xerrmsg('.ZEB file truncated...')
      GO TO 102
C
  100 CONTINUE
      CLOSE(UNIT=LUN)
      LWV=LWV-1
      wrup(lwv)(81:84) = cnull
      GO TO 102
  101 CONTINUE                          ! Error in reading file
      CLOSE(UNIT=LUN)
      FZEB = FNUL
      LEN3 = INDEX(FILNAM,'  ')         ! LENGTH OF FILNAM
      call xerrmsg(FILNAM(1:LEN3)//' does not exist ')
      CALL GETPAR(1,
     &  ' Give alternate name. <CR> quits) ',
     &  'C',FZEB)
      IF(FZEB.EQ.FNUL)GO TO 103
      CALL UPCASE(FZEB,FZEB)            ! UPPER CASE IT.
      CALL ADDSTR('D0$ZEBLST:',FZEB,FZEBA,LEN3)
      FILNAM = FZEBA
      IF(INDEX(FZEBA,'.ZEB').EQ.0)
     &  CALL ADDSTR(FZEBA,'.ZEB',FILNAM,LEN3)!Adding .Zeb only if needed
      GO TO 31
  102 CONTINUE
      RETURN
  103 IERR = 1
C
  999 RETURN
      END
