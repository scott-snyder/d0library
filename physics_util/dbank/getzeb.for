      SUBROUTINE GETZEB(BANK,MWV,WRUP,LWV,IERR)
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
C-   Updated  20-AUG-1992   sss - compile on ibm
C-   Updated   4-JUN-1993   James T. Linnemann  wider filename,help from d0$docs
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) BANK,WRUP(*)
      CHARACTER*20 ZEBNAME
      INTEGER LWV,MWV,LUN,LEN3,INDEX,IERR
      CHARACTER*96 FILNAM,FZEB,FZEBA,FNUL,RES_FILE
      DATA FNUL/' '/
      EXTERNAL LIB$FIND_FILE
      INTEGER CONTEXT
C----------------------------------------------------------------------
C
      IERR = 0
      LUN=13
      IF(BANK.EQ.' ') GO TO 103         ! error return
C
      ZEBNAME = BANK
      IF (BANK.EQ.'HEAD') ZEBNAME = 'EVENT_HEAD'
      FILNAM='D0$ZEBLST:'//ZEBNAME//'.ZEB'
      IF (BANK.EQ.'HELP') FILNAM = 'D0$DOCS:DBANK.DOC'
      CALL UPCASE(FILNAM,FILNAM)    !convert to upper case (don't overwrite
                                    ! input BANK)
C
   31 LWV=0
      CONTEXT = 0
      CALL LIB$FIND_FILE(FILNAM,RES_FILE,CONTEXT)  !PERMITS WILDCARD
C                                        ! SEARCH LIST
      call lib$find_file_end(context)
C&IF IBMAIX
C&      OPEN(UNIT=LUN,FILE=RES_FILE,STATUS='OLD',ERR=101)
C&ELSE
      OPEN(UNIT=LUN,FILE=RES_FILE,STATUS='OLD',READONLY,ERR=101)
C&ENDIF
      DO 50 LWV = 1, MWV
        READ(LUN,30,END=100) WRUP(LWV)
   30   FORMAT(A80)
   50 CONTINUE
      CLOSE(UNIT=LUN)
      LWV=MWV
      CALL OUTMSG(' Truncating file')
      GO TO 102
C
  100 CONTINUE
      CLOSE(UNIT=LUN)
      LWV=LWV-1
      GO TO 102
  101 CONTINUE                          ! Error in reading file
      CLOSE(UNIT=LUN)
      FZEB = FNUL
      LEN3 = INDEX(FILNAM,'  ')         ! LENGTH OF FILNAM
      CALL OUTMSG('  '//FILNAM(1:LEN3)//' does not exist ')
      CALL GETPAR(1,
     &  ' Give alternate name. <CR>: do without .ZEB file) ',
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
