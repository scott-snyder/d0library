      SUBROUTINE DCSTOR(LUNDA,FLNAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : store the STP banks to a STPFILE
C-
C-   Inputs  : LUNDA: logical unit number for the output file
C-             FLNAME: output file name
C-   Outputs : none
C-
C-   Created  27-MAR-1992   Qizhong Li-Demarteau
C-   Updated  11-JUL-1992   Qizhong Li-Demarteau  make output in X-mode 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:QUEST.INC/LIST'
      CHARACTER*(*) FLNAME
      CHARACTER*12 XCHOP
      INTEGER LUNDA, NUH, NUV, I, ILEN
      PARAMETER (NUV=1)
      INTEGER IUHEAD(NUV)
      LOGICAL OK
C----------------------------------------------------------------------
C
C ****  Open the file LUNDA
C
      CALL D0OPEN(LUNDA,FLNAME,'OG',OK)
      IF (.NOT. OK) THEN
        CALL ERRMSG('CDWSTP','CDWSTP',
     &    'Unable to open output file CDC_STPFILE.DAT','F')
      ENDIF
      CALL XZRECL(ILEN, XCHOP)
      CALL FZFILE(LUNDA,ILEN,XCHOP)
      IF (IQUEST(1) .NE. 0) THEN
        WRITE (6,*) ' ***** Problem with FZFILE :IQUEST(1)='
     &              , IQUEST(1)
        GO TO 999
      ENDIF
C
C ****  Write the output structure on file LUNDA
C
      CALL FZOUT(LUNDA, IDVSTP, LSCDC, 1, 'D', 2, NUH, IUHEAD)
      IF (IQUEST(1) .NE. 0) THEN
        WRITE ( 6,* ) ' STATUS IQUEST 1-11 TO 17 ',
     &                IQUEST(1),(IQUEST(I),I=11,17)
        CALL DZSTOR(' DUMP STORE', IXSTP)
        CALL DZSURV(' Survey data structure', IXSTP, LSTPH)
      ENDIF
      CALL FZENDO(LUNDA,'T')
C
  999 RETURN
      END
