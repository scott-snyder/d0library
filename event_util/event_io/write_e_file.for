      SUBROUTINE WRITE_E_FILE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      write one event to temporary file
C-      name of file is E_run#_id#.DAT
C-
C-   ENTRY CLOSE_E_FILE
C-      close and delete temporary file
C-
C-   Created  22-JAN-1991   Serban D. Protopopescu
C-   Updated  23-OCT-1992   Serban Protopopescu  use X mode &
C-                                               close immediately for UNIX 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*24 FILNAM
      CHARACTER*12 XCHOPT
      INTEGER RUN,ID,N,IER,EUNIT,IRECL
      LOGICAL FIRST,OK
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        CALL GTUNIT(23,EUNIT,IER)
        FIRST=.FALSE.
      ENDIF
C
      CALL EVENT_FILE(FILNAM,N)
      FILNAM=FILNAM(1:N)//'.DAT'
      CALL D0OPEN(EUNIT,FILNAM,'XO',OK)
      CALL XZRECL(IRECL,XCHOPT)
      CALL FZFILE(EUNIT,IRECL,XCHOPT)
      IF(OK) THEN
        CALL WREVNT(EUNIT)
      ELSE
        CALL ERRMSG('WRITE_E_FILE',' Cannot open '//FILNAM,' ','W')
      ENDIF
      CALL FZENDO(EUNIT,'T')
      CLOSE(EUNIT)
C
  999 RETURN
C
      ENTRY CLOSE_E_FILE
      CALL EVENT_FILE(FILNAM,N)
      FILNAM=FILNAM(1:N)//'.DAT'
      CALL D0OPEN(EUNIT,FILNAM,'IOU',OK)
      CLOSE(EUNIT,STATUS='DELETE')
      RETURN
      END
