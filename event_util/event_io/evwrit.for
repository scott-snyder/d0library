      SUBROUTINE EVWRIT
C------------------------------------------------------------
C-                                                          -
C-     handle writing request for single events             -
C-     user must define and set flag WRITE_EVENT            -
C-                                                          -
C-     ENTRY EVWRIT_FILE_EXT(FILE_EXT)
C-       files written have default form E_run#_id#.DAT
C-       this entry point allows changing extension to an arbitrary value
C-     Input:
C-       FILE_EXT= file extension
C-
C-     Serban Protopopescu Nov.1,1988                       -
C-                                                          -
C------------------------------------------------------------
C 
      IMPLICIT NONE
      CHARACTER*(*) FILE_EXT
      LOGICAL FIRST,FLGVAL,OK
      CHARACTER*24 FILNAM,EXT
      CHARACTER*12 XCHOPT
      INTEGER IU,IERR,N,IRECL
      DATA FIRST,IU/.TRUE.,-10/
      DATA EXT/'.DAT'/
C------------------------------------------------------------
C
      IF(FLGVAL('WRITE_EVENT')) THEN 
        IF (FLGVAL('WRITE_STREAM_EDS')) THEN
          CALL FLSETS('WRITE_STREAM_',.FALSE.)  ! turn off all output streams
          CALL FLGSET('WRITE_STREAM_EDS',.TRUE.)
          CALL EVTWOS
          CALL FLSETS('WRITE_STREAM_',.TRUE.)   ! turn on all output streams
          CALL INTMSG(' Event written to EDS output stream')
        ELSE
          IF(FIRST) THEN
            CALL GTUNIT(1,IU,IERR)
            FIRST=.FALSE.
          ENDIF
          CALL EVENT_FILE(FILNAM,N)
          FILNAM=FILNAM(1:N)//EXT
          CALL D0OPEN(IU,FILNAM,'XO',OK)
          IF(.NOT.OK) GOTO 200
          CALL XZRECL(IRECL,XCHOPT)
          CALL FZFILE(IU,IRECL,XCHOPT)
          CALL WREVNT(IU)
          CALL FZENDO(IU,'T')
          CLOSE(IU)
          CALL INTMSG(' File '//FILNAM//' written.')
        ENDIF
        CALL FLGSET('WRITE_EVENT',.FALSE.)
      ENDIF
      RETURN
C
  200 CALL INTMSG(' Cannot open '//FILNAM)
      GOTO 999  ! return
C
C
      ENTRY EVWRIT_FILE_EXT(FILE_EXT)
      EXT='.'//FILE_EXT
  999 RETURN
      END      
