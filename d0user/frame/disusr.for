      SUBROUTINE DISUSR(COMMAND)
C---------------------------------------------------------------------
C-                                                                   -
C-      Dispatching subroutine for D0USER event interrupt            -
C-                                                                   -
C-     INPUT:                                                        -
C-     COMMAND= command to be executed                               -
C-                                                                   -
C-                       SDP Oct,1986  Jan.,1987                     -
C-                                                                   -
C---------------------------------------------------------------------
C
      IMPLICIT NONE
      LOGICAL FL,FLGVAL
      CHARACTER*(*)  COMMAND
C
      IF(COMMAND(1:5).EQ.'PAUSE') THEN
        FL=.NOT.FLGVAL('PAUSE')
        CALL SETFLG
        CALL FLGSET('PAUSE',FL)
C
      ELSE IF(COMMAND(1:6).EQ.'DMPREQ') THEN
        CALL DMPREQ
C
      ELSE IF(COMMAND(1:6).EQ.'DMPDEF') THEN
        CALL DMPDEF
C
      ELSE IF(COMMAND(1:10).EQ.'USER PAUSE')  THEN
        CALL SETFLG
        CALL FLGSET('USER_PAUSE',.TRUE.)
C
      ELSE IF(COMMAND(1:7).EQ.'EXAMINE') THEN 
        CALL SETFLG
        CALL FLGSET('EXAMINE',.TRUE.)
        CALL FLGSET('CANCEL_INTER',.TRUE.)
C
      ELSE IF(COMMAND(1:6).EQ.'STATUS')   THEN
        CALL FLGSET('STATUS',.TRUE.)
C
      ELSE IF(COMMAND(1:13).EQ.'EVENT DISPLAY') THEN
        CALL FLGSET('EVENT_DISPLAY',.TRUE.)
C
      ELSE IF(COMMAND(1:11).EQ.'WRITE EVENT') THEN
        CALL FLGSET('WRITE_EVENT',.TRUE.)
C
      ELSE IF(COMMAND(1:9).EQ.'SUMMARIES') THEN 
        CALL SETFLG
        CALL FLGSET('SUMMARIES',.TRUE.)
C
      ELSE IF(COMMAND(1:6).EQ.'NOMORE') THEN
        CALL SETFLG
        CALL SETNMR(.TRUE.)
C
      ELSE IF(COMMAND(1:4).EQ.'QUIT') THEN
        CALL SETFLG
        CALL FLGSET('QUIT',.TRUE.)
C
      ENDIF
C
      CALL SETINQ(.TRUE.)      ! set flag to indicate made a request 
      RETURN
      END      
