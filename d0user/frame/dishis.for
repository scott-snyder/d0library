      SUBROUTINE DISHIS(COMAND)
C---------------------------------------------------------------------
C-                                                                   -
C-      Dispatching subroutine for HISPAK INTERRUPT menu             -
C-                                                                   -
C-     INPUT:                                                        -
C-     COMAND= comand to be executed                               -
C-                                                                   -
C-                       SDP March,1987                              -
C-                                                                   -
C---------------------------------------------------------------------
C
      IMPLICIT NONE
      CHARACTER*(*)  COMAND
      LOGICAL FL,FLGVAL
C
      IF(COMAND(1:6).EQ.'STATUS') THEN
        CALL FLGSET('STATUS',.TRUE.)
C
      ELSE IF(COMAND(1:10).EQ.'END HISPAK') THEN
        CALL SETFLG
        CALL FLGSET('CANCEL_EXAMINE',.TRUE.)
        CALL D0HPLT('END HISPAK')
C
      ELSE IF(COMAND(1:5).EQ.'PAUSE') THEN
        FL = .NOT.FLGVAL('PAUSE')
        CALL SETFLG
        CALL FLGSET('PAUSE',FL)
C
      ELSE
        CALL D0HPLT(COMAND)
      ENDIF
      CALL SETINQ(.TRUE.)
C
      RETURN
      END      
