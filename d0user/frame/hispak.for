      SUBROUTINE HISPAK(INTER)
C------------------------------------------------------
C-                                                    -
C-     Interface to D0HPLT
C-                                                    -
C-      INPUT:                                        -
C-      INTER = true if in interrupt mode
C-                                                    -
C-    SDP March,1987                                   -
C-    Modified from DHS to D0HPLT Sept.,1988
C-
C------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CANCEL,INTAST,FLGVAL,INTER,GOTOP
      CHARACTER*40 TITLE
      CHARACTER*16 COMAND
      EXTERNAL DISHIS
C
      IF(INTER) THEN
C
C       interrupt menu is available or wanted
C
        IF(.NOT.INTAST()) THEN       ! setup interrupt menu
C
          CALL INTMSG('  >>>Use line mode for TYPE command<<<')
          TITLE=' D0USER HISPAK INTRPT '
          CALL INTMEN(TITLE,'EXAMINE',DISHIS)
        ENDIF
        CALL D0HPLE
C
C        no interrupt menu
C
      ELSE
      CALL INTMSG('  >>>Use line mode for TYPE command<<<')
    1 TITLE='D0H OPTIONS '
      CALL MENUDO(TITLE,'D0H_OPTIONS',COMAND)
C
      IF(COMAND(1:4).EQ.'EXIT') GOTO 999    ! return
      IF(GOTOP()) GOTO 999
C
      IF(COMAND(1:6).EQ.'STATUS') THEN
        CALL USRPST
C
      ELSE IF(COMAND(1:6).EQ.'USDIAL') THEN
        CALL USDIAL
C
      ELSE IF(COMAND(1:4).EQ.'QUIT') THEN
        CALL QUIT
C
      ELSE
        CALL D0HPLD(COMAND)
      ENDIF
C
      GOTO 1
C
      ENDIF
  999 RETURN
      END
