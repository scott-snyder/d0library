      SUBROUTINE MSGSCR(STATU,TEXT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Decode an error return code "STATU" and put 
C-                         message on screen. VAX-specific
C-
C-   Inputs  : STATU:  Error code to find message for
C-             TEXT:   Extra text to include in message on screen
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER STATU
      CHARACTER*(*) TEXT
C&IF VAXVMS
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER ISTAT,SYS$GETMSG,LIBNXT,TRULEN,I
      CHARACTER*132 BUFFIN
      INTEGER*2 MSIZ
C----------------------------------------------------------------------
      ISTAT=SYS$GETMSG (%VAL(STATU),MSIZ,BUFFIN,%VAL(15),)     ! Decode message
      IF(MSIZ.GT.4.AND.ISTAT.EQ.1) THEN
        I=INDEX(BUFFIN(1:MSIZ),'/')
        IF(I.GT.0) THEN
          MSIZ=I-1
        ENDIF
        IF(INDEX(TEXT,'->').GT.0.OR.INDEX(TEXT,':').GT.0) THEN
          IF(BUFFIN(1:4).EQ.'%SMG') THEN
            WRITE(6,100) TEXT//' '//BUFFIN(1:MSIZ)//CHAR(7)
100         FORMAT('0',A)
            CALL OUTMSG('0 ')
            CALL OUTMSG('  ')
          ELSEIF(FULSCR) THEN
            ISTAT=LIBNXT(TEXT//' '//BUFFIN(1:MSIZ)//CHAR(7),1)    ! Put message on screen
            ISTAT=LIBNXT(' ',0)
          ELSE
            CALL OUTMSG('0'//TEXT//' '//BUFFIN(1:MSIZ)//CHAR(7))   ! Put message on screen
          ENDIF
        ELSE
          IF(BUFFIN(1:4).EQ.'%SMG') THEN
            WRITE(6,100) BUFFIN(1:MSIZ)//TEXT//CHAR(7)
          ELSEIF(FULSCR) THEN
            ISTAT=LIBNXT(BUFFIN(1:MSIZ)//TEXT//CHAR(7)//
     *         '                               ',1)    ! Put message on screen
            ISTAT=LIBNXT(' ',0)
          ELSE
            CALL OUTMSG('0'//BUFFIN(1:MSIZ)//
     *              TEXT(1:TRULEN(TEXT))//CHAR(7))              ! Put message on screen
          ENDIF
        ENDIF
      ENDIF
C&ENDIF
      RETURN
      END
