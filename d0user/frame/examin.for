      SUBROUTINE EXAMIN(STITLE,COMAND)
C======================================================================
C
C   Purpose and Methods :
C        call menu to plot histograms on screen
C
C   Inputs  :  
C   STITLE = title indicating at what stage it is called
C
C   Output  :
C   COMAND = pass along command when exiting
C
C   Created   1-JUN-1987   Serban D. Protopopescu
C
C======================================================================
      IMPLICIT NONE
      CHARACTER*12 COMAND
      CHARACTER*24 TITLE
      CHARACTER*10 STITLE
      LOGICAL GOTOP,FL,PAUSCH
C======================================================================
C
      TITLE='EXAMINE PLOTS '//STITLE
    2 CALL MENUDO(TITLE,'EXAMINE',COMAND)
      IF(COMAND(1:4).EQ.'EXIT') GOTO 999
      IF(COMAND(1:10).EQ.'END HISPAK') GOTO 999
      IF(COMAND(1:6).EQ.'DHSOPT') GOTO 999
      IF(GOTOP()) GOTO 999
C
      IF(COMAND(1:5).EQ.'PAUSE') THEN
        FL = .NOT.PAUSCH()
        CALL SETPAU (FL)
C
      ELSE IF(COMAND(1:6).EQ.'STATUS') THEN
        CALL USRPST
C
      ELSE
        CALL DSDATA(COMAND)
      ENDIF
      GOTO 2
  999 RETURN
      END
