      SUBROUTINE GCOMND
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check if COMMAND file qualifier is present
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  25-MAY-1989   Jan S. Hoftun (pulled from SETCHK)
C-
C-      27-Oct-1989 Penelope Constanta-Fanourakis
C-	    Replaced command file opening on LUN 5 with
C-	    opening on LUN COMLUN.
C-	    Make sure to close the file opened on COMLUN
C-	    before opening the new one.
C-
C----------------------------------------------------------------------
C&IF VAXVMS
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INTEGER ISTAT,CLI$PRESENT,CLI$GET_VALUE
      INTEGER TRULEN,K,IOS,I,ISTS,ISTV,IUNI
      EXTERNAL CLI$_PRESENT
      CHARACTER*132 FILNAM
C----------------------------------------------------------------------
      CMDOPEN = .FALSE.
      ISTAT=CLI$PRESENT('COMMAND')
      IF(ISTAT.EQ.%LOC(CLI$_PRESENT)) THEN
        ISTAT=CLI$GET_VALUE('COMMAND',FILNAM)
        K=TRULEN(FILNAM)
	CLOSE(COMLUN)		! Close previous file
        OPEN(COMLUN,FILE=FILNAM(1:K),READONLY,STATUS='OLD',
     &    DEFAULTFILE='.INP',IOSTAT=IOS)
	INPLUN=COMLUN	    ! Make input to be command file
	CMDOPEN = .TRUE.    !Indicate that the file was opened
        IF(IOS.NE.0) THEN
	  INPLUN=5	    !Default INPLUN if open fails
	  CMDOPEN = .FALSE.
          CALL OUTMSG('0OPEN on COMMAND file->'//FILNAM(1:K)//
     &         '<-failed!'//CHAR(7))
          CALL ERRSNS(I,ISTS,ISTV,IUNI,IOS)
          CALL ABOMEN(IOS,FILNAM)
        ELSE
          FSAVE=FULSCR
          FULSCR=.FALSE.
          SAVTRM=TRMFLG
          TRMFLG=.FALSE.                         ! Indicate no prompting
C                                              ! while reading from command file
          RDCOM=.TRUE.
          OLDLEV=1
        ENDIF
      ENDIF
C&ENDIF
  999 RETURN
      END
