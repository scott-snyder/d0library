      SUBROUTINE RUNLOG
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set up logging of commands in command file.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: Command file is opened and LOGUP flag set to .TRUE.
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-
C-   Updated   1-APR-1992   Harrison B. Prosper
C-    Add carriagecontrol='LIST'
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      CHARACTER*132 FILNAM,TRANUP
      INTEGER IERR,TRULEN,I,IOS,K
      INTEGER ISTS,ISTV,IUNI
      CHARACTER*32 INTXT,PRTNAM
      LOGICAL OK
C----------------------------------------------------------------------
      CALL GETPAR(1,' Enter name of file to save commands in > ',
     *     'C',INTXT)
      IF(PF.EQ.0) THEN
        PRTNAM=NAMLEV(CURLEV)
        IF(PRTNAM.EQ.'MENUDEF') THEN
          PRTNAM=NAMLEV(SAVLEV)(1:TRULEN(NAMLEV(SAVLEV)))//'$MENUDEF'
        ENDIF
        CALL GTUNIT(555,COMUNI,IERR)
        IF(IERR.EQ.0) THEN
C
C     OPEN command file unit
C
          FILNAM=PRTNAM(1:TRULEN(PRTNAM))//'$'//INTXT
          FILNAM=FILNAM(1:TRULEN(FILNAM))//'.INP'
          K=TRULEN(FILNAM)
          CALL D0OPEN(COMUNI, FILNAM(1:K), 'OFL', OK)
          IF(.NOT.OK) THEN
            CALL OUTMSG('0OPEN on COMMAND file->'//FILNAM(1:K)//
     &         '<-failed!'//CHAR(7))
          ELSE
            LOGUP=.TRUE.
            OLDLEV=CURLEV
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END
