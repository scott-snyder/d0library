      SUBROUTINE RUNSET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set up to save commands in file instead of
C-                         executing them.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-
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
      CALL GETPAR(1,' Enter name of file to set up > ','C',INTXT)
      IF(PF.EQ.0) THEN
        PRTNAM=NAMLEV(CURLEV)
        IF(PRTNAM.EQ.'MENUDEF') THEN
          PRTNAM=NAMLEV(SAVLEV)(1:TRULEN(NAMLEV(SAVLEV)))//'$MENUDEF'
        ENDIF
C
C     OPEN command file unit
C
        CALL GTUNIT(555,COMUNI,IERR)
        IF(IERR.EQ.0) THEN
          FILNAM=PRTNAM(1:TRULEN(PRTNAM))//'$'//INTXT
          FILNAM=FILNAM(1:TRULEN(FILNAM))//'.INP'
          K=TRULEN(FILNAM)
          CALL D0OPEN(COMUNI, FILNAM(1:K), 'OFL', OK)
          IF(.NOT.OK) THEN
            CALL OUTMSG('0OPEN on COMMAND file->'//FILNAM(1:K)//
     &         '<-failed!'//CHAR(7))
          ELSE
            SETUP=.TRUE.
            OLDLEV=CURLEV
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END
