      SUBROUTINE ENSTOH (PRG,FILNAM,SWITCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Store ALL histograms.
C-
C-   Inputs  : PRG       Name of ENDTSK function
C-             FILNAM    Name of output file
C-
C-   Outputs : None
C-
C-   Controls: SWITCH    'L' Line-printer, 'P' Plotter
C-
C-   Created  21-MAR-1988   Harrison B. Prosper
C-   Modified 21-MAR-1988
C-   Updated   8-MAR-1989   Harrison B. Prosper
C-                          Call D0OPEN
C-   Updated  11-Mar-1992   Herbert B. Greenlee
C-      Get rid of machine blocks.  Fix concatenations.
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PRG,FILNAM,SWITCH
      LOGICAL       OK
      INTEGER       L,N,LUN,ERR,PROGID
      CHARACTER*(*) ERRMSG
      CHARACTER*1   MODE
      CHARACTER*132 CTEMP
      INTEGER TRULEN
      PARAMETER( ERRMSG = ' %ENSTOH-E-' )
C----------------------------------------------------------------------
C
      L = LEN(FILNAM)
      N = LEN(PRG)
      CALL UPCASE (SWITCH(1:1),MODE)
C
      CALL ENPROG (PROGID)
      CALL GTUNIT (PROGID,LUN,ERR)
C
      IF ( MODE .EQ. 'P' ) THEN
        OK = .TRUE.
        CALL D0OPEN (LUN,FILNAM(1:L),'OU',OK)
        IF ( OK ) THEN
          CALL HSTORE (0,LUN)
          CLOSE (UNIT=LUN)
          CTEMP = PRG(1:N)//' Histograms stored on file: '//FILNAM(1:L)
          CALL INTMSG(CTEMP(1:TRULEN(CTEMP)))
        ELSE
          CTEMP = ERRMSG//'OPENFILE; Histograms NOT stored'
          CALL INTMSG(CTEMP(1:TRULEN(CTEMP)))
        ENDIF
      ELSE
        OK = .TRUE.
        CALL D0OPEN (LUN,FILNAM(1:L),'O',OK)
        IF ( OK ) THEN
          CALL HOUTPU (LUN)
          CALL HPRINT (0)
          CLOSE (UNIT=LUN)
          CTEMP = PRG(1:N)//' Histograms stored on file: '//FILNAM(1:L)
          CALL INTMSG(CTEMP(1:TRULEN(CTEMP)))
        ELSE
          CTEMP = ERRMSG//'OPENFILE; Histograms NOT stored'
          CALL INTMSG(CTEMP(1:TRULEN(CTEMP)))
        ENDIF
      ENDIF
C
      CALL RLUNIT (PROGID,LUN,ERR)
C
  999 RETURN
      END
