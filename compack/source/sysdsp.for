      SUBROUTINE SYSDSP(COMIN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dispatching routine for system type commands
C-
C-   Inputs  : COMIN: Command to dispatch to (unique identifier)
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-   Updated     7-OCT-1991   Herbert Greenlee
C-   Modified   14-AUG-1992   sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) COMIN
C
C     Get definitions for COMPACK
C
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER SPAWIT,ISTAT,LIBERA,LIBREP,I,LENINT,LININ,LOCMAX
      LOGICAL SETMOD
      CHARACTER*80 PROMPT
C&IF IBMAIX
C&      character*40 parint
C&      integer trulen
C&      external parint, trulen
C&ENDIF
C----------------------------------------------------------------------
C
C     Start the search for predefined commands
C
      IF(INDEX(COMIN,'CALL RUNCOM').GT.0) THEN
        CALL RUNCOM
      ELSEIF(INDEX(COMIN,'CALL RUNSET').GT.0) THEN
        CALL RUNSET
        PF=0                 !Just in case command was aborted
      ELSEIF(INDEX(COMIN,'CALL EVECOM').GT.0) THEN
        CALL EVECOM
        PF=0                 !Just in case command was aborted
      ELSEIF(INDEX(COMIN,'CALL RUNLOG').GT.0) THEN
        CALL RUNLOG
        PF=0                 !Just in case command was aborted
      ELSEIF(INDEX(COMIN,'CALL ENDLOG').GT.0) THEN
        CALL ENDLOG
      ELSEIF(INDEX(COMIN,'CALL ADDITM').GT.0) THEN
        CALL ADDITM
      ELSEIF(INDEX(COMIN,'CALL SPLIT').GT.0) THEN
        IF(STAFLG) THEN
          LOCMAX=PBSAVE-STALIN-7
        ELSE
          LOCMAX=PBSAVE-7
        ENDIF
C&IF IBMAIX   ! losing ibm compiler doesn't have varible formats...
C&        prompt = 'Enter LINES for split screen display (3-' //
C&     &           parint (locmax)(1:trulen (parint (locmax))) //
C&     &           ') [' //
C&     &           parint (spllin)(1:trulen (parint (spllin))) //
C&     &           '] > '
C&ELSE
        WRITE(PROMPT,101) LOCMAX,SPLLIN
  101   FORMAT('Enter LINES for split screen display (3-',
     *          I<LENINT(LOCMAX)>') [',
     *          I<LENINT(SPLLIN)>,'] > ')
C&ENDIF
        LININ=SPLLIN
        CALL GETPAR (1,PROMPT,'I',LININ)
        IF(PF.EQ.0) THEN
          CALL SET_SPLIT_SCREEN(LININ)
          IF(SPLLIN.GE.3.AND.LININ.LE.LOCMAX) THEN
            CALL SPLTIT
          ELSE
            CALL PFWAIT
          ENDIF
        ENDIF
      ELSEIF(INDEX(COMIN,'CALL SPLSTA').GT.0) THEN
        LOCMAX=5
C&IF IBMAIX
C&        prompt = 'Enter LINES for status display (3-' //
C&     &           parint (locmax)(1:trulen (parint (locmax))) //
C&     &           ') [' //
C&     &           parint (spllin)(1:trulen (parint (stalin))) //
C&     &           '] > '
C&ELSE
        WRITE(PROMPT,102) LOCMAX,STALIN
  102   FORMAT('Enter LINES for status display (3-',
     *          I<LENINT(LOCMAX)>') [',
     *          I<LENINT(STALIN)>,'] > ')
C&ENDIF
        LININ=STALIN
        CALL GETPAR (1,PROMPT,'I',LININ)
        IF(PF.EQ.0) THEN
          CALL SETSTA(LININ)
          IF(STALIN.GE.3.AND.STALIN.LE.LOCMAX) THEN
            CALL SPLSTA
          ELSE
            CALL PFWAIT
          ENDIF
        ENDIF
      ELSEIF(INDEX(COMIN,'CALL ENDSPLIT').GT.0) THEN
        CALL ENDSPL
      ELSEIF(INDEX(COMIN,'CALL ENDSTA').GT.0) THEN
        CALL ENDSTA
      ELSEIF(INDEX(COMIN,'CALL SETDIR').GT.0) THEN
        CALL SETDIR
      ELSEIF(INDEX(COMIN,'CALL GOTOP').GT.0) THEN
        TOPGO=.TRUE.
      ELSEIF(INDEX(COMIN,'CALL REFRESH').GT.0) THEN
        ISTAT=LIBREP()
      ELSE
C
C     Not one of the predefined commands, try to spawn a subprocess to do COMIN
C     as a VMS command.
C
        IF(.NOT.SETMOD()) THEN
          ISTAT=SPAWIT(COMIN(6:))   ! TAKE OUT 'fake' CALL
          CALL PFWAIT
        ENDIF
      ENDIF
      RETURN
      END
