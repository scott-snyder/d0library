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
      INTEGER SPAWIT,ISTAT,LIBREP,I,LENINT,LININ,LOCMAX
      LOGICAL SETMOD
      CHARACTER*80 PROMPT
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
      ELSEIF(INDEX(COMIN,'CALL SETDIR').GT.0) THEN
        CALL SETDIR
      ELSEIF(INDEX(COMIN,'CALL GOTOP').GT.0) THEN
        TOPGO=.TRUE.
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
