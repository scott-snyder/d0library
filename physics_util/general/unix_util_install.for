C------------------------------------------------------------------------
      LOGICAL FUNCTION UNIX_UTIL_INSTALL
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Initialization routine for UNIX_UTILITY pkg.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   3-May-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
C&IF VAXVMS
C&ELSE
C&      INTEGER IER,MAKE_DUMP
C&      LOGICAL DO_SEGV,NO_COREDUMP,DO_CLEANSTOP
C&      LOGICAL FIRST
C&C-
C&      DATA FIRST/.TRUE./
C&C----------------------------------------------------------------------
C&      IF(FIRST) THEN
C&C-
C&C- Read RCP parameters from FLUSHER_RCP.
C&C-
C&        CALL EZPICK_NOMSG('UNIX_UTILITY_RCP', IER)
C&        IF(IER.NE.0)THEN
C&          CALL INRCP('UNIX_UTILITY_RCP', IER)
C&          CALL EZPICK_NOMSG('UNIX_UTILITY_RCP', IER)
C&        ENDIF
C&        IF(IER.EQ.0)CALL EZGET('INSTALL_SEGV_TRACEBACK', DO_SEGV, IER)
C&        IF(IER.EQ.0)CALL EZGET('DISABLE_COREDUMP',NO_COREDUMP,IER)
C&        IF(IER.EQ.0)CALL EZGET('INSTALL_CLEANSTOP', DO_CLEANSTOP, IER)
C&        CALL EZRSET
C&        IF (IER.NE.0) CALL ERRMSG('Error in UNIX_UTILITY_RCP',
C&     &    'FLUSHER',' ','F')
C&        FIRST=.FALSE.
C&      ENDIF
C&C-
C&      MAKE_DUMP=1
C&      IF(NO_COREDUMP)  MAKE_DUMP=0
C&      IF(DO_SEGV)      CALL SEGV_INSTALL(MAKE_DUMP)
C&      IF(DO_CLEANSTOP) CALL STOPME_INSTALL
C&C-
C&C- End of initialization
C&C-
C&ENDIF
      UNIX_UTIL_INSTALL=.TRUE.
  999 RETURN
      END
