      SUBROUTINE SETCOM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialization routine for COMPACK
C-                         Do NOT parse for qualifiers etc.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: Returns immediately if already called
C-
C-   Created   4-NOV-1988   Jan S. Hoftun
C-   Updated   4-Mar-1996   H. Greenlee - lcompack version.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
C----------------------------------------------------------------------
      IF(.NOT.SETDON) THEN               ! Could be called more than once
        SETDON=.TRUE.
        ENDFLG=.TRUE.             ! Default to confirm for exit
        BEGJOU=.FALSE.            ! Default NO journaling
        UPRLEV=0                  ! Start with no levels
        MAILEV=0                  ! Start with no main level
        ENDFLG=.TRUE.
        INPLUN=5
        DEFDIR=' '
C
C     Set up terminal the first time around
C
        CALL REASET
      ENDIF
      RETURN
      END
