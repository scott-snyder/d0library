      SUBROUTINE ERRINI( LOG, WRN )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To determine which logging unit and whether
C-                         to send warnings for messages of level W, E
C-                         and F. 
C-
C-   Inputs  : LOG         Logical unit for logging all messages
C-             WRN         .FALSE. = only count, and sent to logging unit
C-                         .TRUE.  = sent via ERRWRN, default is .TRUE.
C-
C-   Outputs : None
C-
C-   Controls: None
C-
C-   Created  18-DEC-1988   Jun-Jang Jeng (MSU)
C-   Updated   3-JAN-1989   James T. Linnemann   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LOG
      LOGICAL WRN
      INCLUDE 'D0$ERRMSG_UTIL$PARAMS:ERRMSG.PARAMS'
      INCLUDE 'D0$INC:ERRCTL.INC'
C
      CALL ERRINT
      LULOG = LOG
      WARN = WRN
C
  999 RETURN
      END
