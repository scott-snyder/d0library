      SUBROUTINE PBD_MSG ( MESSAGE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine outputs the given message to user
C-                         terminal (logical unit 6) and log file ( logical
C-                         unit 4 - must be opened before the call ) if PBD 
C-                         LOG function is enabled.
C-
C-   Inputs  : MESSAGE - Display message to output 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  28-JUN-1991   Hyon Joo Kehayias
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------

      INCLUDE 'D0$PROGRAM_BUILDER$INC:PBD_COMMON.INC'
      CHARACTER*(*) MESSAGE
C
C     Send message to user terminal
C
      WRITE (6,100) MESSAGE
100   FORMAT (1X,A)
C
C     Write message to log file if LOG option specified
C
      IF ( LOG ) THEN
        WRITE (4,200) MESSAGE
200     FORMAT (A)
      END IF

  999 RETURN
      END
