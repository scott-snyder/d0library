      LOGICAL FUNCTION FLGERR (IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return error code for FLAG routines; ERFLAG
C-                         is TRUE if IER is non-zero, that is, if an
C-                         error has occurred.
C-
C-   Inputs  : None
C-   Outputs : IER         Error code.
C-                          0 ---> OK.
C-                         -1 ---> Not enough room to book ALL flags.
C-                         -2 ---> No more room left to book flags.
C-                         -3 ---> Flag not found.
C-                          N ---> The Nth flag has already been booked.
C-                                 OR could not be found.
C-   Controls: None
C-
C-   Created   9-JUL-1988   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER       IER
      INCLUDE 'D0$INC:FLAGS.INC'
C----------------------------------------------------------------------
      IER    = ERRFLG
      FLGERR = IER .NE. 0
  999 RETURN
      END
