      LOGICAL FUNCTION FLGCHK(NAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Check if flag NAME exists, returns false if never booked,
C-     true otherwise.
C-
C-   Inputs  : 
C-   NAME = name of flag to be checked
C-
C-   Created  28-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) NAME
      INTEGER       ID,N
      LOGICAL       FOUND
      INCLUDE 'D0$INC:FLAGS.INC'
      INCLUDE 'D0$INC:FLAGNM.INC'
C----------------------------------------------------------------------
C
      N    = LEN(NAME)
      IF ( N .GT. MAXCHR ) N = MAXCHR
C
C ****  Find flag by binary search.
C
      CALL LOCSTR (NAME(1:N),NAMFLG,NUMFLG,FOUND,ID)
      IF ( FOUND ) THEN
        FLGCHK = .TRUE.
      ELSE
        FLGCHK=.FALSE.
      ENDIF
C
  999 RETURN
      END
