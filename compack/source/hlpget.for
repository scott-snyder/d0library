      INTEGER FUNCTION HLPGET(MSGSTR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Capture a line of output help information 
C-                         and put it into HLPINF
C-
C-   Inputs  : MSGSTR :String to be captured.
C-   Outputs : None
C-   Controls: None
C-
C-   Created  10-OCT-1988   Jan S. Hoftun
C-   Modified 16-MAY-1991   Scott Snyder
C-    Use STRAPP.
C-   Updated  12-DEC-1991   Herbert Greenlee
C-    UNIX compatible version.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) MSGSTR
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INTEGER TRULEN
      INTEGER*4 STRAPP
C&IF VAXVMS
C&ELSE
C&      CHARACTER*132 CTEMP
C&ENDIF
C----------------------------------------------------------------------
C&IF VAXVMS
      HELP_COOKIES(MAXLIN(CURLEV), CURLEV) =
     &     STRAPP(HELP_COOKIES(MAXLIN(CURLEV), CURLEV),
     &            MSGSTR//CHAR(13)//CHAR(10))
C&ELSE
C&      CTEMP = MSGSTR//CHAR(13)//CHAR(10)
C&      HELP_COOKIES(MAXLIN(CURLEV), CURLEV) =
C&     &     STRAPP(HELP_COOKIES(MAXLIN(CURLEV), CURLEV),
C&     &            CTEMP)
C&ENDIF
      HLPGET=1
  999 RETURN
      END
