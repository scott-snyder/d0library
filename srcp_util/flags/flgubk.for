      SUBROUTINE FLGUBK (NAME,NUMBER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-                         Unbook a set of named logical flags. Maximum
C-                         name size = 16 characters. The flags are
C-                         stored in private common block /FLAGS/.
C-                         NOTE: /FLAGS/ is not part of the D0 ZEBRA
C-                         structure; therefore, this routine, the
C-                         routine STFLAG and the logical function FLAG
C-                         can be used without ZEBRA.
C-
C-   Inputs  : 
C-             NAME(*)     Array of flag names
C-             NUMBER      Number of named flags
C-
C-   Outputs : None
C-   Controls: None
C-
C-   Error codes:          Use logical function FLGERR (IER) to test for
C-                         errors.
C-
C-                         IER: 0 ---> Unbooking successful.
C-                              N ---> The Nth flag could not be found.
C-
C-   Created   9-JUL-1988   Harrison B. Prosper
C-   Updated  15-OCT-1991   Sharon Hagopian, zero remaining BOOLE array
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER       NUMBER,I,J,N,ID
      CHARACTER*(*) NAME(*)
      LOGICAL       FOUND
      INCLUDE 'D0$INC:FLAGS.INC'
      INCLUDE 'D0$INC:FLAGNM.INC'
C----------------------------------------------------------------------
C
C ****  Unbook flag
C
      N = LEN(NAME(1))
      IF ( N .GT. MAXCHR ) N = MAXCHR
C
      ERRFLG = 0
      DO 100 I =  1,NUMBER  
        CALL LOCSTR (NAME(I)(1:N),NAMFLG,NUMFLG,FOUND,ID)
        IF ( FOUND ) THEN
          NUMFLG = NUMFLG - 1
          DO 90 J =  ID,NUMFLG
            NAMFLG(J) = NAMFLG(J+1)
            BOOLE(J)  = BOOLE(J+1)
   90     CONTINUE
        ELSE
          ERRFLG = I
          GOTO 999
        ENDIF
  100 CONTINUE
      DO 200 I= NUMFLG+1,MAXFLG
  200 BOOLE(I)=.FALSE.
C
  999 RETURN
      END
