      SUBROUTINE FLGSET (NAME,TRUTH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SET the truth value of flag NAME. Maximum
C-                         name size = 16 characters. The flags are stored
C-                         in private common block /FLAGS/.
C-                         NOTE: /FLAGS/ is not part of the D0 ZEBRA
C-                         structure; therefore, this routine, the
C-                         routine STFLAG and the logical function FLAG
C-                         can be used without ZEBRA.
C-
C-   Inputs  : 
C-             NAME        Name of flag whose truth value is to be set.
C-             TRUTH       Truth value to which flag is to be set.
C-
C-   Outputs : None
C-   Controls: None
C-
C-   Error codes:          Use the logical function FLGERR(IER) to test
C-                         for errors.
C-                         
C-                         IER: 0 ---> Flag found.
C-                             -3 ---> Flag not found.
C-
C-   Created   9-JUL-1988   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) NAME
      INTEGER       ID,N
      LOGICAL       TRUTH,FOUND
      INCLUDE 'D0$INC:FLAGS.INC'
      INCLUDE 'D0$INC:FLAGNM.INC'
C----------------------------------------------------------------------
      N    = LEN(NAME)
      IF ( N .GT. MAXCHR ) N = MAXCHR
C
C ****  SET TRUTH VALUE of flag. Find flag by binary search.
C
      CALL LOCSTR (NAME(1:N),NAMFLG,NUMFLG,FOUND,ID)
      IF ( FOUND ) THEN
        BOOLE(ID) = TRUTH
        ERRFLG = 0
      ELSE
        ERRFLG =-3
      ENDIF
C
  999 RETURN
      END
