      SUBROUTINE FLGBK (NAME,NUMBER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-                         Book a set of named logical flags. Maximum
C-                         name size = 16 characters. The flags are
C-                         stored in private common block /FLAGS/.
C-                         NOTE: /FLAGS/ is not part of the D0 ZEBRA
C-                         structure; therefore, this routine, the
C-                         routine STFLAG and the logical function FLAG
C-                         can be used without ZEBRA.
C-
C-                         The routine only books flags if they have not
C-                         already been booked and if there is enough
C-                         room left in the flag buffer.
C-
C-   Inputs  : 
C-             NAME(*)     Array of flag names
C-             NUMBER      Number of named flags
C-
C-   Outputs : None
C-   Controls: None
C-
C-   Error codes:          
C-                         IER: 0 ---> Booking successful.
C-                             -1 ---> No flags booked; not enough room.
C-                             -2 ---> No flags booked; flag buffer full.
C-                             >0 ---> DUPLICATED flag 
C-
C-    An error will trigger a call to FLGABO and abort.
C-    FLGABO will write file FLAGS.LIST listing all booked flags.
C-
C-   Created   9-JUL-1988   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER       NUMBER,I,J,N,ID
      CHARACTER*(*) NAME(*)
      LOGICAL       FOUND,FIRST,LTEMP
      CHARACTER*80  TEMP
      INCLUDE 'D0$INC:FLAGS.INC'
      INCLUDE 'D0$INC:FLAGNM.INC'
      DATA FIRST/.TRUE./
      SAVE FIRST
C----------------------------------------------------------------------
C
C ****  Initialize flags
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        NUMFLG= 0
        DO 50 I =  1,MAXFLG
          BOOLE(I) = .FALSE.
   50   CONTINUE
      ENDIF
C
C ****  Check if flag buffer is full.
C
      ERRFLG = 0
      IF ( NUMFLG .GE. MAXFLG ) THEN
        ERRFLG =-2
        TEMP=' FLAG ERROR: Exceeded maximum number of flags available'
        CALL FLGABO(TEMP)
      ENDIF
C
C ****  Only book flags if there is enough room to book ALL of them.
C
      IF ( NUMBER .GT. (MAXFLG-NUMFLG) ) THEN
        ERRFLG =-1
        TEMP=' FLAG ERROR: Exceeded maximum number of flags available'
        CALL FLGABO(TEMP)
      ENDIF
C
C ****  Book flag if not already booked.
C
      N = LEN(NAME(1))
      IF ( N .GT. MAXCHR ) N = MAXCHR
C
      DO 100 I =  1,NUMBER  
        CALL LOCSTR (NAME(I)(1:N),NAMFLG,NUMFLG,FOUND,ID)
        IF ( FOUND ) THEN
          ERRFLG = I
          TEMP=' FLAG ERROR: Flag '//NAME(I)//' already booked'
          CALL FLGABO(TEMP)
        ELSE
          NUMFLG = NUMFLG + 1
          NAMFLG(NUMFLG) = NAME(I)
        ENDIF
  100 CONTINUE
C
C ****  Sort flag names into alphabetical order; use Tree sort.
C
      DO 300 I = 1,NUMFLG
        DO 200 J = 1,NUMFLG
          IF(NAMFLG(I).GT.NAMFLG(J)) GOTO 200
          TEMP = NAMFLG(I)
          NAMFLG(I) = NAMFLG(J)
          NAMFLG(J) = TEMP
          LTEMP = BOOLE(I)
          BOOLE(I) = BOOLE(J)
          BOOLE(J) = LTEMP
  200   CONTINUE
  300 CONTINUE
C
  999 RETURN
      END
