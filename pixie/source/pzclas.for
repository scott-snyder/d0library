      SUBROUTINE PZCLAS( CLANUM, CLANAM )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the class number associated with name
C-               CLANUM. Creates a new one if needed.
C-               Entry PZCLNA return the name of a given class number, for
C-               internal use only...
C-
C-   Inputs  : CLANAM [Character] : Name of the class
C-   Outputs : CLANUM [I] : Number to be used for class reference.
C-
C-   Created  20-JAN-1988   Olivier Callot
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PZCOMN.INC'
      INTEGER CLANUM, I
      CHARACTER*(*) CLANAM
      CHARACTER*16  CLNAML(NMXCLA), CURNAM
C----------------------------------------------------------------------
      CURNAM = CLANAM
      IF( CURNAM .eq. ' ' ) CURNAM = '*PZCLAS no name*'
C
C ****  Check if this name is already used. If so, keep class number
C
      DO 10 I = 1, MAXCLA
        IF ( CURNAM .EQ. CLNAML(I) ) THEN
          CLANUM = I
          CURCLA = CLANUM
          CALL PZFREE( CLANUM )
          GOTO 999
        ENDIF
   10 CONTINUE
C
C ****  Creates a new class if possible
C
      IF ( MAXCLA .GE. NMXCLA ) THEN
        WRITE( 6, 1000 ) CURNAM
 1000   FORMAT('0** PZCLAS ** Can not open a new class with name',A//
     &    13X,'Dump of class-segment structure follows'/)
        CALL PZDUMP( 6 )
        CALL EXIT
      ENDIF
C
C ****  OK, creates the class
C
      MAXCLA = MAXCLA + 1
      CURCLA = MAXCLA
      CLNAML( CURCLA ) = CURNAM
  999 RETURN
C
C ****  ENTRY PZCLNA. Same args, but reverse the input/output
C
      ENTRY PZCLNA( CLANUM, CLANAM )
C
      IF ( CLANUM .LE. MAXCLA ) THEN
        CLANAM = CLNAML( CLANUM )
      ELSE
        CLANAM = '...Undefined...'
      ENDIF
      RETURN
      END
