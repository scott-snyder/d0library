      SUBROUTINE GTUNIT (IUSER,IUNIT,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get an available unit number for a FORTRAN
C-                         program.
C-
C-   Inputs  : IUSER: User identifier (so RLUNIT checks who's reserved it)
C-   Outputs : IUNIT: Unit number assigned
C-             IERR:  Possible error code
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C       ROUTINE ASSIGNS UNIT NUMBER 'IUNIT' FOR USE BY
C       USER 'IUSER'.  WHEN UNIT NUMBER IS NO LONGER
C       NEEDED IT SHOULD BE RELEASE WITH RLUNIT CALL
C       WITH SAME ARGUMENTS.  VALID IUSER NUMBERS ARE
C       BETWEEN 1 AND 1000000.  IF YOU HAVE NOT BEEN EXPLICITLY
C       ASSIGN AN IUSER NUMBER, PICK A RANDOM NUMBER BETWEEN 100
C       AND 1000. IF 'IERR' IS NOT ZERO ON RETURN THERE IS A PROBLEM.
C

      INTEGER IBASE
      PARAMETER (IBASE=20)

      INTEGER IUN(100),I,II,IUSER,IUNIT,IERR
      DATA IUN/100*0/

C----------------------------------------------------------------------
      IERR=4
      IF ((IUSER .LT. 1) .OR. (IUSER .GT. 1000000)) RETURN
      IERR=0

      DO 1 I=IBASE,99
        II=I
        IF (IUN(I) .EQ. 0) GO TO 2
    1 CONTINUE
      IERR=1
      RETURN

    2 IUNIT=II
      IUN(II)=IUSER
      IERR=0
      RETURN

      ENTRY RLUNIT (IUSER,IUNIT,IERR)
C
C       DEASSIGNS UNIT NUMBER 'IUNIT' PREVIOUSLY ASSIGNED TO
C       USER 'IUSER' MAKING IT AVAILABLE FOR OTHER USES.
C       YOU MUST DEASSIGN A UNIT NUMBER USING THE SAME 'IUSER'
C       WITH WHICH IT WAS ASSIGNED.
C

      IERR=2
      IF ((IUNIT .GT. 99) .OR. (IUNIT .LT. IBASE)) RETURN

      IERR=3
      IF (IUSER .NE. IUN(IUNIT)) RETURN

      IERR=0
      IUN(IUNIT)=0
      RETURN

      END
