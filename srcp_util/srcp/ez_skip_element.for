      SUBROUTINE EZ_SKIP_ELEMENT(POINTER,LOCAL,ITYPE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given that the POINTER is pointing to the start
C-   of an element in an array of the form:
C-       'Name'  value   'Remark'
C-   this routine will skip that element leaving the POINTER
C-   pointing to the name of the next element in the array.
C-
C-   Inputs  : POINTER [I ]: Position of the element that is going to be
C-                           skipped in the array.
C-             LOCAL   [I*]: Array containing the values in the RCP array
C-             ITYPE   [I*]: Array containing the types in the RCP array
C-
C-   Outputs : POINTER [I ]: Position in the array of the next element
C-                           after the one that has been skipped.
C-
C-   Created  16-MAY-1990   Lupe Howell
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER POINTER, LOCAL(*), ITYPE(*)

      CHARACTER*32 CHARNAM
      INTEGER LENGTH
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
      CALL EZGETC1(LOCAL,ITYPE,POINTER,CHARNAM,LENGTH)    ! Skipping the name
      IF( ITYPE(POINTER) .GT. VTCHR ) THEN          ! Skipping the value
        CALL EZGETC1(LOCAL,ITYPE,POINTER,CHARNAM,LENGTH)
      ELSE
        POINTER = POINTER + 1
      ENDIF
      CALL EZGETC1(LOCAL,ITYPE,POINTER,CHARNAM,LENGTH)      ! Getting remark
  999 RETURN
      END
