      SUBROUTINE EXCLUDE_STRINGS (EXLIST,NEXLIS,WORK,LIST,NLIST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Exclude from a given list of strings LIST(i)
C-                         a specified set of strings EXLIST(i). The
C-                         list EXLIST(i) is appended to the list LIST(i)
C-                         and the latter is sorted with SRTCHR. The sorted
C-                         list is then scanned and a new list is created
C-                         skipping the single or double entries of strings
C-                         belonging to the list EXLIST(i).
C-
C-   Inputs  : EXLIST(*)        [C*]    List of strings to be excluded
C-             NEXLIS           [I]     Number of such strings.
C-             WORK(*)          [I]     Array for internal use. Must be
C-                                      contain at least NLIST+NEXLIS 
C-                                      elements.
C-             LIST(*)          [C*]    List of strings from which strings
C-                                      are to be excluded.
C-             NLIST            [I]     Number of such strings.
C-                                      NOTE: The arrays LIST and WORK must
C-                                      be dimensioned to a length at least
C-                                      equal to NLIST+NEXLIS.
C-
C-   Outputs : LIST(*)          [C*]    Modified list of strings
C-             NLIST            [I]     Number of strings
C-   Controls: None
C-
C-   Created  10-AUG-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) EXLIST(*)
      INTEGER       NEXLIS
      INTEGER       WORK(*)
      CHARACTER*(*) LIST(*)
      INTEGER       NLIST
C
      INTEGER I,J,N
C----------------------------------------------------------------------
C
C ****  Initialize array WORK
C
      N = NLIST + NEXLIS
      DO I =  1, N
        WORK(I) = I
      ENDDO
C
C ****  Append strings to be excluded to LIST
C
      DO I =  NLIST+1,N
        LIST(I) = EXLIST(I-NLIST)
      ENDDO
C
C ****  Sort strings
C
      CALL SRTCHR (LIST,N,WORK)
C
C ****  Remove double occurrences of strings from the sorted
C ****  list of strings LIST if and only if one member of
C ****  the pair originated from the exclusion list EXLIST
C ****  and exclude single occurrences of strings which
C ****  originate from EXLIST.
C
      I = 0
      J = 0
      DO WHILE ( I .LT. N - 1 )
        I = I + 1
C
C ****  Compare adjacent strings. If they are not equal AND the first
C ****  string is NOT from the exclusion list then add it to the new list.
C ****  If they are equal exclude both only if one string originated
C ****  from the exclusion list.
C
        IF ( LIST(I) .NE. LIST(I+1) ) THEN
          IF ( WORK(I) .LE. NLIST ) THEN
            J = J + 1
            LIST(J) = LIST(I)
          ENDIF
        ELSE
          IF ( (WORK(I) .GT. NLIST) .OR. (WORK(I+1) .GT. NLIST) ) THEN
            I = I + 1                   ! Skip the double entry
          ENDIF
        ENDIF
C
      ENDDO
C
C ****  If I = N   then last two strings have been skipped.
C ****  If I = N-1 then last string not yet checked.
C
      IF ( I .EQ. (N - 1) ) THEN
        IF ( WORK(N) .LE. NLIST ) THEN
          J = J + 1
          LIST(J) = LIST(N)
        ENDIF
      ENDIF
      NLIST = J
C
  999 RETURN
      END
