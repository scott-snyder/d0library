      SUBROUTINE LOCSTR1(NAME,LIST,IDLIST,NLIST,NEW,II)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the location, II, of the string NAME 
C-   in the list of strings LIST(i) with id-list IDLIST(i). 
C-   
C-   The ID of the string is given by IDLIST(II); it is just the order
C-   in which the string NAME was added to the list of strings.
C-   LOCSTR1 sorts the list, if needed, and performs a binary search on 
C-   LIST(i) to find the location II of NAME within LIST(i).
C-   
C-   1) If the string
C-   NAME is not amongst the strings LIST(i) then NLIST is incremented 
C-   and by one, NAME is assigned the ID NLIST, and the new string is
C-   added to the list of strings. NEW is then set to TRUE to
C-   indicate that NAME is a new string.
C-   
C-   2) To initialize the list set NLIST = 0.
C-   
C-   It is important to ensure that the values of NLIST, IDLIST(i) and 
C-   LIST(i) are preserved by the calling routine.
C-
C-   Inputs  : NAME     [C*]    String whose id is to be returned
C-             LIST(*)  [C*]    List of strings to search
C-             IDLIST(*)[I]     List of string id
C-             NLIST    [I]     Number of strings in list
C-
C-   Outputs : LIST(*)  [C*]    Orderd list of strings
C-             IDLIST(*)[I]     Ordered list of string id
C-             NLIST    [I]     Number of strings in list
C-             NEW      [L]     TRUE if NAME is a new string
C-             II       [I]     ID of string NAME
C-   Controls:
C-
C-   Created  20-MAY-1991   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) NAME,LIST(*)
      INTEGER IDLIST(*),NLIST
      LOGICAL NEW
      INTEGER II
C----------------------------------------------------------------------
      INTEGER L
      LOGICAL FOUND
C----------------------------------------------------------------------
      L = LEN(NAME)
C
C ****  Check whether to initialize list
C
      IF ( NLIST .LE. 0 ) THEN
C
C ****  FIRST string
C
        NLIST = 1
        II    = NLIST
        NEW   = .TRUE.
        LIST(NLIST)   = NAME(1:L)
        IDLIST(NLIST) = NLIST
C
      ELSE
C
        CALL LOCSTR(NAME(1:L),LIST,NLIST,FOUND,II)
        NEW = .NOT. FOUND
C
        IF ( NEW ) THEN
C
C ****  Add to list
C
          NLIST = NLIST + 1
          LIST(NLIST)   = NAME(1:L)
          IDLIST(NLIST) = NLIST
          CALL SRTCHR(LIST,NLIST,IDLIST)   ! Sort list
          CALL LOCSTR(NAME(1:L),LIST,NLIST,FOUND,II)
        ENDIF
      ENDIF
  999 RETURN
      END
