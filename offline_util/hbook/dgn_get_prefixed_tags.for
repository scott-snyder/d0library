C CMS REPLACEMENT HISTORY, Element DGN_GET_PREFIXED_TAGS.FOR
C *1    13-JAN-1996 13:12:16 HARRY "Update"
C CMS REPLACEMENT HISTORY, Element DGN_GET_PREFIXED_TAGS.FOR
      SUBROUTINE DGN_GET_PREFIXED_TAGS(PREFIX1,MAXN,NNAME,NAME,INAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return all names prefixed with the specified
C-   prefix.
C-
C-   Inputs  : PREFIX1   [C*]  Prefix 
C-             MAXN      [I]   Maximum number of names to return
C-            
C-   Outputs : NNAME     [I]   Number of names
C-             NAME(*)   [C*]  NameNames
C-             INAME(*)  [I]   NamePosition
C-   Controls: 
C-
C-   Created  10-JUL-1995   Harrison B. Prosper   
C-   Updated  12-JUL-1995   Susan K. Blessing  Allow prefixes longer than
C-    one character.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PREFIX1
      INTEGER MAXN, NNAME
      CHARACTER*(*) NAME(*)
      INTEGER INAME(*)
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:DGNCOM.INC'
C----------------------------------------------------------------------
      INTEGER I, N
      INTEGER BEG,END,LEN
      INTEGER BEG1,END1
C----------------------------------------------------------------------
      N = MIN(MAXN,NFIELD)
      NNAME = 0
      CALL SWORDS(PREFIX1,BEG1,END1,LEN)
      DO I =  1, N
        CALL SWORDS(PREFIX(I),BEG,END,LEN)
        IF ( PREFIX1(BEG1:END1) .EQ. PREFIX(I)(BEG:END) ) THEN
          NNAME = NNAME + 1
          NAME(NNAME) = FIELD(I)
        ENDIF
      ENDDO
  999 RETURN
      END
