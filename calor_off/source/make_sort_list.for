      SUBROUTINE MAKE_SORT_LIST( NLIST, LIST, ENTRY, NMAX, KEY,
     &  MINENTRY, START )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Maintain a list -LIST-
C-        (sorted in descending order)
C-        which only has NMAX spaces.  Every time this routine is called
C-        with a new ENTRY, that ENTRY is placed at the appropriate spot
C-        in the list or not placed on it at all. NLIST = total number
C-        of entries in list currently.
C-        The list is reinitialized by setting START = .TRUE.
C-        Only entries greater than MINENTRY will be entered on the list.
C-        Optionally, you can maintain a KEY to tell you which maps
C-        the current list into the assumed input list
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  28-APR-1993   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMAX, NLIST, NTARGET, KEY( NMAX ), NENTRY, K
      REAL LIST( NMAX ), ENTRY, MINENTRY
      LOGICAL START
C----------------------------------------------------------------------

C
C: Reinitialize for a new list if START = .TRUE.
C
      IF ( START ) THEN
        START = .FALSE.
        NENTRY = 0
        NLIST = 0
      ENDIF
C
C: Dont even try if ENTRY is too small, but it still counts as an entry
C  for the KEY.
C
      IF ( ENTRY .LT. MINENTRY ) THEN
        NENTRY = NENTRY + 1
        RETURN
      ENDIF
C
C: Find out where this entry belongs on the list and push the rest down
C: to make room for it.
C
      NTARGET = NLIST + 1                 ! Here's where we might put it
      DO K = NLIST, 1, -1
        IF ( ENTRY .GT. LIST( K ) ) THEN
          NTARGET = K
          IF ( K .LT. NMAX ) THEN
            LIST( K+1 ) = LIST( K )
            KEY( K+1  ) = KEY( K )
          ENDIF
        ENDIF
      ENDDO
      IF ( NTARGET .LE. NMAX ) THEN
        LIST( NTARGET ) = ENTRY
        KEY( NTARGET ) = NENTRY + 1
      ENDIF
      NENTRY= NENTRY + 1
      NLIST = MIN( NLIST+1, NMAX )
  999 RETURN
      END
