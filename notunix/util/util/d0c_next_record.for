      SUBROUTINE D0C_NEXT_RECORD (LUNINP,RECORD,EOF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read next record, skipping form-feeds and
C-                         page header lines.
C-
C-   Inputs  : LUNINP   [I]     Input unit number
C-
C-   Outputs : RECORD   [C*]    Next record read in
C-             EOF      [L]     TRUE if End-Of-File
C-
C-   Controls: None
C-
C-   Created  10-AUG-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER LUNINP
      CHARACTER*(*) RECORD
      LOGICAL EOF
      INTEGER L

      INTEGER FORM_FEED
      PARAMETER( FORM_FEED = 12 )
C----------------------------------------------------------------------
      LOGICAL SKIPPED
      SAVE SKIPPED
C----------------------------------------------------------------------
      L = LEN (RECORD)
      READ (UNIT=LUNINP,FMT='(A)',END=900) RECORD(1:L)
      SKIPPED = .FALSE.
C
      IF ( ICHAR(RECORD(1:1)) .EQ. FORM_FEED ) THEN
        SKIPPED = .TRUE.
C
C ****  Skip 3-line page header
C
        READ (UNIT=LUNINP,FMT='(A)',END=900) RECORD(1:L)
        READ (UNIT=LUNINP,FMT='(A)',END=900) RECORD(1:L)
        READ (UNIT=LUNINP,FMT='(A)',END=900) RECORD(1:L)
        READ (UNIT=LUNINP,FMT='(A)',END=900) RECORD(1:L)
      ENDIF
      EOF = .FALSE.
      GOTO 999

  900 CONTINUE
      EOF = .TRUE.
  999 RETURN
C
      ENTRY D0C_BACKSPACE(LUNINP)
      IF ( SKIPPED ) THEN
        BACKSPACE LUNINP
        BACKSPACE LUNINP
        BACKSPACE LUNINP
        BACKSPACE LUNINP
      ENDIF
      BACKSPACE LUNINP
      RETURN
      END
