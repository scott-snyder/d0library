C----------------------------------------------------------------------
      LOGICAL FUNCTION D0DBL3_RENMFZ (CHOP,FILIN,DIR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will rename the FZ file (FILIN) to the todo
C-    area (DIR). So it can be picked up by the server.
C-
C-   Return   .true.  it went ok
C-            .false. something went wrong
C-
C-   Input     CHOP    Currently not used
C-             FILIN   Name of FZ file to be renamed
C-             DIR     The to do area, to put it into
C-   Output    none
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL LIB$RENAME_FILE
C
      CHARACTER *(*) CHOP, FILIN, DIR
C
      LOGICAL LOK
      CHARACTER*255 FILOUT
C----------------------------------------------------------------------
      D0DBL3_RENMFZ = .FALSE.
C
C- Build the appropriate file name for the next file to go into
C- the TODO area
C
      CALL D0DBL3_FILNAM ( DIR, FILOUT, LOK )
      IF (.NOT. LOK) THEN
         CALL ERRMSG 
     &     ('Error in creating TODO file name','D0DBL3_RENMFZ',' ','E')
         RETURN
      END IF
C
C- Rename it so the Server can pick it up
C
      IF ( .NOT. LIB$RENAME_FILE ( FILIN, FILOUT ) ) THEN
         CALL ERRMSG 
     &     ('Error in renaming file','D0DBL3_RENMFZ',' ','E')
         RETURN
      END IF
C
      D0DBL3_RENMFZ = .TRUE.
C
      RETURN
      END
