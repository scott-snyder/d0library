      SUBROUTINE KCLAS
C
C   Routine to erase the alpha-plane
C
      IMPLICIT NONE
      EXTERNAL ERRHND
      CHARACTER*6  ERSTR
C
C   Send the proverbial <ESC>[H<ESC>[J to clear the alpha screen.
C
      ERSTR = CHAR(27)//'[H'//CHAR(27)//'[J'
      CALL PSNST(ERSTR, 1, 'ES_TE', ERRHND)

      RETURN
      END
