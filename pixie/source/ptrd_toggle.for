      SUBROUTINE PTRD_TOGGLE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check the TRD_ANO_CAT flag and it set it
C-   to its oposite value (toggle key)
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  20-NOV-1991   Lupe Howell based in modifications made by
C-   Jean-Francois GLICENSTEIN
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*)TITLE
      PARAMETER( TITLE = 'SELECT WIRE' )
      LOGICAL TRUTH,FLGVAL
C----------------------------------------------------------------------
      TRUTH = FLGVAL('TRD_ANO_CAT')
      IF (.NOT.TRUTH) THEN
        CALL STAMSG(TITLE//' ANODES ',.TRUE.)
      ELSE IF (TRUTH) THEN
        CALL STAMSG(TITLE//' CATHODES ',.TRUE.)
      ENDIF
      CALL FLGSET('TRD_ANO_CAT',.NOT.TRUTH)
  999 RETURN
      END
