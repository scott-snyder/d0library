      FUNCTION VERTEX_FIX_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialization for vertex_fix package
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-JUL-1995   Srini Rajagopalan
C-   Updated   5-SEP-1995   Srini Rajagopalan  Add VERTEX_SWAP flag book 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL VERTEX_FIX_INI
C
      INTEGER LRCP, IER
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
      VERTEX_FIX_INI = .TRUE.
      IF (.NOT.FIRST) GO TO 999
      FIRST = .FALSE.
C
      CALL EZLOC('VERTEX_RCP',LRCP)
      IF (LRCP.LE.0) THEN
        CALL INRCP('VERTEX_RCP',IER)
        IF (IER.NE.0) THEN
          CALL ERRMSG('VERTEX_RCP BAD','VERTEX_FIX_INI',
     &                'VERTEX RCP has a bad read','F')
        ENDIF
      ENDIF
C
      CALL EZLOC('VERTEX_FIX_RCP',LRCP)
      IF (LRCP.LE.0) THEN
        CALL INRCP('VERTEX_FIX_RCP',IER)
        IF (IER .NE. 0) THEN
          CALL ERRMSG('VERTEX_FIX RCP BAD','VERTEX_FIX_INI',
     &                'VERTEX_FIX RCP had a bad read','F')
        END IF
      ENDIF
C
C Book vertex_change flag
C
      CALL FLGBK('VERTEX_CHANGE',1)
      CALL FLGSET('VERTEX_CHANGE',.FALSE.)
C
C Book vertex_swap flag
C
      CALL FLGBK('VERTEX_SWAP',1)
      CALL FLGSET('VERTEX_SWAP',.FALSE.)
C
  999 RETURN
      END
