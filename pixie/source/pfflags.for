      FUNCTION PFFLAGS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize flags for FDC pixie displays.
C-
C-   Returned value  : 0 if ok, else error code (see FLGERR)
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  25-JUN-1992   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PFFLAGS,IERR
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        CALL FLGBK('PF_SET_SCALE',1)
        CALL FLGERR(IERR) 
        FIRST = .FALSE.
      ENDIF
      PFFLAGS = IERR
C
  999 RETURN
      END
