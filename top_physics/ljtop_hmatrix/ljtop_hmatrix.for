      FUNCTION LJTOP_HMATRIX()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Main routine for Top into Lepton + jets
C-
C-   Returned value  : True if OK
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   2-OCT-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL LJTOP_HMATRIX
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      LOGICAL DO_LJTOP_HMATRIX_ANAL,OK,HMATRIX_EVENT
      INTEGER IER
C----------------------------------------------------------------------
      IF(FIRST)THEN                     ! LOCAL INIT
        FIRST = .FALSE.
        CALL EZPICK('LJTOP_HMATRIX_RCP')
        CALL EZGET('DO_LJTOP_HMATRIX_ANAL',DO_LJTOP_HMATRIX_ANAL,IER)
        CALL EZRSET
      ENDIF
C
      CALL LJTOP_HMATRIX_FILL_QUAN(IER)
      IF ( IER.EQ.0 ) THEN
        OK = HMATRIX_EVENT()
        IF(DO_LJTOP_HMATRIX_ANAL)CALL LJTOP_HMATRIX_ANAL        ! ANALYZE
C
C ****  write out event if used in Hmatrix
C
        CALL FLGSET('WRITE_THIS_EVENT',.TRUE.)
C
        LJTOP_HMATRIX = .TRUE.
        CALL LJTOP_HMATRIX_SEL_EVT_WRITE
      ELSE
        LJTOP_HMATRIX = .FALSE.
      ENDIF
  999 RETURN
      END
