      FUNCTION ZTR_EXM_HISTOS_BEGIN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Beginning stuff for CD Examine histogram stuff.
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  13-DEC-1993   Susan K. Blessing
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL ZTR_EXM_HISTOS_BEGIN
C
      INTEGER IER
C
      LOGICAL FIRST
      LOGICAL OK
      LOGICAL VIEW_HISTOS,ZTR_EXM_VIEW_HISTOS
C
      DATA FIRST/.TRUE./
      DATA VIEW_HISTOS/.FALSE./
C
C----------------------------------------------------------------------
C
      ZTR_EXM_HISTOS_BEGIN = .TRUE.
C
      IF (FIRST) THEN
        CALL INRCP('CD_EXM_HISTOS_RCP',IER)
        IF (IER .NE. 0) THEN
          CALL ERRMSG('CD_EXM_HISTOS.RCP BAD','ZTR_EXM_HISTOS_BEGIN',
     &    'CD_EXM_HISTOS_RCP bad','W')
          GO TO 999
        END IF
C
        CALL EZPICK('CD_EXM_HISTOS_RCP')
        CALL EZGET('VIEW_HISTOS',VIEW_HISTOS,IER)
        CALL EZRSET
C
        FIRST = .FALSE.
      END IF
C
      IF (.NOT.VIEW_HISTOS) OK = ZTR_EXM_VIEW_HISTOS()
C
  999 RETURN
      END
