      FUNCTION FDC_ERROR_NHIT(AVE_HIT_WIRE,UNIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Correct error for dependence on average 
C-   number of hits in sector.
C-
C-   Returned value  : factor by which to increase error
C-   Inputs  : AVE_HIT_WIRE     Ave number of hits per wire in sector.
C-   Outputs : none
C-
C-   Created   2-JUL-1992   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      REAL    FDC_ERROR_NHIT
C
C  Input:
      REAL    AVE_HIT_WIRE
      INTEGER UNIT
C
C  Local:
      INTEGER IER
      REAL    ERR_NHIT_THETA,ERR_NHIT_PHI
C
      LOGICAL FIRST
C
      SAVE FIRST,ERR_NHIT_THETA,ERR_NHIT_PHI
C
      DATA FIRST /.TRUE./
      DATA ERR_NHIT_THETA /0.0/     
      DATA ERR_NHIT_PHI   /0.0/     
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('ERR_NHIT_THETA',ERR_NHIT_THETA,IER)
        CALL EZGET('ERR_NHIT_PHI',ERR_NHIT_PHI,IER)
        CALL EZRSET
      ENDIF
C      
      IF ( UNIT.EQ.0 ) THEN
        FDC_ERROR_NHIT = 1.0 + ERR_NHIT_THETA * AVE_HIT_WIRE
      ELSE
        FDC_ERROR_NHIT = 1.0 + ERR_NHIT_PHI * AVE_HIT_WIRE
      ENDIF
C
  999 RETURN
      END
