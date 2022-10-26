            REAL FUNCTION ULALPS(Q2)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Determine which alpha_s running routine to use.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  21-JAN-1996   Adam L. Lyon
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE

      REAL Q2

      LOGICAL first, susy
      INTEGER ier
      REAL back, susy_thresh, susy_offset, ulalps_norm, ulalps_susy
      DATA first,susy/.true.,.false./
      SAVE first, susy, susy_thresh, susy_offset
      
C-----------------------------------------------------------------------

      IF ( first ) THEN

	first = .false.
        
        CALL EZPICK('SPYTHIA_RCP')
        CALL EZGET_l('SUSY_ALPHA_S',susy,ier)
        IF ( ier .NE. 0 ) THEN
          CALL ERRMSG('NO RCP','ULALPS',
     &         'No SUSY_ALPHA_S in RCP file -- making false','W')
          susy = .false.
        ENDIF

        IF ( susy ) THEN
          CALL EZGET('SUSY_ALPHA_S_THRESH',susy_thresh,ier)
          CALL EZGET('SUSY_ALPHA_S_OFFSET',susy_offset,ier)
          PRINT*, 'USING SUSY ALPHA S RUNNING: THRESH=',susy_thresh
        ENDIF

      ENDIF

      IF ( susy ) THEN
        back = ulalps_susy(q2,susy_thresh,susy_offset)
      ELSE
        back = ulalps_norm(q2)
      ENDIF

      ulalps = back
        
 999  RETURN
      END


      
