      FUNCTION PU_PICK_ACTIVE()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return TRUE if PIXIE is in picking mode,
C-   FALSE otherwise. 
C-
C-   Returned value  : TRUE     if in PICK mode
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  21-JUN-1991   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FLGVAL
      LOGICAL PU_PICK_ACTIVE, PU_ROTATE_ACTIVE, PU_ZOOM_ACTIVE
C----------------------------------------------------------------------
      PU_PICK_ACTIVE = FLGVAL('PICKING')
      RETURN
C
      ENTRY PU_ROTATE_ACTIVE()
      PU_ROTATE_ACTIVE = FLGVAL('ROTATING')
      RETURN
C
      ENTRY PU_ZOOM_ACTIVE()
      PU_ZOOM_ACTIVE = FLGVAL('ZOOMING')
C      
  999 RETURN
      END
