      SUBROUTINE CL2_VZERO_PTCAEP2
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Zero the pointer array for CL2_xxx routines by VZERO
C-
C-      more sophisticated zeroing is done in CL2_ROTOW_ETNOM for level2 itself
C-
C-      an algorithm like CPTCAZ is possible only offline (no end of event hook
C-              exists in level2)
C-
C-      the PTCAEP2_ALL_ZEROS flag is set to false in CL2_MAKE_ETNOM
C-
C-   Inputs  : none
C-   Outputs : zero'ed PTCAEP2
C-   Controls: PTCAEP2_ALL_ZEROS   : if .TRUE. do nothing
C-
C-   Created  17-MAY-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$CALOR_FILTER$SOURCE:PTCAEP2.DEF'
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        PTCAEP2_ALL_ZEROS = .FALSE.
        FIRST = .FALSE.
      ENDIF
C----------------------------------------------------------------------
      IF(.NOT.PTCAEP2_ALL_ZEROS) THEN      ! don't do it if array is empty
        CALL VZERO( PTR2, NLYRL*NPHIL*(2*NETAL+1) )
        PTCAEP2_ALL_ZEROS=.TRUE.
      ENDIF
  999 RETURN
      END
