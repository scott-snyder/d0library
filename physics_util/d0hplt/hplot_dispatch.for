      LOGICAL FUNCTION HPLOT_DISPATCH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Executes HPLOT functions requested from HPLOT_MENU
C-
C-   Returned value  : TRUE IF OK
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created  17-JUL-1990   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL D0HPLT_DISPATCH,FIRST
      CHARACTER*40 COMMAND
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      HPLOT_DISPATCH = .TRUE.
      CALL EXAMINE_DISPATCH_COMMAND(COMMAND)
C
      IF (COMMAND .EQ. 'HISTOGRAM DISPLAY') THEN
C
C ****  Turn OFF all packages except HPLOT
C
        CALL PBD_SET_ALL_FLAGS('HPLOT',.FALSE.)

        HPLOT_DISPATCH = D0HPLT_DISPATCH ()
C
C ****  Reset all flags to original values
C
        CALL PBD_RESET_ALL_FLAGS
C
      END IF
  999 RETURN
      END
