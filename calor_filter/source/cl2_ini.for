      SUBROUTINE CL2_INI
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      initialization for CL2_xxx routines:
C-              build tables for later use
C-              set up CL2_LINK areas
C-   Inputs  : none
C-   Outputs : /PHTT/,/TTPH/,/TTMGETA/,/TTEDGE/,/CL2CRATE/,links and STP banks
C-   Controls: none
C-
C-   Created   9-NOV-1990   James T. Linnemann
C-   Updated  30-APR-1991   James T. Linnemann   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$CALOR_FILTER$SOURCE:PTCAEP2.DEF'
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL CL2_GET_SFTVSN             ! tell table building SFTVSN from CAGS
        CALL CGPHTT                     ! build physics to tower conversion
        CALL CGTTPH                     ! build tower to physics conversion
        CALL CGDBITT                    ! l1 index to TT eta,phi conversion
        CALL CGTTMGETA                  ! whether eta has MG/ICD
        CALL CGTTEDGE                   ! where to zero for this TTOWER
        CALL CGL2CRATE                  ! level 2 crate id's
        CALL CL2_LINK_INI               ! tell ZEBRA about CL2 link areas
        CALL CL2_RING_INIT              ! arrays for eta,phi rings
        FIRST = .FALSE.
      ENDIF
      CALL CL2_STPLNK                   ! put SL2H CAGS and CADT in link area
      CALL CL2_GET_GEOM                 ! offline geometry in place
      PTCAEP2_ALL_ZEROS = .FALSE.       ! force (re-)init of pointer array
      CALL CL2_VZERO_PTCAEP2            
      CALL L2_L0_PARAMETERS             ! get params for calc of L0 vertex
      CALL CL2_ETMISS_GEOM_INIT            ! sin, cos phi for missing Pt calcs
  999 RETURN
      END
