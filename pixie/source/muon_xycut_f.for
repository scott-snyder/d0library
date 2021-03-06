      SUBROUTINE MUON_XYCUT_F
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display full muon cut view (A,B,and C layers)
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  26-OCT-1990   Sharon Hagopian
C-   Updated   5-NOV-1991   Lupe Howell  Letting the pick display legends 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER IVIEW,IFLAG
      LOGICAL PU_PICK_ACTIVE
C______________________________________________________________________
      DATA IVIEW/2/
      DATA IFLAG/0/
C----------------------------------------------------------------------
      IF ( PU_PICK_ACTIVE() ) THEN
        CALL PMPICK
        CALL PX_PICK_QUIT
        GOTO 999
      ENDIF
      CALL PMDDETC(IVIEW,IFLAG)
      CALL PMEVNTC(IVIEW,IFLAG)
  999 RETURN
      END
