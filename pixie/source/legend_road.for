      SUBROUTINE LEGEND_ROAD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set up legend for particle roads
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   4-SEP-1991   Sharon Hagopian
C-   Updated   7-OCT-1991   Lupe Howell  the color foreground added to legend 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL VWSAVE(85)
      CHARACTER*3 COLORS(5)
      CHARACTER*15 LABELS(5)
      DATA COLORS/'GRE','RED','CYA','YEL','FOR'/
      DATA LABELS/'MUON','ELEC','TAUS','VEES','OTHER'/
C----------------------------------------------------------------------
      CALL JVSAVE(VWSAVE)
      CALL LEGEND_LINE(COLORS,LABELS,5)
      CALL JVLOAD(VWSAVE)
  999 RETURN
      END
