      SUBROUTINE DBKCHI
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Book histograms: Chi-square/(degree freedom) on XY and RZ fits
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: called by DTRDIA
C-             ENTRY point: DTSCHI(HSTCHI)
C-
C-   Created  18-AUG-1988   Qizhong Li-Demarteau
C-   Updated   8-JUN-1991   Qizhong Li-Demarteau  added Chi**2 histos for
C-                                                segments 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL YES, HSTCHI
C----------------------------------------------------------------------
C
      YES=.FALSE.
      CALL GETPAR(1,' Book histograms: Chi-sq on XY and RZ fits? Y/N>',
     &  'L',YES)
      IF(.NOT.YES) GOTO 999
C
      CALL HBOOK1(1100,'Chisq/degf on XY fit for full track$',
     &  100,0.,50.0,0.)
      CALL HBOOK1(1101,'Chisq/degf on RZ fit for full track$',
     &  100,0.0,100.0,0.)
      CALL HBOOK1(1102,'Chisq/degf of segments in layer 0$',
     &  100,0.,20.0,0.)
      CALL HBOOK1(1103,'Chisq/degf of segments in layer 1$',
     &  100,0.,20.0,0.)
      CALL HBOOK1(1104,'Chisq/degf of segments in layer 2$',
     &  100,0.,20.0,0.)
      CALL HBOOK1(1105,'Chisq/degf of segments in layer 3$',
     &  100,0.,20.0,0.)
C
 999  RETURN
C---------------------------------------------------------------------------
      ENTRY DTSCHI(HSTCHI)
      HSTCHI = YES
      RETURN
      END
