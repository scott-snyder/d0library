      SUBROUTINE P0PADS(END)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Action routine to display one LV0 array with
C-                         hits and tracks if requested.
C-
C-   Inputs  : END = (1=North, 2=South) Level 0 array
C-   Outputs : display LV0 scintillator pad hits
C-   Controls:
C-
C-   Created  29-JUN-1992   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER CHANS_HIT(72)
      INTEGER NLC(8,2),NSC(2)
      INTEGER PADS_HIT(-4:4,-4:4,2,2)
      INTEGER PADS_FHIT(-4:4,-4:4,2)
      INTEGER END
      INTEGER IER
C
      REAL LC_HIT_POSITIONS(10,8,2,3)
      REAL SC_HIT_POSITIONS(100,2,5)
C
      LOGICAL EZERROR,FIRST
      EXTERNAL EZERROR
C
      SAVE FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST=.FALSE.
        CALL EZPICK('LEVEL0_RCP')
        IF( EZERROR(IER) ) THEN
          CALL ERRMSG('PIXIE-l0-no-rcp','P0PADS',
     &                             'Can not find LEVEL0_RCP','W')
        ELSE
          CALL EZRSET
        ENDIF
      ENDIF
C
C  Find out which Level 0 pads were hit.
C
      CALL L0_PADS_HIT(PADS_HIT)
C
C  Obtain the projections of all FDC tracks passing through the Level 0.
C
      CALL L0_PROJ_FTRAKS(CHANS_HIT,NLC,LC_HIT_POSITIONS,PADS_FHIT,NSC,
     &  SC_HIT_POSITIONS)
C
C  Initialize a graphics segment.
C
      CALL PUOPEN
C
C  Draw one Level 0 R-Phi view, one end (N=1,S=2).
C
      CALL P0RPHI(END,PADS_HIT,NLC,LC_HIT_POSITIONS,PADS_FHIT,NSC,
     &  SC_HIT_POSITIONS)
C
C  Close graphics segment.
C
      CALL JRCLOS
C----------------------------------------------------------------------
  999 RETURN
      END
