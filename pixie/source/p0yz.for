      SUBROUTINE P0YZ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display the Level 0 in Y-Z view.
C-
C-   Inputs  : none
C-   Outputs : display Level 0 Y-Z view
C-   Controls: none
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
      REAL L0Z(2)
C
      LOGICAL L0ONLY
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
     &                            'Can not find LEVEL0_RCP','W')
          L0Z(1)=-142.3
          L0Z(2)= 142.3
        ELSE
          CALL EZGET('L0Z',L0Z,IER)
          CALL EZRSET
        ENDIF
        CALL EZPICK('PX_LV0DIS_RCP')
        IF( EZERROR(IER) ) THEN
          CALL ERRMSG('PIXIE-l0px-no-rcp','P0PADS',
     &                             'Can not find PX_LV0DIS_RCP','W')
          GOTO 999
        ENDIF
        CALL PUGETV('LV0 ONLY',L0ONLY)
        CALL EZRSET
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
C  Initialize a graphics segment and (if only L0) draw Y-Z axes.
C
      CALL PUOPEN
      CALL JJUST(2,2)
      IF ( L0ONLY ) THEN
        END=1
        CALL PUVSTR( 0.0, 60.0, 3., 1.5, 'Level 0 Arrays')
        CALL J3MOVE( 0.,-60.,L0Z(END))
        CALL J3DRAW(10.,-60.,L0Z(END))
        CALL J3MOVE(15.,-60.,L0Z(END))
        CALL J3STRG('Z')
        CALL J3MOVE(0.,-60.,L0Z(END))
        CALL J3DRAW(0.,-50.,L0Z(END))
        CALL J3MOVE(0.,-45.,L0Z(END))
        CALL J3STRG('Y')
      ENDIF
C
C  Draw the Level 0 Y-Z view, one end (N=1,S=2) at a time.
C
      DO END=1,2
        CALL P0YZEND(END,PADS_HIT,NLC,LC_HIT_POSITIONS,PADS_FHIT,NSC,
     &    SC_HIT_POSITIONS)
      ENDDO
C
C  Close graphics segment.
C
      CALL JRCLOS
C
C----------------------------------------------------------------------
  999 RETURN
      END
