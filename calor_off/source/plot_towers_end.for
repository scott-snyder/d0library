      LOGICAL FUNCTION PLOT_TOWERS_END
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER DEVICE,SWITCH
      REAL    SCALE,X,Y,DETA,DPHI,ETMIN,RADIUS
      LOGICAL PAUS
C----------------------------------------------------------------------
      PLOT_TOWERS_END = .FALSE.
      CALL PLOT_TOWERS_VALUES
     &  (DEVICE,PAUS,SCALE,X,Y,DETA,DPHI,SWITCH,ETMIN,RADIUS)
      CALL JCLEAR
      CALL JDEVOF (DEVICE)
      CALL JDEND  (DEVICE)
      CALL JEND
  999 RETURN
      END
