C----------------------------------------------------------------------
      LOGICAL FUNCTION T0TRKS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determines if there are tracks hitting the fiber
C-                         detector
C-
C-   Inputs       : None
C-   Outputs      : None
C-   Return value : .True.  - if there are tracks in the T0 detector region
C-                  .False. - otherwise
C-   Controls     : None
C-
C-   Created  14-MAY-1992   Gregory L. Landsberg
C-   Updated  11-APR-1993   Qizhong Li-Demarteau  removed MZDROP, which
C-                                          prevents the D0RECO to work 
C-
C----------------------------------------------------------------------
      IMPLICIT      NONE
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INCLUDE      'D0$INC:T0DREC.INC'
      INTEGER       LT0TH, GZT0TH, GZPROC
C----------------------------------------------------------------------
      T0TRKS = .TRUE.
      LT0TH  = GZT0TH()
      IF ( LT0TH .GT. 0 ) THEN
        IF ( IQ(LT0TH+2) .GT. 0 ) THEN
          IF (L_HIST) CALL HFILL(1,FLOAT(IQ(LT0TH+2)),0.,1.)
          RETURN
        END IF
        IF ( IQ(LT0TH+3) .GT. 0 ) RETURN
      END IF
      IF (L_HIST) CALL HFILL(1,0.,0.,1.)
      T0TRKS = .FALSE.
C----------------------------------------------------------------------
      RETURN
      END
