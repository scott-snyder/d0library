      SUBROUTINE CORRECT_JETS_NOISEU(LJETS, INDET_ETA, INCONE_SIZE,
     &  UE_E, UE_ET, ZSP_E, ZSP_ET )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the noise and underlying event
c-                         contribution for this event.
C-
C-   Inputs  :  [I] LJETS - pointer to JETS bank. If 0, will use ETA
C-                          and CONE_SIZE to return the number.
C-              [R] DET_ETA   - detector eta of jet (not needed if LJETS .GT. 0)
C-              [R] CONE_SIZE - effective area of jet. (not needed if LJETS
C-                              .GT. 0 )
C-
C-   Outputs :  [R] UE_E     - underlying event contribution to E
C-              [R] UE_ET    - underlying event contribution to ET
C-              [R] ZSP_E    - noise contribution to E
C-              [R] ZSP_ET   - noise contribution to ET
C-   Controls:
C-
C-   Created   8-MAY-1995   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL DEFAULT_CONE_SIZE, INCONE_SIZE
      PARAMETER( DEFAULT_CONE_SIZE = .3 )
      EXTERNAL  CORRECT_JETS_NUM_INTERACTIONS
      REAL      CORRECT_JETS_NUM_INTERACTIONS
      EXTERNAL  PETA_TO_DETA
      REAL      PETA_TO_DETA
      REAL  UE_E, ZSP_ET, ZSP_E
      REAL UE_ET, DET_ETA, CONE_SIZE, INDET_ETA
      REAL ZV(3)
      REAL AVE_NUM_INTER
      INTEGER LJETS, LCAPH, IER
      LOGICAL DONE
C----------------------------------------------------------------------
      CONE_SIZE = INCONE_SIZE
      DET_ETA   = INDET_ETA
      IF ( CONE_SIZE .LE. 0. ) CONE_SIZE  = DEFAULT_CONE_SIZE
C
C: If LJETS .GT. 0, we can check to see if the noise/ue was calculated
C: online. Also, we can get the physics ETA and cone size from this.
C
      IF ( LJETS .GT. 0 ) THEN
C
C: Get vertex to find detector eta
C
        DET_ETA       = Q( LJETS + 9 )            ! physics eta
        CALL VERTEX_INFO(1, IER , ZV, DONE )
        IF ( .NOT. DONE ) ZV(1) = 0.0
        DET_ETA = PETA_TO_DETA( DET_ETA, ZV(1) )
C
C: Find conesize
C
        LCAPH     = LQ( LJETS + 1 )
        CONE_SIZE = DEFAULT_CONE_SIZE
        IF ( IQ(LCAPH+4) .EQ. 2 ) CONE_SIZE = Q( LCAPH+6)
C
C: If noise/ue words already filled. Return them.
C
        CALL JETS_BANK_CORRECTED(LJETS, 'UND', DONE , IER )
        DONE  = ( DONE .AND. ( IER .EQ. 0 ) )
        CALL JETS_BANK_CORRECTED(LJETS, 'ZSP', DONE , IER )
        DONE  = ( DONE .AND. ( IER .EQ. 0 ) )

        IF ( DONE ) THEN
          UE_ET     = Q( LJETS + 31)
          UE_E      = UE_ET*COSH(Q(LJETS+9))
          ZSP_ET    = Q( LJETS + 30)
          ZSP_E     = ZSP_ET*COSH(Q(LJETS+9))
          RETURN
        ENDIF
      ENDIF

C
C: Call normal routine
C
      CALL JET_UNDZSP_FACTOR( CONE_SIZE, 1, DET_ETA, UE_E, UE_ET,
     &  ZSP_E, ZSP_ET )

C
C: Scale by number of interactions
C
      AVE_NUM_INTER  = CORRECT_JETS_NUM_INTERACTIONS()
      UE_E  = UE_E*AVE_NUM_INTER
      UE_ET = UE_ET*AVE_NUM_INTER

  999 RETURN
      END
