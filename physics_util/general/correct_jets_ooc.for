      SUBROUTINE CORRECT_JETS_OOC( E, ET, ETA, CONE_USED, CORR_FACTOR,
     &  IER )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return Out-of-cone correction factors
C-
C-   Inputs  :
C-            E     [R] -Energy of jet
C-            ET    [R] -ET of jet
C-            ETA   [R] -eta of jet
C-        CONE_USED [R] -Jet algorithm (.3,.5,.7,1.0,-1=NN)
C-
C-   Outputs :
C-
C-    CORR_FACTOR   [R] -Correction factor
C-            IER   [I] -Error code 0=ok -1=unknown algorithm
C-   Controls:
C-
C-   Created  14-JUL-1995   Richard V. Astur
C-   Updated  Oct-12-1995   Bob Kehoe  -- invert NN loss correction
C-   Updated  Oct-25-1995   Bob Kehoe  -- replace JINT with INT for IBM
C-                                        compatibility, add error entry pt.
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL E,ET,ETA,CONE_USED,CORR_FACTOR,error(2,2),err_ooc(2,2)
      REAL FACTOR_HIGH, FACTOR_LOW, CONE_HIGH, CONE_LOW, REDUCE_FACTOR
      INTEGER ICONE_LOW, ICONE_HIGH, I
      INTEGER IER, ICONE
C----------------------------------------------------------------------
      call vzero(error,2*2)
      error(1,2) = 0.01
      error(2,2) = 0.01
      IER   = 0                             ! OK
      CORR_FACTOR = 1.0                     ! No correction
      ICONE = INT( CONE_USED/.1 )          ! ALGORITHM FLAG
C
C: Correction for New Nearest Neighbor jets
C
      IF ( ICONE .EQ. -10 ) THEN   ! NN
        CORR_FACTOR = .984 + (-.2438)*EXP(-ABS(E)/59.11)
        corr_factor = 1.0/corr_factor
        RETURN
      ENDIF
C
C: We know the correction factors in the CC. Assume that forward jets of
C: the same E (energy) shower the same in physical space, but the cone
C: has been reduced in physical space.
C:
C: For now assume the space is reduced to a circle whose diameter is
C: equal to the eta difference. In reality it might be more like an
C: ellipse or an oblong circle.
C
      REDUCE_FACTOR = ABS(ATAN(EXP(-CONE_USED)) - ATAN(EXP(CONE_USED)))
      REDUCE_FACTOR = ABS(ATAN(EXP(ETA-CONE_USED))
     &  -ATAN(EXP(ETA+CONE_USED)))/REDUCE_FACTOR
C
C: Find the interpolation bin
C
      IF ( CONE_USED*REDUCE_FACTOR .LE. .5 ) THEN
        ICONE_LOW  = 3
        ICONE_HIGH = 5
      ELSEIF ( CONE_USED*REDUCE_FACTOR .LE. .7 ) THEN
        ICONE_LOW  = 5
        ICONE_HIGH = 7
      ELSE
        ICONE_LOW  = 7
        ICONE_HIGH = 10
      ENDIF
      CONE_LOW  = .1*ICONE_LOW
      CONE_HIGH = .1*ICONE_HIGH
C
C: Find correction factors for CC for high and low
C
      DO I = 1, 2
        IF ( I .EQ. 1 ) ICONE = ICONE_LOW
        IF ( I .EQ. 2 ) ICONE = ICONE_HIGH

        IF ( ICONE .EQ. 10 ) THEN ! R=1.0
          CORR_FACTOR = 1.0
        ELSEIF ( ICONE .EQ. 7 ) THEN ! R=0.7
          CORR_FACTOR = MIN( 1.0, .994 + .00005*E)
        ELSEIF ( ICONE .EQ. 5 ) THEN ! R=0.5
          CORR_FACTOR = MIN( 1.0, .963 + .0005*E)
        ELSEIF ( ICONE .EQ. 3 ) THEN ! R=0.3
          CORR_FACTOR = MIN( .98, .939 + .00032*E )
        ENDIF

        IF ( I .EQ. 1 ) FACTOR_LOW = CORR_FACTOR
        IF ( I .EQ. 2 ) FACTOR_HIGH= CORR_FACTOR
      ENDDO
C
C: Interpolate
C
      CORR_FACTOR = FACTOR_LOW + (FACTOR_HIGH-FACTOR_LOW)*
     &  (CONE_USED*REDUCE_FACTOR-CONE_LOW)/(CONE_HIGH-CONE_LOW)

      CORR_FACTOR = 1.0/CORR_FACTOR
  999 RETURN

      ENTRY algo_loss_errors(err_ooc)
C------------------------------------------------------------------------
C-    Purpose:  output value of errors 
C-
C-    outputs:
C-          err_ooc  R  = array of statistical and systematic errors
C------------------------------------------------------------------------
      call ucopy(error,err_ooc,2*2)
      RETURN

      END
