      SUBROUTINE ECEMCR_CRACK( E_MEAS, ZV1, X_EM3, Y_EM3, Z_EM3_MOD,
     &                      DE_CRACK, ERR_DE_CRACK, OK )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Calculate the energy correction for the crack.
C-
C-             This correction is proportional to the energy of the shower.
C-             We may parameterize the effect by a simple gaussian in x.
C-
C-   Inputs  : E_MEAS       Measured energy
C-             ZV1          Z vertex of event
C-             X_EM3        X position at EM3 center
C-             Y_EM3        Y position at EM3 center
C-             Z_EM3_MOD    Position of EM3 center
C-   Outputs : DE_CRACK     Energy correction to be ADDED to cluster energy
C-             ERR_DE_CRACK Estimate of error in DE_CRACK
C-             OK           Error flag --  0: Ok
C-                                        -1: Cracks improperly defined
C-   Controls:
C-
C-   Created   25-SEP-1992 Orin Dahl
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------

C                         Input and output variables
      REAL     E_MEAS
      REAL     ZV1
      REAL     X_EM3
      REAL     Y_EM3
      REAL     Z_EM3_MOD
      REAL     DE_CRACK
      REAL     ERR_DE_CRACK
      INTEGER  OK

C                         Geometry variables formed at initialization
      INTEGER  CRACK_DIM
      PARAMETER ( CRACK_DIM = 2 )
      REAL     X_CRACK_ARRAY(CRACK_DIM, 2)
      REAL     DXDY_CRACK_ARRAY(CRACK_DIM, 2)
      INTEGER  TOTAL_CRACKS

      SAVE     X_CRACK_ARRAY,    DXDY_CRACK_ARRAY, TOTAL_CRACKS

C                         Variables formed at initialization
      REAL     ERR_FRAC
      REAL     E_FRAC
      REAL     X_SIGMA_ZERO
      REAL     POS_LENGTH
      REAL     POS_E_SCALE
      REAL     POS_OFFSET

      SAVE     ERR_FRAC,         E_FRAC,           X_SIGMA_ZERO
      SAVE     POS_LENGTH,       POS_E_SCALE,      POS_OFFSET

C                         Position variables
      REAL     TAN_X
      REAL     X_CRACK

C                         Variables used to find the nearest crack
      INTEGER  ENDCAP
      INTEGER  N_CLOSE
      REAL     LEN_ABS_BEST
      REAL     LEN_ABS
      INTEGER  I

C                         Variables used in forming the x dependence
      REAL     Z_FAC
      REAL     X_SHIFT
      REAL     X_OFFSET
      REAL     X_ZERO
      REAL     X_SIGMA
      REAL     X_PEAK
      REAL     X_GAUSS
      REAL     X_CORR

C                         Variables used to form the final correction
      REAL     F_DEL

C                         Miscellaneous variables
      INTEGER  IER
      LOGICAL  FIRST
      SAVE     FIRST
      DATA     FIRST /.TRUE./

C----------------------------------------------------------------------

C--->     Local initialization.

      IF ( FIRST )  THEN
         FIRST = .FALSE.

C                         Get the crack geometry

         CALL ECEMCR_CRACK_GEOM( CRACK_DIM,
     &           X_CRACK_ARRAY, DXDY_CRACK_ARRAY, TOTAL_CRACKS )

C                         Read parameters from the RCP file.

         CALL EZPICK('ECEMCR_RCP')
         CALL EZGET('ECEM_CRACK_ERR_FRACTION',     ERR_FRAC,       IER)
         CALL EZGET('ECEM_CRACK_E_FRACTION',       E_FRAC,         IER)
         CALL EZGET('ECEM_CRACK_X_SIGMA_ZERO',     X_SIGMA_ZERO,   IER)
         CALL EZGET('ECEM_CRACK_POS_LENGTH',       POS_LENGTH,     IER)
         CALL EZGET('ECEM_CRACK_POS_E_SCALE',      POS_E_SCALE,    IER)
         CALL EZGET('ECEM_CRACK_POS_OFFSET',       POS_OFFSET,     IER)
      ENDIF

C--->     Give up if cracks improperly defined

      IF ( TOTAL_CRACKS.EQ.0 )  THEN
          OK = -1
          DE_CRACK = 0.
          ERR_DE_CRACK = 0.
          RETURN
      ELSE
          OK = 0
      ENDIF

C--->     Calculate positions.

C                         Calculate the position of the cluster.

      TAN_X = X_EM3 / ABS( Z_EM3_MOD - ZV1 )

C                         Find the nearest crack

      IF ( Z_EM3_MOD.LT.0. )  THEN
          ENDCAP = 1
      ELSE
          ENDCAP = 2
      ENDIF

      LEN_ABS_BEST = 1.E10
      DO 100 I = 1,TOTAL_CRACKS
          LEN_ABS = ABS( X_EM3 - X_CRACK_ARRAY(I,ENDCAP) )
          IF ( LEN_ABS.LT.LEN_ABS_BEST )  THEN
              LEN_ABS_BEST = LEN_ABS
              N_CLOSE = I
          ENDIF
  100 CONTINUE

      X_CRACK = X_CRACK_ARRAY(N_CLOSE,ENDCAP)
     &        + DXDY_CRACK_ARRAY(N_CLOSE,ENDCAP) * Y_EM3

C--->     Form the crack corrections

C                         Form the x dependent factors

      Z_FAC    = Z_EM3_MOD / ( Z_EM3_MOD - ZV1 )
      X_SHIFT  = - TAN_X * POS_LENGTH * LOG( E_MEAS / POS_E_SCALE )
      X_OFFSET = POS_OFFSET * SIGN( 1.0 , X_CRACK )
      X_ZERO   = X_CRACK + X_OFFSET + X_SHIFT
      X_SIGMA  = X_SIGMA_ZERO * Z_FAC

      X_PEAK   = E_FRAC / Z_FAC
      X_GAUSS  = EXP( -0.5 * ( (X_EM3-X_ZERO) / X_SIGMA )**2 )
      X_CORR   = X_PEAK * X_GAUSS

C                         Form the final correction

      F_DEL   = - X_CORR / ( 1. + X_CORR )

      DE_CRACK = E_MEAS * F_DEL
      ERR_DE_CRACK = DE_CRACK * ERR_FRAC

  999 RETURN
      END
