      SUBROUTINE ECEMCR_BOLT( E_MEAS, ZV1, X_EM3, Y_EM3, Z_EM3_MOD,
     &                      DE_BOLT, ERR_DE_BOLT, OK )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Calculate the energy correction for the bolts.
C-
C-             This correction is proportional to the energy of the shower.
C-             We may parameterize the effect by the product of two functions:
C-             a function in r and a function in phi.
C-             1.  The radial dependence is a sum of two gaussians.
C-             2.  The azimuthal dependence is a sum of a gaussian and
C-                 a 4th power exponential of the form exp(-phi**4).
C-
C-   Inputs  : E_MEAS      Measured energy
C-             ZV1         Z vertex of event
C-             X_EM3       X position at EM3 center
C-             Y_EM3       Y position at EM3 center
C-             Z_EM3_MOD   Position of EM3 center
C-   Outputs : DE_BOLT     Energy correction to be ADDED to cluster energy
C-             ERR_DE_BOLT Estimate of error in DE_BOLT
C-             OK           Error flag --  0: Ok
C-                                        -1: Bolts improperly defined
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
      REAL     DE_BOLT
      REAL     ERR_DE_BOLT
      INTEGER  OK

C                         Geometry variables formed at initialization
      INTEGER  BOLT_DIM
      PARAMETER ( BOLT_DIM = 128 )
      REAL     R_BOLT_ARRAY(BOLT_DIM, 2)
      REAL     PHI_BOLT_ARRAY(BOLT_DIM, 2)
      INTEGER  TOTAL_BOLTS

      SAVE     R_BOLT_ARRAY,  PHI_BOLT_ARRAY,TOTAL_BOLTS

C                         Variables formed at initialization
      REAL     ERR_FRAC
      REAL     E_FRAC
      REAL     TAN_CORR
      REAL     R_SIGMA
      REAL     R_SIG_FRAC_B
      REAL     R_SIG_RAT
      REAL     PHI_SIGMA
      REAL     PHI_SIG_FRAC_4
      REAL     PHI_SIG_RAT
      REAL     POS_LENGTH
      REAL     POS_E_SCALE
      REAL     POS_OFFSET

      REAL     PHI_SIG_FRAC_2
      REAL     PHI_SIGMA_2Z
      REAL     PHI_SIGMA_4Z

      REAL     R_SIG_FRAC_A
      REAL     R_SIGMA_AZ
      REAL     R_SIGMA_BZ

      SAVE     ERR_FRAC,      E_FRAC,        TAN_CORR
      SAVE     R_SIG_FRAC_A,  R_SIG_FRAC_B,  R_SIGMA_AZ,   R_SIGMA_BZ
      SAVE     PHI_SIG_FRAC_2,PHI_SIG_FRAC_4,PHI_SIGMA_2Z, PHI_SIGMA_4Z
      SAVE     POS_LENGTH,    POS_E_SCALE,   POS_OFFSET

C                         Position variables
      REAL     PHI_EM3
      REAL     R_EM3
      REAL     TAN_THETA
      REAL     R_BOLT
      REAL     PHI_BOLT

C                         Variables used to find the nearest bolt
      INTEGER  ENDCAP
      INTEGER  N_CLOSE
      REAL     LEN_SQ_BEST
      REAL     LEN_SQ
      REAL     DELPHI
      INTEGER  I

C                         Variables used in forming the r dependence
      REAL     TAN_FAC
      REAL     R_SHIFT
      REAL     R_ZERO
      REAL     R_SIGMA_A
      REAL     R_SIGMA_B
      REAL     R_GAUSS_A
      REAL     R_GAUSS_B
      REAL     R_PEAK
      REAL     R_CORR

C                         Variables used in forming the phi dependence
      REAL     PHI_SIGMA_2
      REAL     PHI_SIGMA_4
      REAL     PHI_DIFF
      REAL     PHI_GAUSS_2
      REAL     PHI_GAUSS_4
      REAL     PHI_CORR

C                         Variables used to form the final correction
      REAL     F_CORR
      REAL     F_DEL

C                         Miscellaneous variables
      REAL     PI
      PARAMETER ( PI = 3.141592653589793 )

      INTEGER  IER
      LOGICAL  FIRST
      SAVE     FIRST
      DATA     FIRST /.TRUE./

C----------------------------------------------------------------------

C--->     Local initialization.

      IF ( FIRST )  THEN
         FIRST = .FALSE.

C                         Get the bolt geometry

         CALL ECEMCR_BOLT_GEOM( BOLT_DIM,
     &           R_BOLT_ARRAY, PHI_BOLT_ARRAY, TOTAL_BOLTS )

C                         Read parameters from the RCP file.

         CALL EZPICK('ECEMCR_RCP')
         CALL EZGET('ECEM_BOLT_ERR_FRACTION',       ERR_FRAC,       IER)
         CALL EZGET('ECEM_BOLT_E_FRACTION',         E_FRAC,         IER)
         CALL EZGET('ECEM_BOLT_TAN_CORR',           TAN_CORR,       IER)
         CALL EZGET('ECEM_BOLT_R_SIGMA',            R_SIGMA,        IER)
         CALL EZGET('ECEM_BOLT_R_SIGMA_FRACTION',   R_SIG_FRAC_B,   IER)
         CALL EZGET('ECEM_BOLT_R_SIGMA_RATIO',      R_SIG_RAT,      IER)
         CALL EZGET('ECEM_BOLT_PHI_SIGMA',          PHI_SIGMA,      IER)
         CALL EZGET('ECEM_BOLT_PHI_SIGMA_FRACTION', PHI_SIG_FRAC_4, IER)
         CALL EZGET('ECEM_BOLT_PHI_SIGMA_RATIO',    PHI_SIG_RAT,    IER)
         CALL EZGET('ECEM_BOLT_POS_LENGTH',         POS_LENGTH,     IER)
         CALL EZGET('ECEM_BOLT_POS_E_SCALE',        POS_E_SCALE,    IER)
         CALL EZGET('ECEM_BOLT_POS_OFFSET',         POS_OFFSET,     IER)

C                         Form the separate widths.

         PHI_SIG_FRAC_2 = 1. - PHI_SIG_FRAC_4
         PHI_SIGMA_2Z   = PHI_SIGMA /
     &      SQRT( PHI_SIG_FRAC_2 + PHI_SIG_FRAC_4 * PHI_SIG_RAT**2 )
         PHI_SIGMA_4Z   = PHI_SIGMA_2Z * PHI_SIG_RAT

         R_SIG_FRAC_A   = 1. - R_SIG_FRAC_B
         R_SIGMA_AZ     = R_SIGMA /
     &      SQRT( R_SIG_FRAC_A + R_SIG_FRAC_B * R_SIG_RAT**2 )
         R_SIGMA_BZ     = R_SIGMA_AZ * R_SIG_RAT
      ENDIF

C--->     Give up if bolts improperly defined

      IF ( TOTAL_BOLTS.EQ.0 )  THEN
          OK = -1
          DE_BOLT = 0.
          ERR_DE_BOLT = 0.
          RETURN
      ELSE
          OK = 0
      ENDIF

C--->     Calculate positions.

C                         Calculate the position of the cluster.

      PHI_EM3   = ATAN2( Y_EM3, X_EM3 )
      R_EM3     = SQRT( X_EM3**2 + Y_EM3**2 )
      TAN_THETA = R_EM3 / ABS( Z_EM3_MOD - ZV1 )

C                         Find the nearest bolt

      IF ( Z_EM3_MOD.LT.0. )  THEN
          ENDCAP = 1
      ELSE
          ENDCAP = 2
      ENDIF

      LEN_SQ_BEST = 1.E10
      DO 100 I = 1,TOTAL_BOLTS
          DELPHI = PHI_EM3 - PHI_BOLT_ARRAY(I,ENDCAP)
          LEN_SQ = R_EM3**2 + R_BOLT_ARRAY(I,ENDCAP)**2
     &           - 2. * R_EM3 * R_BOLT_ARRAY(I,ENDCAP) * COS( DELPHI )
          IF ( LEN_SQ.LT.LEN_SQ_BEST )  THEN
              LEN_SQ_BEST = LEN_SQ
              N_CLOSE = I
          ENDIF
  100 CONTINUE

      R_BOLT   = R_BOLT_ARRAY(N_CLOSE,ENDCAP)
      PHI_BOLT = PHI_BOLT_ARRAY(N_CLOSE,ENDCAP)

C--->     Form the bolt corrections

C                         Form the radial dependent factors

      TAN_FAC   = TAN_THETA * ( 1. - TAN_CORR * TAN_THETA )
      R_SHIFT   = - TAN_THETA * POS_LENGTH * LOG( E_MEAS / POS_E_SCALE )
      R_ZERO    = R_BOLT + POS_OFFSET + R_SHIFT
      R_SIGMA_A = R_SIGMA_AZ * TAN_FAC
      R_SIGMA_B = R_SIGMA_BZ * TAN_FAC

      R_GAUSS_A = EXP( -0.5 * ( (R_EM3-R_ZERO) / R_SIGMA_A )**2 )
      R_GAUSS_B = EXP( -0.5 * ( (R_EM3-R_ZERO) / R_SIGMA_B )**2 )

      R_PEAK = ( E_FRAC / TAN_FAC )
      R_CORR = R_PEAK
     &       * ( R_SIG_FRAC_A * R_GAUSS_A + R_SIG_FRAC_B * R_GAUSS_B )

C                         Form the angle dependent factors

      PHI_SIGMA_2 = PHI_SIGMA_2Z / R_BOLT
      PHI_SIGMA_4 = PHI_SIGMA_4Z / R_BOLT

      PHI_DIFF = MOD( PHI_EM3 - PHI_BOLT , 2.*PI )
      IF ( ABS(PHI_DIFF).GT.PI )
     &     PHI_DIFF = PHI_DIFF - SIGN( 2.*PI , PHI_DIFF )

      PHI_GAUSS_2 =
     &     EXP( -0.5 * ( PHI_DIFF / PHI_SIGMA_2 )**2 )
      PHI_GAUSS_4 =
     &     EXP( -1./9. * ( PHI_DIFF / PHI_SIGMA_4 )**4 )

      PHI_CORR =
     &     PHI_SIG_FRAC_2 * PHI_GAUSS_2 + PHI_SIG_FRAC_4 * PHI_GAUSS_4

C                         Form the final correction

      F_CORR = R_CORR * PHI_CORR
      F_DEL  = - F_CORR / ( 1. + F_CORR )

      DE_BOLT = E_MEAS * F_DEL
      ERR_DE_BOLT = DE_BOLT * ERR_FRAC

  999 RETURN
      END
