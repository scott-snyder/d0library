      SUBROUTINE ECEMCR_BOLT_GEOM
     &      ( BOLT_DIM, R_BOLT, PHI_BOLT, TOTAL_BOLTS )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Get the ECEM bolt positions.
C-
C-             We get the bolt positions for one octant from the RCP file,
C-             then replicate the positions in the other octants and in the
C-             other calorimeter endcap.  Any survey corrections added to
C-             the cells by CELXYZ are added here to the bolt positions.
C-
C-             This is a temporary routine.  Eventually the bolt
C-             positions will be described in the calorimeter geometry
C-             RCP files and will be set up (complete with survey
C-             corrections) by the appropriate geometry routines.
C-
C-   Inputs  : BOLT_DIM     Bolt array dimension
C-   Outputs : R_BOLT       Bolt r position (centimeters)
C-             PHI_BOLT     Bolt phi position (radians)
C-             TOTAL_BOLTS  Number of bolts in each end calorimeter
C-   Controls:
C-
C-   Created    1-OCT-1992 Orin Dahl
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------

      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$PARAMS:CLIN.PARAMS'

C                         Input and output variables
      INTEGER  BOLT_DIM
      REAL     R_BOLT(BOLT_DIM,2)
      REAL     PHI_BOLT(BOLT_DIM,2)
      INTEGER  TOTAL_BOLTS

C                         Bolt variables
      INTEGER  NUM_BOLTS
      INTEGER  BOLT_SECS
      REAL     PHI_OFFSET

C                         Positions
      REAL     XNOM(3)
      REAL     XSURV(3)
      REAL     Z_EM3_MOD

C                         Returned but not used
      REAL     DZED
      REAL     CENRAD
      REAL     DRAD
      REAL     TILT
      REAL     ARGSOK
      REAL     X
      REAL     Y
      REAL     Z

C                         Miscellaneous variables
      INTEGER  I
      INTEGER  J
      INTEGER  K
      INTEGER  KK

      INTEGER  IER
      CHARACTER MSG*80

      REAL     PI
      PARAMETER ( PI = 3.141592653589793 )
      REAL     CM_PER_INCH
      PARAMETER ( CM_PER_INCH = 2.54 )

C                         Nominal positions
      INTEGER  JLAYER
      PARAMETER ( JLAYER = 3 )
      INTEGER  JPHI
      PARAMETER ( JPHI = 1 )
      INTEGER  IETA(2)
      DATA     IETA / -20, +20 /

C----------------------------------------------------------------------


C--->       Form the nominal positions

C                         Get the data from the RCP file.

      CALL EZPICK('ECEMCR_RCP')
      CALL EZGET('ECEM_BOLT_GEOM_NUMBER',       NUM_BOLTS,      IER)
      CALL EZGET('ECEM_BOLT_GEOM_SECTIONS',     BOLT_SECS,      IER)
      CALL EZGETA('ECEM_BOLT_GEOM_RADIUS',
     &               1, NUM_BOLTS, 1,           R_BOLT,         IER)
      CALL EZGETA('ECEM_BOLT_GEOM_ANGLE',
     &               1, NUM_BOLTS, 1,           PHI_BOLT,       IER)

C                         Total number of bolts in each end calorimeter

      TOTAL_BOLTS = BOLT_SECS * NUM_BOLTS

C                         Give up if improper number of bolts

      IF ( TOTAL_BOLTS.LE.0 .OR. TOTAL_BOLTS.GT.BOLT_DIM )  THEN
         WRITE(MSG,20)  TOTAL_BOLTS, BOLT_DIM
   20    FORMAT(I4,' ECEM bolts, maximum is',I4,'  -- no correction')
         CALL ERRMSG( 'IMPROPER NUMBER OF ECEM BOLTS',
     &          'ECEMCR_BOLT_GEOM', MSG, 'W' )
         TOTAL_BOLTS = 0
         RETURN
      ENDIF

C                         Convert to centimeters and radians.

      DO 210 I = 1,NUM_BOLTS
         R_BOLT(I,1)   = R_BOLT(I,1) * CM_PER_INCH
         PHI_BOLT(I,1) = PHI_BOLT(I,1) * (PI/180.)
  210 CONTINUE

C                         Replicate the sections

      PHI_OFFSET = (2.*PI) / REAL( BOLT_SECS )

      DO 230 J = 2,BOLT_SECS
         KK = (J-1) * NUM_BOLTS
         DO 220 I = 1,NUM_BOLTS
            R_BOLT(KK+I,1)   = R_BOLT(I,1)
            PHI_BOLT(KK+I,1) = PHI_BOLT(I,1) + REAL(J-1) * PHI_OFFSET
  220    CONTINUE
  230 CONTINUE

C                         Load the other end calorimeter

      DO 240 I = 1,TOTAL_BOLTS
         R_BOLT(I,2)   = R_BOLT(I,1)
         PHI_BOLT(I,2) = PHI_BOLT(I,1)
  240 CONTINUE

C--->     Include the survey correctiions

      DO 260 K = 1,2

C                         Get Z of EM3 layer

        CALL CALZED( IETA(K), JLAYER,
     &                 Z_EM3_MOD, DZED, CENRAD, DRAD, TILT, ARGSOK )

C                         Use CELXYZ to set pointer to alignment banks.

        LQCLIN = 0
        CALL CELXYZ( IETA(K), JPHI, JLAYER, X, Y, Z, IER )

C                         Transform X' = <X> - del + R * (X-<X>)

        IF ( LQCLIN.NE.0 .AND. IER.EQ.0 )  THEN
          DO 250 I = 1,TOTAL_BOLTS

             XNOM(1) = R_BOLT(I,K) * COS( PHI_BOLT(I,K) )
             XNOM(2) = R_BOLT(I,K) * SIN( PHI_BOLT(I,K) )
             XNOM(3) = Z_EM3_MOD

             CALL CAL_SURVEY_TRANSF( XNOM, C(LQCLIN+IGDTX),
     &              C(LQCLIN+IGR11), C(LQCLIN+IGMDLX), XSURV )

             R_BOLT(I,K)   = SQRT( XSURV(1)**2 + XSURV(2)**2 )
             PHI_BOLT(I,K) = ATAN2( XSURV(2) , XSURV(1) )

  250     CONTINUE
        ENDIF

  260 CONTINUE

C--->     All done

  999 RETURN
      END
