      SUBROUTINE ECEMCR_CRACK_GEOM
     &      ( CRACK_DIM, X_CRACK, DXDY_CRACK, NUM_CRACKS )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Get the ECEM crack positions.
C-
C-             We get the crack positions for one calorimeter endcap from
C-             the RCP file, then replicate the positions in the other
C-             calorimeter endcap.  Any survey corrections added to the
C-             cells by CELXYZ are added here to the crack positions.
C-
C-             This is a temporary routine.  Eventually the crack
C-             positions will be described in the calorimeter geometry
C-             RCP files and will be set up (complete with survey
C-             corrections) by the appropriate geometry routines.
C-
C-   Inputs  : CRACK_DIM    Crack array dimension
C-   Outputs : X_CRACK      Crack x position at y = 0
C-             DXDY_CRACK   Crack dx/dy
C-             NUM_CRACKS   Number of cracks in each end calorimeter
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
      INTEGER  CRACK_DIM
      REAL     X_CRACK(CRACK_DIM,2)
      REAL     DXDY_CRACK(CRACK_DIM,2)
      INTEGER  NUM_CRACKS

C                         Positions
      REAL     XNOM(3)
      REAL     XSURV_L(3)
      REAL     XSURV_H(3)
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
      INTEGER  K

      INTEGER  IER
      CHARACTER MSG*80

      REAL     CM_PER_INCH
      PARAMETER ( CM_PER_INCH = 2.54 )

C                         Nominal positions
      REAL     Y_TRANS
      PARAMETER ( Y_TRANS = 50. )
      INTEGER  JLAYER
      PARAMETER ( JLAYER = 3 )
      INTEGER  JPHI
      PARAMETER ( JPHI = 1 )
      INTEGER  IETA(2)
      DATA     IETA / -20, +20 /

C----------------------------------------------------------------------


C--->     Form the nominal positions

C                         Get the data from the RCP file.

      CALL EZPICK('ECEMCR_RCP')
      CALL EZGET('ECEM_CRACK_GEOM_NUMBER',      NUM_CRACKS,     IER)
      CALL EZGETA('ECEM_CRACK_GEOM_X',
     &               1, NUM_CRACKS, 1,          X_CRACK,        IER)
      CALL EZGETA('ECEM_CRACK_GEOM_DXDY',
     &               1, NUM_CRACKS, 1,          DXDY_CRACK,     IER)

C                         Give up if improper number of cracks

      IF ( NUM_CRACKS.LE.0 .OR. NUM_CRACKS.GT.CRACK_DIM )  THEN
         WRITE(MSG,10)  NUM_CRACKS, CRACK_DIM
   10    FORMAT(I4,' ECEM cracks, maximum is',I4,'  -- no correction')
         CALL ERRMSG( 'IMPROPER NUMBER OF ECEM CRACKS',
     &          'ECEMCR_CRACK_GEOM', MSG, 'W' )
         NUM_CRACKS = 0
         RETURN
      ENDIF

C                         Convert to centimeters.

      DO 110 I = 1,NUM_CRACKS
         X_CRACK(I,1) = X_CRACK(I,1) * CM_PER_INCH
  110 CONTINUE

C                         Load the other endcap

      DO 140 I = 1,NUM_CRACKS
         X_CRACK(I,2)    = X_CRACK(I,1)
         DXDY_CRACK(I,2) = DXDY_CRACK(I,1)
  140 CONTINUE

C--->     Include the survey correctiions

      DO 160 K = 1,2

C                         Get Z of EM3 layer

        CALL CALZED( IETA(K), JLAYER,
     &                 Z_EM3_MOD, DZED, CENRAD, DRAD, TILT, ARGSOK )

C                         Use CELXYZ to set pointer to alignment banks.

        LQCLIN = 0
        CALL CELXYZ( IETA(K), JPHI, JLAYER, X, Y, Z, IER )

C                         Transform X' = <X> - del + R * (X-<X>)

        IF ( LQCLIN.NE.0 .AND. IER.EQ.0 )  THEN
          DO 150 I = 1,NUM_CRACKS

C                                Transform point x,-y

             XNOM(1) = X_CRACK(I,K) - Y_TRANS * DXDY_CRACK(I,K)
             XNOM(2) = -Y_TRANS
             XNOM(3) = Z_EM3_MOD
             CALL CAL_SURVEY_TRANSF( XNOM, C(LQCLIN+IGDTX),
     &              C(LQCLIN+IGR11), C(LQCLIN+IGMDLX), XSURV_L )

C                                Transform point x,+y


             XNOM(1) = X_CRACK(I,K) + Y_TRANS * DXDY_CRACK(I,K)
             XNOM(2) = Y_TRANS
             XNOM(3) = Z_EM3_MOD
             CALL CAL_SURVEY_TRANSF( XNOM, C(LQCLIN+IGDTX),
     &              C(LQCLIN+IGR11), C(LQCLIN+IGMDLX), XSURV_H )

C                                Form transformed x_zero and dx/dy

             X_CRACK(I,K) =
     &           ( XSURV_H(2) * XSURV_L(1) - XSURV_H(1) * XSURV_L(2) )
     &         / ( XSURV_H(2) - XSURV_L(2) )
             DXDY_CRACK(I,K) =
     &         ( XSURV_H(1) - XSURV_L(1) ) / ( XSURV_H(2) - XSURV_L(2) )

  150     CONTINUE
        ENDIF

  160 CONTINUE

C--->     All done

  999 RETURN
      END
