      FUNCTION CL2_CAGS(ICRATE,ADDR,SCALE,IETA,IPHI,LYR,DO_GNSCOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      calculate Sin(theta nominal) * gain corr * ADC_TO_GEV
C-   Inputs  : ICRATE           [I]     crate number [0,63]
C-             ADDR             [I]     13 bit hardware address
C-             SCALE            [I]     Scale bit: 0 for X8, 1 for X1
C-             IETA,IPHI,LYR    [I]     offline indices of channel
C-   Outputs : CL2_CAGS         [F] the advertised factor
C-   Controls: DO_GNSCOR        [L]     .TRUE. means use gain from DBL3
C-
C-   Created  25-APR-1991   James T. Linnemann
C-   Updated  11-FEB-1993   James T. Linnemann  map negative gains to zero 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ICRATE,ADDR,SCALE
      INTEGER IETA,IPHI,LYR
      REAL    CL2_CAGS
      LOGICAL DO_GNSCOR
      REAL    GAIN_CORR,GNSIN
      REAL    CAD_GAIN                  ! function for adc_to_gev factor
      REAL    GNARRAY(3)  ! (1) = K (E=K*(PH-PED))  (2) = SIGMA  (3) = PEDESTAL
      INTEGER IER,IOK                   ! error flags
      REAL    X,Y,Z                     ! cell center
      INTEGER NVAL                      ! number of values returned by GTCGEV
C----------------------------------------------------------------------
      CL2_CAGS = 0.0
C
C...calculate nominal sin theta (assuming z of vertex = 0)
      CALL CELXYZ(IETA,IPHI,LYR,X,Y,Z,IOK)
      IF (IOK.EQ.0) THEN
        GNSIN = 1.0 / SQRT( 1.0 +  (Z**2)/(X**2+Y**2) )
C
C...electronic gain correction
        GAIN_CORR = 1.0
        CALL GTCGEV(LYR,IPHI,IETA,SCALE,NVAL,GNARRAY,IER)
        GAIN_CORR = GNARRAY(1)
C          CALL CAEPFL_PEDGNS('CAHITS_RCP',ICRATE,ADDR,SCALE,GAIN_CORR,
C     &      IER)
        IF (IER.EQ.0) THEN
C
C...sampling fraction and adc to GeV factor
          CL2_CAGS= MAX(0.0,GAIN_CORR*GNSIN)
        ENDIF
      ENDIF
  999 RETURN
      END
