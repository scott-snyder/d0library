      SUBROUTINE C_SETUP_PWCPAR(IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : IER2 : error code from TBD0
C-   Controls: fill in UVEC and VERT from the PWC track in CTRAK.INC 
C-
C-   Created  16-DEC-1991   Meenakshi Narain, Scott Snyder
C-   Updated  10-MAR-1992   Meenakshi Narain  update GTTBD0 call 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:CTRAK.INC'
C
      INTEGER TAGBITS, K, IER
C
      REAL VTX(3), VTX2(3), CRYO(2), HIT(2), SIG(5)
      REAL TRACK_DIR(3), DR, PWC_ETA, PWC_PHI, THETA
      REAL DELETA, DELPHI, EFF_ETA, EFF_PHI, ENERGY

C----------------------------------------------------------------------
C
      CALL GTTBD0(PWC_ETA, PWC_PHI, VTX, SIG, VTX2, CRYO, HIT, IER,
     &  ENERGY, TAGBITS)
      IF (IER .NE. 0) RETURN
C
      THETA = 2*ATAN(EXP(-PWC_ETA))
      UVEC(1,1) = COS(PWC_PHI) * SIN(THETA)
      UVEC(2,1) = SIN(PWC_PHI) * SIN(THETA)
      UVEC(3,1) = COS(THETA)

      DO K=1, 3
        VERT(K) = VTX(K)
      ENDDO
      
  999 RETURN
      END
