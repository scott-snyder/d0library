      SUBROUTINE CACMPR(ETA,PHI,IETA,IPHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate IETA and IPHI bins for the EC region.
C-
C-   Inputs  : ETA   = eta of point
C-             PHI   = phi of point
C-   Outputs : IETA  = Physics system Eta index
C-             IPHI  = Physics system Phi index
C-   Controls:
C-
C-   Created  31-JAN-1989   Alan M. Jonckheere
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C INPUT/OUTPUT VARIABLES
      REAL ETA,PHI
      INTEGER IETA,IPHI
C
C COMPRESSION VARIABLES
      INTEGER NCOMPR
      PARAMETER( NCOMPR = 5 )
      REAL CETA(NCOMPR),CETAP
      INTEGER NCETA
C
C UTILITY VARIABLES
C
C COMPRESSION POINTS (ETA LIMIT, ETA COMPRESSION FACTOR, PHI COMPRESSION FACTOR
      DATA CETA /3.2,3.4,3.7,4.1,4.4/      ! Non-"normal" Eta boundaries
      DATA NCETA/32/                       ! Last "normal" eta cell
      DATA CETAP/3.2/                      ! Eta where PHI granularity changes
C
C 'DPHI' and 'DETA' steps
      REAL DPHI,DETA
C
C DELTA PHI (RADIANS)
      DATA DPHI/.098175/
C DELTA ETA
      DATA DETA/0.1/
C
C LOCAL VARIABLES
      INTEGER I
C
C----------------------------------------------------------------------
C
      IPHI = INT(PHI/DPHI) + 1                  ! CALCULATE PHI INDEX
      IETA = INT(ETA/DETA) + SIGN(1.,ETA)       ! CALCULATE ETA INDEX
C
C ****  COMPRESS ETA IF REQUIRED
      IF ( ABS(ETA).GT.CETA(1) ) THEN
        IETA = NCETA
        DO I = 1,NCOMPR
          IF(ABS(ETA).GT.CETA(I)) THEN
            IETA = IETA + 1
          ENDIF
        ENDDO
        IETA = SIGN(1.,ETA)*IETA
      ENDIF
C
C ****  COMPRESS PHI IF REQUIRED
      IF ( ABS(ETA).GT.CETAP ) THEN
        IPHI = INT((IPHI-1)/2)*2 + 1
      ENDIF
  999 RETURN
      END
