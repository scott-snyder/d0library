      SUBROUTINE GTFDCT_VERTEX(ITRACK,IVERT,CHISQ,THT,ETHT,PHI,EPHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the FDCT track information based on fit
C-   including the vertex position
C-
C-   Inputs  : ITRACK    = FDC Track Number (= IQ(LFDCT-5))
C-   Outputs : IVERT [I] = Vertex with which the track is associated with
C-             CHISQ     = Chi**2 of fit
C-             THT,ETHT  = Theta and Error on Theta
C-             PHI,EPHI  = Phi and Error on Phi
C-   Controls: none
C-
C-   Created  12-DEC-1993   Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER ITRACK,IVERT
      INTEGER LFDCT,GZFDCT
      REAL CHISQ,THT,ETHT,PHI,EPHI
C----------------------------------------------------------------------
C
      LFDCT=GZFDCT(ITRACK)
      IF (LFDCT.LE.0) THEN
        IVERT = 0
        CHISQ = 0.
        THT = 0.
        ETHT = 999.
        PHI = 0.
        EPHI = 999.
        GO TO 999
      ENDIF
C
      THT   = Q(LFDCT+26)
      ETHT  = Q(LFDCT+27)
      PHI   = Q(LFDCT+28)
      EPHI  = Q(LFDCT+29)
      CHISQ = Q(LFDCT+30)
      IVERT = IQ(LFDCT+31)
C
  999 RETURN
      END
