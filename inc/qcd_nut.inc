C----------------------------------------------------------------------
C-
C-   Created   20-DEC-1992  Andrew G. Brandt
C-   Updated   24-SEP-1993  Keep 2 NUT Banks (2,3) or (4,5)
C-   Updated   02-NOV-1994  update for CW
C----------------------------------------------------------------------
C
C QCD_NUT1
C
      COMMON /QCD_NUT1/NUT_ET(2),NUT_PHI(2)
C
      REAL    NUT_ET,NUT_PHI
      CHARACTER*255 QCD_NUT1_TAG
      DATA QCD_NUT1_TAG/'NUT_ET2:R,NUT_ET4:R,NU_PHI2:R,NU_PHI4:R'/
C
C QCD_NUT2
C
      COMMON /QCD_NUT2/NUT_E(2),NUT_ETA(2),
     &                NUT_SCET(2),NUT_SGX(2),NUT_SGY(2),NUT_SGET(2)
C
      REAL    NUT_E,NUT_ETA,NUT_SCET,NUT_SGX,
     &        NUT_SGY,NUT_SGET
      CHARACTER*255 QCD_NUT2_TAG
      DATA QCD_NUT2_TAG/'NU_E2:R,NU_E4:R,NU_ETA2:R,NU_ETA4:R,
     &   NU_SCET2:R,NU_SCET4:R,NU_SGX2:R,NU_SGX4:R,
     &   NU_SGY2:R,NU_SGY4:R,NU_SGET2:R,NU_SGET4:R'/
