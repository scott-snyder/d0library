C----------------------------------------------------------------------
C-
C-   Created  17-SEP-1992   Myungyun Pang
C-
C-   Common block to hold values necessary for VTX offline calibration
C-
C----------------------------------------------------------------------
      INTEGER NMAX_LAYER,NMAX_SECTOR,NMAX_WIRE
      PARAMETER (NMAX_LAYER=2)
      PARAMETER (NMAX_SECTOR=31)
      PARAMETER (NMAX_WIRE=7)
      COMMON /VTXCOFF/ EPSILON, R_INPUT, AREA_DIST, AREA_GAIN,
     &  AREA_DIST_ERR, AREA_GAIN_ERR
      REAL EPSILON(0:NMAX_WIRE,0:NMAX_SECTOR,0:NMAX_LAYER)
      REAL R_INPUT(2,0:NMAX_WIRE,0:NMAX_SECTOR,0:NMAX_LAYER)
      REAL AREA_DIST(-20:20,0:2,0:NMAX_LAYER)
      REAL AREA_GAIN(0:NMAX_WIRE,0:NMAX_SECTOR,0:NMAX_LAYER)
      REAL AREA_DIST_ERR(-20:20,0:2,0:NMAX_LAYER)
      REAL AREA_GAIN_ERR(0:NMAX_WIRE,0:NMAX_SECTOR,0:NMAX_LAYER)
