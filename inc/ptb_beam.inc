C----------------------------------------------------------------------
C-
C-   Created  12-FEB-1990   C.E.Cretsinger
C-
C----------------------------------------------------------------------
      COMMON /PTB_BEAM/ XBEAM,YBEAM,SLOPE,INTC,DXDZUP,DXDZDN,DYDZ,
     &  XINTUP,XINTDN,YINT,NXUP

      REAL XBEAM(8),YBEAM(8),SLOPE,INTC
      REAL DXDZUP(2)
      REAL DXDZDN
      REAL DYDZ
      REAL XINTUP(2)
      REAL XINTDN
      REAL YINT
      INTEGER NXUP
