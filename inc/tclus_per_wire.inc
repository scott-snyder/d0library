C----------------------------------------------------------------------
C-
C-   Created  19-OCT-1993   A. Zylberstejn
C-
C----------------------------------------------------------------------
      INTEGER NCLMAX,nbwire_max
      PARAMETER( NCLMAX = 50 )
      PARAMETER( nbwire_max=6)
      COMMON /TCLUS_PER_WIRE/
     &  NTOT_CL(6),NCL_PER_WIRE(nbwire_max,6),ECL_PER_WIRE(NCLMAX,6),
     &  LEFT_CL(NCLMAX,6),RIGHT_CL(NCLMAX,6),
     &  CENTER_CL(NCLMAX,6),HEIGTH_CL(NCLMAX,6)
      INTEGER
     &  NTOT_CL,NCL_PER_WIRE, LEFT_CL,RIGHT_CL, CENTER_CL
      REAL
     &  ECL_PER_WIRE, HEIGTH_CL

