C----------------------------------------------------------------------
C-
C-permanent link area for CL2 conversion routines
C-
C-   Created  17-FEB-1991   Richard Astur
C-   Updated  18-DEC-1992   William Cobau  - add L2GLOB 
C-
C----------------------------------------------------------------------
      INTEGER CL2NSLNK
      PARAMETER(CL2NSLNK = 20)
      INTEGER CL2SLNK( CL2NSLNK )
      COMMON /CL2_LINK/ CL2SLNK
C
      INTEGER L2CAD1,L2CAD2,L2CAEP,L2PNUT,L2CAEH,L2CATE,L2GLOB
      EQUIVALENCE( L2CAD1 , CL2SLNK(1) )
      EQUIVALENCE( L2CAD2 , CL2SLNK(2) )
      EQUIVALENCE( L2CAEP , CL2SLNK(3) )
      EQUIVALENCE( L2PNUT , CL2SLNK(4) )
      EQUIVALENCE( L2CAEH,  CL2SLNK(5) )
      EQUIVALENCE( L2CATE , CL2SLNK(6) )
      EQUIVALENCE( L2GLOB , CL2SLNK(7) )
C---Add more links in a like manner...
