C----------------------------------------------------------------------
C-
C-Temporary Link Area for CL2TEST
C-
C-   Created  17-JUN-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      INTEGER CL2TEST_LNKFLG(2)
      INTEGER NTLNK
      PARAMETER (NTLNK = 10)
      INTEGER CL2TEST_LINK( NTLNK )
      COMMON /CL2TEST_LINK/ CL2TEST_LNKFLG,CL2TEST_LINK
C---Links
      INTEGER LCAEP,LCAEH
      EQUIVALENCE( LCAEP, CL2TEST_LINK(1) ) 
      EQUIVALENCE( LCAEH, CL2TEST_LINK(2) )
