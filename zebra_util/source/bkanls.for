      SUBROUTINE BKANLS(LANLS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book Zebra bank ANLS
C-
C-   Inputs  : 
C-   Outputs : Pointer to ANLS bank
C-   Controls: 
C-
C-   Created  20-AUG-1991   Andrew J. Milder
C-   Updated   9-MAR-1994   Serban Protopopescu  
C-    increase number of links
C-   Updated   2-OCT-1995   Dhiman Chakraborty   
C-    increase number of links to 8
C-   Updated  12-OCT-1995   Dhiman Chakraborty   
C-    increase number of links to 10
C-   Updated  13-OCT-1995   Dhiman Chakraborty   
C-    increase number of links to 15
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZANLS.LINK/LIST'
      INTEGER LANLS
      INTEGER  NDATA, NLINKS
      PARAMETER( NDATA = 0 )
      PARAMETER( NLINKS = 15 )
      INTEGER MORED,MOREL
C----------------------------------------------------------------------
      LANLS = LQ(LHEAD-IZANLS)
      IF( LANLS.LE.0 ) THEN
        CALL MZBOOK(IXMAIN,LANLS,LHEAD,-IZANLS,'ANLS',
     &    NLINKS,NLINKS,NDATA,0,0)
      ELSE
        MOREL=NLINKS-IQ(LANLS-3)
        MORED=NDATA-IQ(LANLS-1)
        IF (MOREL.GT.0.OR.MORED.GT.0) 
     &    CALL MZPUSH(IXCOM,LANLS,MOREL,MORED,' ')
      ENDIF
  999 RETURN
      END
