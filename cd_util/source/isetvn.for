

      INTEGER FUNCTION ISETVN (ISTAT,NVERS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set version number in ZEBRA bank status word
C-                         in bits 13-17 (bit 0 is lowest order bit)  
C-                         as is standard for all CD ZEBRA banks.
C-
C-   Note :  Version # can be retrieved from JSTAT = ISETVN(ISTAT,NVERS)
C-          using NVERS = IBITS(JSTAT,13,5). 
C-
C-   Returned value  : ISETVN is ISTAT with version # replacing bits 13-17
C-   Inputs  : ISTAT = ZEBRA status word IQ(Lbank).     
C-             NVERS = integer version number in range 0-31.
C-   Outputs : none
C-   Controls: none
C-
C-   Created  20-JUN-1989   Thomas G. Trippe
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ISTAT,NVERS,ISHIFT,MASK
      PARAMETER (ISHIFT=2**13)
      PARAMETER (MASK=31*ISHIFT)   ! ones in bits 13-17
C----------------------------------------------------------------------
C
      ISETVN=IOR( IAND(NVERS*ISHIFT,MASK) , IAND(ISTAT,NOT(MASK)) )
  999 RETURN
      END
