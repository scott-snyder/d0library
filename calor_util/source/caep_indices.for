      SUBROUTINE CAEP_INDICES(PACKED_WORD,IETA,IPHI,LAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-   given the CAEP word with packed adress return the physics indices
C-   Inputs  : 
C-     PACKED_WORD= CAEP word with packed address
C-   Outputs : 
C-   IETA = eta index
C-   IPHI = phi index
C-   LAYER= layer index
C-
C-   Created   4-APR-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER PACKED_WORD
      INTEGER PAKADR,IETA,IPHI,LAYER
      BYTE BYTES(4)
      EQUIVALENCE (PAKADR,BYTES)
C----------------------------------------------------------------------
C
      PAKADR=PACKED_WORD
C
C           unpack addresses
      IETA=BYTES(BYTE4)
      IPHI=BYTES(BYTE3)
      LAYER=BYTES(BYTE2)
  999 RETURN
      END
