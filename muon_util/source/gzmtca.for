      INTEGER FUNCTION GZMTCA(IMUON)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get pointer to MTCA bank
C-
C-   Returned value  : 
C-   Inputs  : IMUON = MUON bank ID
C-
C-   Created   9-FEB-1994   Daria Zieminska
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMTCA.LINK'
      INTEGER IMUON,LMUON,GZMUON
      GZMTCA=0
      LMUON=GZMUON(IMUON)
      IF(LMUON.NE.0) THEN
        GZMTCA=LQ(LMUON-IZMTCA)
      ENDIF
  999 RETURN
      END
