        SUBROUTINE MU_FINE(IFINE,JFINE,MFINE,NFINE)
C--------------------------------------------------------------
C  Fill or clear muon fine centroid arrays
C
C  INPUT:  IFINE - Input array of fine centroids from one MAC
C          JFINE - Number of fine centroids (-1 to clear array)
C
C  OUTPUT: MFINE - Output array of fine centroids
C          NFINE - Number of output fine centroids, MAX = 32
C
C  Created 11-91  M. Fortner
C  rename from MUFINE to MU_FINE  1-18-92 K. Bazizi
C--------------------------------------------------------------
        IMPLICIT NONE
        INTEGER IFINE(32),JFINE,MFINE(32),NFINE
        INTEGER I,II,IND
C
        IF (JFINE.LT.0) THEN
          DO I=1,32
            MFINE(I)=0
          ENDDO
          NFINE=0
        ELSE
          DO II=1,JFINE
            IND=II+NFINE
            IF (IND.LE.32) MFINE(IND)=IFINE(II)
          END DO
          NFINE=NFINE+JFINE
          IF (NFINE.GT.32) NFINE=32
        END IF
C--------------------------------------------------------------
        RETURN
        END
