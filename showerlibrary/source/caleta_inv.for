      SUBROUTINE CALETA_INV(ETA,IETA,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given Pseudo rapidity of point, gives the Eta Cell
C-   number
C-
C-   Inputs  : ETA = Real signed pseudo rapidity
C-   Outputs : IETA = signed Integer Cell index
C-             IERR = non zero, ETA out of bounds
C-   Controls: 
C-
C-   Created  16-MAY-1990   Rajendran Raja
C-   Inverse of CALETA routine
C-   Updated  11-DEC-1992 W.Dharmaratna, inclease the last eta bin 
C-                        to include eta <5.7  because some of the
C_                        particles above 4.98 has GCAH and CAEP banks. 
C-                        5.7 is the average value of the boundary where 
C_                        the calorimeter has energy due to GEANT 3.14 data.
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    ETA,ETAA,SIGN,SGN
      INTEGER IETA,IERR
C----------------------------------------------------------------------
      IERR = 0
      SGN = SIGN(1.0,ETA)
      ETAA = ABS(ETA)
      IF(ETAA.LE.3.2)THEN
        IETA = (ETAA+0.1)*10.0
      ELSEIF(ETAA.GT.3.2.AND.ETAA.LE.3.42)THEN
        IETA = 33
      ELSEIF ( ETAA.GT.3.42.AND.ETAA.LE.3.7 ) THEN
        IETA = 34
      ELSEIF ( ETAA.GT.3.7.AND.ETAA.LE.4.1 ) THEN
        IETA = 35
      ELSEIF ( ETAA.GT.4.1.AND.ETAA.LE.4.45 ) THEN
        IETA = 36
      ELSEIF ( ETAA.GT.4.45.AND.ETAA.LE.5.7 ) THEN
        IETA = 37
      ELSE
        IERR = 1
        IETA = 999
      ENDIF
      IETA = IETA*SGN
  999 RETURN
      END
