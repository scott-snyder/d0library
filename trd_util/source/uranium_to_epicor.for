       SUBROUTINE URANIUM_TO_EPICOR
     &  (URANIUM,PEAK_MB_DATA,PEAK_MB_REFERENCE,EPICOR)
C----------------------------------------------------------------------
C-   Purpose and Methods : computes calibration factor EPICOR
C-                         (relative calibration from URANIUM peak positions)
C-                         (absolute calibration from MB peak positions)
C-   Inputs  :  URANIUM           real(3) uranium peak positions (FADC counts)
C-              PEAK_MB_DATA      real    MB peak position (FADC counts) 
C-              PEAK_MB_REFERENCE real    MB peak position (MIP)
C-   Outputs :  EPICOR            real(3) EPICOR calibration factor
C-   Controls: none
C-   Created  28-FEB-1994   Alain PLUQUET
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL URANIUM(3),PEAK_MB_DATA,PEAK_MB_REFERENCE,EPICOR(3)
      EPICOR(1)=URANIUM(1)/URANIUM(2)*PEAK_MB_DATA/PEAK_MB_REFERENCE
      EPICOR(2)=URANIUM(2)/URANIUM(2)*PEAK_MB_DATA/PEAK_MB_REFERENCE
      EPICOR(3)=URANIUM(3)/URANIUM(2)*PEAK_MB_DATA/PEAK_MB_REFERENCE
      END
