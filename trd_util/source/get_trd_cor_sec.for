      SUBROUTINE GET_TRD_COR_SEC(PLANE,SECTOR,VERSION,CORRECTION,ERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : calculates sector correction
C-
C-   Inputs  : PLANE      integer   1,2,3 (anodes) or 4,5,6 (cathodes)
C-                                  (cathodes are not corrected)
C-             SECTOR     integer   in [1,16]
C-             VERSION    integer
C-   Outputs : CORRECTION real
C-             ERROR      integer   0 = OK
C-                                  1 = correction not required in TRD.RCP
C-                                  2 = wrong plane
C-                                  3 = wrong sector
C-                                  4 = version not found
C-   Controls: TRD.RCP
C-
C-   Created  15-JAN-1993   Alain PLUQUET
C-   Updated   2-MAR-1994   Alain PLUQUET   uses uranium sector corrections
C----------------------------------------------------------------------
      IMPLICIT NONE
C  VERSION 0 : all sector corrections = 1.
C  VERSION 1 :
C  The following relative sector gains have been obtained from
C  runs 43170,43085 and 43237. The normalization has been performed for each
C  layer individually.
C  Correction Laurent Chevalier from URANIUM run analysis
      REAL    CORSEC1(16,3)
      REAL    CORSEC2(16,3)
      DATA CORSEC1/0.96,0.96,0.87,1.00,1.00,0.90,1.02,1.06,
     + 1.12,1.03,1.16,1.07,0.70,1.05,1.18,1.05,
     + 1.00,1.00,1.07,1.02,1.08,0.99,1.01,1.01,
     + 1.03,0.93,1.06,1.01,0.97,0.86,1.02,0.99,
     + 1.05,0.99,0.99,0.96,1.04,0.93,1.02,1.06,
     + 1.07,0.97,1.04,1.04,0.97,0.95,1.04,0.97/
      DATA CORSEC2/1.068,1.034,1.011,0.947,0.992,1.032,1.062,1.062,
     & 1.023,0.959,0.949,0.954,0.975,0.914,1.020,0.998,
     & 0.978,0.986,1.044,1.019,1.007,0.982,0.955,1.024,
     & 0.977,0.993,0.993,1.016,1.036,0.999,1.001,0.991,
     & 0.985,0.964,0.966,0.967,1.017,0.998,1.029,1.095,
     & 1.043,0.959,0.973,0.992,1.064,0.986,0.997,0.965/
C----------------------------------------------------------------------
      INTEGER PLANE,SECTOR,ERROR,IER,VERSION
      REAL CORRECTION
      LOGICAL FIRST,DO_CORRECTION
      DATA FIRST/.TRUE./
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('TRD_RCP')
        CALL EZGET('COR_SEC',DO_CORRECTION,IER)
        CALL EZRSET
      ENDIF

      IF(DO_CORRECTION) THEN
        IF (PLANE.GE.1.AND.PLANE.LE.3) THEN
          IF (SECTOR.GE.1.AND.SECTOR.LE.16) THEN
            IF (VERSION.EQ.0) THEN
              CORRECTION=1.
              ERROR=0
            ELSE IF (VERSION.EQ.1) THEN
              CORRECTION=CORSEC1(SECTOR,PLANE)
              ERROR=0
            ELSE IF (VERSION.EQ.2) THEN
              CORRECTION=1./CORSEC2(SECTOR,PLANE)
              ERROR=0
            ELSE
              CORRECTION=1.
              ERROR=4
            ENDIF
          ELSE
            CORRECTION=1.
            ERROR=3
          ENDIF
        ELSE
          CORRECTION=1.
          ERROR=2
        ENDIF
      ELSE
        CORRECTION=1.
        ERROR=1
      ENDIF
      END
