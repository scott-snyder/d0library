      SUBROUTINE GET_TRD_COR_APC(PLANE,CORRECTION,ERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : calculates additive pedestal correction
C-
C-   Inputs  : PLANE      integer   1,2,3 (anodes) or 4,5,6 (cathodes)
C-   Outputs : CORRECTION real      additive pedestal correction in fadc counts
C-             ERROR      integer   0 = OK 
C-                                  1 = correction not required in TRD.RCP
C-   Controls: TRD.RCP
C-
C-   Created  15-JAN-1993   Alain PLUQUET
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TCNTRL.INC'
      INTEGER PLANE,ERROR,IER
      REAL CORRECTION
      LOGICAL FIRST,DO_CORRECTION
      DATA FIRST/.TRUE./
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('TRD_RCP')
        CALL EZGET('COR_APC',DO_CORRECTION,IER)
        CALL EZGET('APC',APC(1),IER)
        CALL EZRSET
      ENDIF
      IF (DO_CORRECTION) THEN
        IF (PLANE.LE.3) THEN
          CORRECTION=APC(PLANE)
        ELSE
          CORRECTION=APC(PLANE-3)
        ENDIF
        ERROR=0
      ELSE
        CORRECTION=0.
        ERROR=1
      ENDIF
      END
