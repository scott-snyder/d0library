      SUBROUTINE GET_TRD_COR_ANG(TRACK,CORRECTION,ERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : calculates angular correction
C-
C-   Inputs  : TRACK       integer  track number
C-   Outputs : CORRECTION  real
C-             ERROR       integer   0 = OK 
C-                                   1 = correction not required in TRD.RCP
C-                                   2 = |sin(theta)|>1
C-   Controls: TRD.RCP
C-
C-   Created  15-JAN-1993   Alain PLUQUET
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TRINTR.INC'
      INTEGER TRACK,ERROR,IER
      REAL CORRECTION
      LOGICAL FIRST,DO_CORRECTION
      DATA FIRST /.TRUE./
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('TRD_RCP')
        CALL EZGET('COR_ANG',DO_CORRECTION,IER)
        CALL EZRSET
      ENDIF
      IF (DO_CORRECTION) THEN
        IF (ABS(STHETA(TRACK)).LT.1.) THEN
          CORRECTION=ABS(STHETA(TRACK))
          ERROR=0
        ELSE
          CORRECTION=1.
          ERROR=2
        ENDIF
      ELSE
        CORRECTION=1.
        ERROR=1
      ENDIF
      END
