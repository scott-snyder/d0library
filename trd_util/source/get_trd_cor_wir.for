      SUBROUTINE GET_TRD_COR_WIR(PLANE,WIRE,VERSION,CORRECTION,ERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : calculates wire correction
C-
C-   Inputs  : PLANE      integer   1,2,3 (anodes) or 4,5,6 (cathodes)
C-                                  (cathodes are not corrected)
C-             WIRE       integer   in [1,256]
C-             VERSION    integer
C-   Outputs : CORRECTION real
C-             ERROR      integer   0 = OK
C-                                  1 = correction not required in TRD.RCP
C-                                  2 = wrong plane
C-                                  3 = wrong wire
C-                                  4 = version not found
C-   Controls: TRD.RCP
C-
C-   Created  15-JAN-1993   Alain PLUQUET
C-
C- VERSION 0 : all wire corrections = 1
C----------------------------------------------------------------------
      IMPLICIT NONE
C       INCLUDE 'D0$INC:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.INC'
      INTEGER PLANE,WIRE,ERROR,IER,VERSION
      REAL CORRECTION
      LOGICAL FIRST,DO_CORRECTION
      DATA FIRST/.TRUE./
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('TRD_RCP')
        CALL EZGET('COR_WIR',DO_CORRECTION,IER)
        CALL EZRSET
      ENDIF

      IF(DO_CORRECTION) THEN
        IF (PLANE.GE.1.AND.PLANE.LE.3) THEN
          IF (WIRE.GE.1.AND.WIRE.LE.NWIRE_PER_LAYER(PLANE)) THEN
            IF (VERSION.EQ.0) THEN
              CORRECTION=1.
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
