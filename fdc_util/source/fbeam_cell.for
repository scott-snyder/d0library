      SUBROUTINE FBEAM_CELL(HALF,UNIT,QDRT,SCTR,X_BEAM,Y_BEAM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return position of nominal beam center with respect
C-   to local sector coordinates. (x in drift direction, y orthogonal).
C-
C-   Inputs  : HALF,UNIT,QDRT,SCTR
C-   Outputs : X_BEAM,Y_BEAM
C-
C-   Created   9-JUL-1990   Robert E. Avery
C-   Updated  26-APR-1991   Jeffrey Bantly  change to use FDC.RCP 
C-   Updated   1-JUL-1991   Robert E. Avery  Replace call to DRIDIR 
C-                                              with FDRIFTDIR.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C-   Inputs  
      INTEGER HALF,UNIT,QDRT,SCTR
C-   Outputs  
      REAL    X_BEAM,Y_BEAM
C-   Local:
      INTEGER IADD
      INTEGER IER
C
      REAL    CDRIFT
      REAL    SDRIFT
      REAL    XWIRE,YWIRE,ZWIRE
      REAL    X_C 
      REAL    Y_C 
      REAL    X_FDC
      REAL    Y_FDC
C
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
C---------------------------------------------------------------------
      IF (FIRST) THEN
        CALL EZPICK('FDC_RCP')
        CALL EZGET('X_FDC',X_FDC,IER)
        CALL EZGET('Y_FDC',Y_FDC,IER)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
C Get position of BEAM W.R.T sector center:
C
      CALL FDRIFTDIR(HALF,UNIT,QDRT,SCTR,0,SDRIFT,CDRIFT)
      CALL GTFALH(HALF,UNIT,QDRT,SCTR,0,
     &                        XWIRE,YWIRE,ZWIRE)
      X_C = - XWIRE - X_FDC
      Y_C = - YWIRE - Y_FDC
      X_BEAM =  X_C*CDRIFT + Y_C*SDRIFT
      Y_BEAM = -X_C*SDRIFT + Y_C*CDRIFT
C-----------------------------------------------------------------------
  999 RETURN
      END
