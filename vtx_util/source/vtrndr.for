      LOGICAL FUNCTION VTRNDR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : End of run routine for VTRAKS.
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  24-MAY-1989   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PRUNIT, IMETHO, INEFF, HITSEG, MINHIT
      INTEGER USUNIT, IPATH, IER
      CHARACTER*4 PATH, VPATH
      CHARACTER*13 METHOD(2)
      EQUIVALENCE (IPATH,VPATH)
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
      DATA METHOD /'link_and_tree','    road     '/
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('VPATH',IPATH,IER)
        CALL EZGET('METHOD',IMETHO,IER)
        CALL EZGET('INEFF',INEFF,IER)
        CALL EZGET('MINHIT',MINHIT,IER)
        CALL EZRSET
        HITSEG=8-INEFF
      ENDIF
      PRUNIT = USUNIT()
      WRITE (PRUNIT,101)
  101 FORMAT('1 VTX TRACKING')
      WRITE (PRUNIT,102)
  102 FORMAT(/'     METHOD     MIN_HITS/SEG  MIN_HITS/TRK   PATH')
      WRITE (PRUNIT,103) METHOD(IMETHO), HITSEG, MINHIT, VPATH
  103 FORMAT(1X,A13,6X,I2,12X,I2,9X,A4)
      VTRNDR = .TRUE.
  999 RETURN
      END
