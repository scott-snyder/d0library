      SUBROUTINE MUREFIT_CLEAN(IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : cleanup the extra banks left
C-                         over from refitting
C-
C-   Inputs  : none
C-   Outputs : IERR = error code, 0=success
C-   Controls: 
C-
C-   Created  12-JAN-1994   Darien R. Wood
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IERR
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMRFT.LINK'
      INTEGER LMUON,NLINKS,ILINK,LMRFT
      INTEGER  GZMUON
      EXTERNAL GZMUON
C----------------------------------------------------------------------
      IERR = 0
C loop over muon banks
      LMUON = GZMUON(0)
      DO WHILE(LMUON.GT.0)
        LMRFT = LQ(LMUON-IZMRFT)
        IF(LMRFT.GT.0) THEN
          CALL MZDROP(IXMAIN,LMRFT,' ')
        ENDIF  
        LMUON = LQ(LMUON)
      ENDDO
  999 RETURN
      END
