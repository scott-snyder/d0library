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
C-   Modified 16-oct-1995   DRW: fixed for new MTRH location
C-   Updated  27-NOV-1995   Andrei Mayorov  drop SAMUS working banks 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IERR
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMRFT.LINK'
      INTEGER LMTRH,NLINKS,LMRFT,LSAMT_OLD
      INTEGER  GZMTRH
      EXTERNAL GZMTRH
C----------------------------------------------------------------------
      IERR = 0
      LMTRH = GZMTRH(0)
      IF(LMTRH.GT.0) THEN
        NLINKS = IQ(LMTRH-3)
        IF(NLINKS.GE.4) THEN
          LMRFT = LQ(LMTRH-IZMRFT)
          IF(LMRFT.GT.0) THEN
C drop whole linear chain of MRFT banks
            CALL MZDROP(IXMAIN,LMRFT,'L')
          ENDIF
          IF(NLINKS.GE.5) THEN
            LSAMT_OLD = LQ(LMTRH-5)
            IF(LSAMT_OLD.GT.0) THEN
C drop old SAMT bank
              CALL MZDROP(IXMAIN,LSAMT_OLD,' ')
            ENDIF
          ENDIF
        ENDIF  
      ENDIF
      call sadrop
C
  999 RETURN
      END
