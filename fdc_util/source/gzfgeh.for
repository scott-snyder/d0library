      FUNCTION GZFGEH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FGEH 
C-
C-   Returned value  : FGEH bank pointer
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  17-MAR-1989   Jeffrey Bantly
C-   Updated  15-JUL-1991   Susan K. Blessing  Remove path checking.
C-    We only use STPC.
C-   Updated  20-AUG-1992   Robert E. Avery  Put in call to FDC_MCCHECK 
C-      to get correct version of MC STP bank.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INCLUDE 'D0$LINKS:IZFGEH.LINK'
      INTEGER GZFGEH
      INTEGER GZSFDC, LKSFDC
      LOGICAL FGEAN_CHK
C----------------------------------------------------------------------
      IF(LFGEH.EQ.0) THEN               ! link not set
        IF (.NOT.FGEAN_CHK()) THEN
          CALL FDC_MCCHECK
        END IF
        LKSFDC=GZSFDC()
        IF ( LKSFDC .NE. 0 ) LFGEH=LC(LKSFDC-IZFGEH)
        GZFGEH=LFGEH
      ELSE                              ! link set
        GZFGEH=LFGEH
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
