      FUNCTION GZSFDC()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank SFDC
C-
C-   Returned value  :
C-   Inputs  : 
C-   Outputs :
C-   Controls:
C-
C-   Created  17-MAR-1989   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSFDC.LINK'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INTEGER GZSFDC
      INTEGER LKSTPC
C----------------------------------------------------------------------
      GZSFDC=0
      IF ( LSFDC .NE. 0 ) GZSFDC=LSFDC
      IF( LSFDC .EQ. 0 .AND. LSTPH.NE.0 ) THEN
        LKSTPC=LC(LSTPH-IZSTPC)
        IF ( LKSTPC .NE. 0 ) GZSFDC=LC(LKSTPC-IZSFDC)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
