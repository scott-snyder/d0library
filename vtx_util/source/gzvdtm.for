      INTEGER FUNCTION GZVDTM(LAYER,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to bank VDTM -- contains
C-               a layer's worth of distance time maps
C-
C-   Returned value  : address of VDTM bank - return 0 if nonexistant
C-   Inputs  : LAYER,SECTOR
C-   Outputs : 
C-   Controls: 
C-
C-   Created  22-JUN-1990   ED OLTMAN
C-   Updated  18-OCT-1990   ED OLTMAN  Added SECTOR dependance to DTM map 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVTMW.LINK'
      INCLUDE 'D0$LINKS:IZVDTM.LINK'
      INTEGER  LAYER,SECTOR
      INTEGER  LVTMW,CATEG,OFFSET
      INTEGER  ITEMS, NWIRE
      DATA NWIRE / 8 / 
C----------------------------------------------------------------------
      GZVDTM = 0
      IF (LVTMH .GT. 0) THEN
        LVTMW = LC( LVTMH - IZVTMW - LAYER) 
        IF (LVTMW .GT. 0) THEN
          ITEMS = IC(LVTMW + 3)
          OFFSET = LVTMW + 6 + NWIRE*ITEMS*IC(LVTMW+5)
          CATEG = IC(OFFSET + SECTOR)
          GZVDTM = LC( LVTMW - (IZVDTM+CATEG) )
        ENDIF
      ENDIF
  999 RETURN
      END
