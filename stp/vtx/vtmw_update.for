      SUBROUTINE VTMW_UPDATE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Drop old version 0 VTMW banks from MC STPFILE and
C-                         book new version 1 VTMW banks in their place.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   2-AUG-1992   Peter M. Grudberg
C-   Updated  12-FEB-1993   Ed Oltman  apply adjustments for distance-time:
C-                          tzero tweek and SCALE
C-   Updated  12-DEC-1993   Liang-Ping Chen EZPICK('VTWSTP_RCP'), not 
C-                          EZPICK('VTWSTP'), per Herb Greenlee's mail    
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER IVERS, LAY, LVTMW, GZVTMW, IER, NSEC(0:2), SEC, WIR, ITEMS
      CHARACTER*60 T0_FILE
      REAL WSCALE(0:7,0:2),T0_ADJ(0:7,0:2)
      LOGICAL OK
      DATA NSEC/15,31,31/
C----------------------------------------------------------------------
C
      IVERS = 1
      DO LAY = 0, 2
        LVTMW = GZVTMW(LAY)
        IF ( LVTMW .GT. 0 ) CALL MZDROP(IXSTP,LVTMW,' ')
        CALL BKVTMW(LAY,IVERS,LVTMW)
      ENDDO
C
C ****  Now read in the tzeros from the ascii file
C
      CALL VTMW_FROM_ASCII(OK)
C
C ****  Finally, fill SCALE portion of bank and adjust t0s.
C
      CALL EZPICK('VTWSTP_RCP')
      CALL EZGET('WSCALE',WSCALE,IER)
      CALL EZGET('T0_ADJ',T0_ADJ,IER)
      CALL EZRSET
      DO LAY = 0,2
        LVTMW = GZVTMW(LAY)
        ITEMS = IC(LVTMW+3)
        DO SEC = 0,NSEC(LAY)
          DO WIR = 0,7
            C(LVTMW + ITEMS*(8*SEC+WIR) + 10) = WSCALE(WIR,LAY)
            C(LVTMW + ITEMS*(8*SEC+WIR) + 6)  = 
     &        C(LVTMW + ITEMS*(8*SEC+WIR) + 6) + T0_ADJ(WIR,LAY)
            C(LVTMW + ITEMS*(8*SEC+WIR) + 8)  = 
     &        C(LVTMW + ITEMS*(8*SEC+WIR) + 8) + T0_ADJ(WIR,LAY)
          ENDDO
        ENDDO
      ENDDO
C
  999 RETURN
      END
