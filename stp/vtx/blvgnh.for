      SUBROUTINE BLVGNH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build the pedestal banks for the Vertex chamber
C-
C-   Inputs  : None, but ZEBRA structure up to SVTX should exist.
C-   Outputs :
C-
C-   Created  17-FEB-1988   Olivier Callot
C-   Updated  30-SEP-1988   Ghita Rahal-Callot  : adapted for VTX
C-   Updated  10-NOV-1988   Peter Grudberg      : added z-strip banks   
C-   Updated  19-MAR-1991   Peter M. Grudberg   use BKVGNH
C-   Updated   2-AUG-1992   Peter Grudberg      remove strips
C-   Updated  25-SEP-1992   Peter M. Grudberg   Change bank structure
C-   Updated   5-NOV-1993   Ed Oltman   NEW FORMAT FOR AREA VS DRIFT DISTANCE
C-   Updated  12-DEC-1993   Liang-Ping Chen assign values with proper type 
C-                          to words (LVGNL+6) to (LVGNL+15 ). Since this code  
C-                          fills STP for MC, these words may be by-passed 
C-                          in VTQDIV, V_CORRECT_AREA.  
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVRFT.LINK'
      INCLUDE 'D0$LINKS:IZVZST.LINK'
      INTEGER MPVGNL(5), LOWRUN, HIGRUN, NWFADC, NBFADC
      INTEGER MAXLAY, SECMAX, LAY, SEC, ADC, IP, LVGNL, LVRFT
      INTEGER MAXB(0:7, 0:2), WIRE, WORD
      REAL    RINPUT_MC, RPERCM, LENGTH(0:2) 
      PARAMETER( NWFADC = 1 )
      PARAMETER( NBFADC = 16)
      REAL    GAIN, AREA_CORR
      PARAMETER( GAIN = 1. )
      PARAMETER ( AREA_CORR = 1. )
      PARAMETER ( RINPUT_MC =400., RPERCM=18.0) ! from VTRAKS.RCP
      DATA LENGTH/ 48.3, 53.3, 58.4/            ! from VGEH bank
      DATA MAXB/11,13,15,17,19,20,22,24,        ! from D0$STP$VTX:areas_.dat
     &          13,14,15,16,17,18,18,19,
     &          22,23,24,25,26,27,28,28/
      DATA    MPVGNL / 4HVGNL, 0, 0, 0, 0 /
      DATA LOWRUN, HIGRUN / 0, 999999/
C----------------------------------------------------------------------
      CALL MZFORM( 'VGNL', '5I 2F 8I -F', MPVGNL(5) )
      IF ( LVGNH .EQ. 0 ) THEN
        CALL BKVGNH( LVGNH )
      ENDIF
C
C ****  Book the layer banks
C
      LVRFT = LC ( LVGEH - IZVRFT )
      MAXLAY = IC ( LVRFT + 1 )
      DO 10 LAY = 0, MAXLAY-1
        SECMAX = IC ( LVRFT + 2 + 7*LAY )
        MPVGNL(4) = 5 + 3*41 + SECMAX*NBFADC*NWFADC
        CALL MZLIFT( IDVSTP, LVGNL, LVGNH, -(LAY+1), MPVGNL, -1)
        IC( LVGNL-5 ) = LAY
        IC( LVGNL+1 ) = LOWRUN
        IC( LVGNL+2 ) = HIGRUN
        IC( LVGNL+3 ) = NWFADC
        IC( LVGNL+4 ) = NBFADC
        IC( LVGNL+5 ) = SECMAX
         C( LVGNL+6 ) = 20.           ! BINS/CM FOR AREA CORRECTION
         C( LVGNL+7 ) = 1. + RINPUT_MC/RPERCM/LENGTH(LAY)
        DO WIRE=0,7 
          IC( LVGNL+ 8+WIRE) = MAXB(WIRE,LAY)
        ENDDO
        DO WORD = 16, 3*41
          C(LVGNL+WORD) = AREA_CORR
        ENDDO
        DO 20 SEC = 0, SECMAX-1
          DO 30 ADC = 0, NBFADC-1
            IP = LVGNL + ( SEC*NBFADC + ADC ) * NWFADC + 5 + 3*41
            C( IP+1 ) = GAIN
   30     CONTINUE
   20   CONTINUE
   10 CONTINUE
C
  999 RETURN
      END
