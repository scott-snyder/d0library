      SUBROUTINE FICDD1(IADADC, TIME, AREA, WIDTH, COORD, NHITS,
     * NWDAT)
C======================================================================
C
C   Purpose and Methods : Converts hits to digitization in the FADC
C                         and performs the Zero-suppression, then fills
C                         the bank CDD1
C
C   Inputs  :   IADADC(3) = layer, sector, wire*2+end
C               TIME      = drift time (ns)
C               AREA      = integrated pulse area
C               WIDTH     = track length in VTX cell
C               COORD     = drift distance
C               NHITS     = total number of hits
C   Outputs :   NWDAT     = Total number of data written in CDD1
C
C-   Created   9-MAR-1987   Ghita Rahal-Callot
C             30-APR-1987   G. Rahal : add FADC response to the charge
C             09-SEP-1987   G. Rahal : IPROVE initialization
C-   Updated  11-FEB-1988   Ghita Rahal-Callot: No time ordering (already
C-                          done ); Increase the width of the pulse in the
C-                          FADC, and add pedestals
C-   Updated  13-JUL-1988   Ghita Rahal-Callot: Clean up   
C    Modified  6-OCT-1988   Tom Trippe - for VTX
C-   Modified 18-NOV-1988   Peter Grudberg - added z-strips for VTX
C-   Updated  14-JUN-1989   Peter Grudberg   New pulse shape
C-   Updated  06-NOV-1989   Peter Grudberg- set IARRAY to 255, call ZADPED
C-   Updated  20-FEB-1991   Peter M. Grudberg Remove unneeded reference to t0
C-   Updated   5-MAY-1992   Alexandre Zinchenko - modified to simulate new
C-                          pulse shapes (2 additional input parameters, etc.)
C
C======================================================================
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:VTXPSH.INC/LIST'
C
      REAL TIME(*), AREA(*), AMP, FLUCT, AMPED, FLPED, VALPED 
      REAL WIDTH(*), COORD(*), PROVE1(2500) 
      INTEGER IARRAY, DIGFLG, NWDAT
C
C ****  IARRAY = Length of the FADC
C
      PARAMETER ( IARRAY = 256 )
      REAL PROVE(0:IARRAY-1)
      REAL TSEEN, TMAX
      INTEGER IADADC(3), IPROVE( 0:IARRAY-1 )
      INTEGER LVTMW, LVTMZ, LVPDL, LVPDZ
      INTEGER GZVTMW, GZVTMZ, GZVPDL, GZVPDZ
      INTEGER PLVTMW, PLVTMZ, PLVPDL, PLVPDZ
      INTEGER NCLUS, ILEAD( 64 ), ITRAIL( 64 )
      INTEGER NHITS, I, ITYPE, MXWLAY, MAXCNT, DTTYPE, J 
      PARAMETER (MXWLAY=2)              ! Max. wire layer number
      PARAMETER (DTTYPE=0)              ! Detector type
      DATA TMAX /2600.0/, MAXCNT/255/
C======================================================================
C
      IF ( NHITS.LE.0 ) GO TO 999
C
      CALL VZERO ( PROVE(0), IARRAY )
      CALL VZERO ( IPROVE(0), IARRAY )
      CALL VZERO ( PROVE1(1), 2500 ) 
C
C   ITYPE: 0 = wire layer, 1 = z-strip layer
C
      ITYPE = 0
      IF ( IADADC(1) .GT. MXWLAY ) ITYPE = 1
C
C       pulse simulation
C
      DIGFLG = 0
      DO 100 I =1 ,NHITS
        INHITS = I 
        TSEEN = TIME ( I )
        IF (TSEEN .GE. TMAX .OR. AREA(I) .LE. 0.0) GO TO 100
        IF(SVTX(6).EQ.1.) THEN 
          CALL VTXPUL(TSEEN,AREA(I),WIDTH(I),COORD(I),
     &                PROVE1,DIGFLG) 
        ELSE 
          CALL SIMPUL(DTTYPE, TSEEN, AREA(I), ITYPE, PROVE, DIGFLG)
        ENDIF 
  100 CONTINUE
C
C**** Increase time bin
C
      IF(SVTX(6).EQ.1.) THEN
        DO 10 I = 10, 2500, 10
          J = I/10.
          PROVE(J-1) = PROVE1(I)
10      CONTINUE
      ENDIF
C
C ****  DIGFLG = 0   No digitization has been performed
C
      IF ( DIGFLG .EQ. 0 ) THEN
        GO TO 999
      ENDIF
C
C **** FADC response to the charge
C        SVTX(4) = 0. no bilinear conversion
C        SVTX(4) = 1. do bilinear conversion
C
      IF (SVTX(4) .NE. 0.) THEN
        CALL ZCNVRT(PROVE, IARRAY)
      ENDIF
C
C ****  Addition of pedestal value
C
      IF ( ITYPE .EQ. 0 ) THEN     ! sense wire channels
        LVPDL = GZVPDL(IADADC(1))
        IF ( LVPDL .LE. 0 ) THEN
          WRITE(LOUT,*) ' **** FICDD1: Pedestal bank LVPDL not defined'
          CALL EXIT(1)
        ENDIF
        LVPDL = LVPDL + (IADADC(2)*IC(LVPDL+4)+IADADC(3))
     &                *  IC(LVPDL+3)+5
        AMPED = C ( LVPDL + 1 )
        FLPED = C ( LVPDL + 2 )
      ELSE                             ! z-strips
        LVPDZ = GZVPDZ(IADADC(1)-8)          ! z-strip channels
C                                            ! IADADC(1) = z-layer + 8
        IF ( LVPDZ .LE. 0 ) THEN
          WRITE (LOUT,*) ' **** FICDD1: Pedestal bank LVPDZ not defined'
          CALL EXIT(1)
        ENDIF
        LVPDZ = LVPDZ + (IADADC(3) * IC(LVPDZ+5) / 2 ) 
     &                * IC(LVPDZ+3)+5
        AMPED = C ( LVPDZ + 1 )
        FLPED = C ( LVPDZ + 2 )
      ENDIF
C
C ****  Add pedestal
C
      CALL ZADPED( PROVE, IARRAY, AMPED, FLPED, IPROVE )
C
C ****  Zero suppression simulation
C
      CALL VZERO ( ILEAD, 64 )
      CALL VZERO ( ITRAIL, 64 )
      CALL ZERSUP ( DTTYPE, IARRAY, IPROVE, NCLUS, ILEAD, ITRAIL )
C
C ****  Pack and store data in ZEBRA bank
C
      IF ( NCLUS .EQ. 0 ) GO TO 999
C
      CALL VTXWRT ( IADADC, IPROVE, NCLUS, ILEAD, ITRAIL, NWDAT )
C
  999 RETURN
      END
