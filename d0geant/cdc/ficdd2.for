      SUBROUTINE FICDD2(IADADC, TIME, AREA, NHITS, NWDAT)
C======================================================================
C
C   Purpose and Methods : Converts hits to digitization in the FADC
C                         and performs the Zero-suppression, then fills
C                         the bank CDD2
C
C   Inputs  :   IADADC(3) = layer, sector, cell
C               TIME      = drift time (ns)
C               AREA      = pulse area
C               NHITS     = total number of hits
C   Outputs :   NWDAT     = Total number of data written in CDD2
C
C-   Created   9-MAR-1987   Ghita Rahal-Callot
C             30-APR-1987   G. Rahal : add FADC response to the charge
C             09-SEP-1987   G. Rahal : IPROVE initialization
C-   Updated  11-FEB-1988   Ghita Rahal-Callot: No time ordering (already
C-                          done ); Increase the width of the pulse in the
C-                          FADC, and add pedestals
C-   Updated  13-JUL-1988   Ghita Rahal-Callot: Clean up   
C-   Updated  25-OCT-1988   Ghita Rahal-Callot   : correct XMIN 
C-   Updated  31-MAR-1989   Qizhong Li-Demarteau   new pulse shape 
C-   Updated  06-NOV-1989   Peter Grudberg - set IARRAY to 256, call ZADPED
C-   Updated  24-FEB-1991   Qizhong Li-Demarteau   fix array size
C
C======================================================================
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:CDPARA.INC/LIST'
      INCLUDE 'D0$LINKS:IZDTMW.LINK'
      INCLUDE 'D0$LINKS:IZDTMD.LINK'
      INCLUDE 'D0$LINKS:IZDPDL.LINK'
C
      REAL FADCSW
      REAL FADCDL
      REAL TIME(*), AREA(*), FLUCT, AMPED, FLPED, VALPED 
      INTEGER IARRAY, DIGFLG, NWDAT
C
C ****  IARRAY = Length of the FADC
C
      PARAMETER ( IARRAY = 256 )
      REAL PROVE(0:IARRAY-1)
      REAL TSEEN, TMAX
      INTEGER IADADC(3), IPROVE( 0:IARRAY-1 )
      INTEGER LDPDL, PLDPDL, NHMAX
      PARAMETER( NHMAX = 100 )
      INTEGER NCLUS, ILEAD( NHMAX ), ITRAIL( NHMAX )
      INTEGER NHITS, I, ITYPE, MAXCNT
      DATA  TMAX/2600.0/, MAXCNT/255/
C======================================================================
C
      IF ( NHITS.LE.0 ) GO TO 999
C
      CALL VZERO ( PROVE(0), IARRAY)
      CALL VZERO ( IPROVE(0), IARRAY)
C
C   ITYPE to distinguish between sense wire and delay line 
C   (0: SW, 1:DL)
C
      ITYPE = 0
      IF ( IADADC(3) .GT. MXSENS ) ITYPE = 1
C
C   pulse simulation
C
      DIGFLG = 0
      DO 100 I =1 ,NHITS
        TSEEN = TIME(I)
        IF (TSEEN .GE. TMAX .OR. AREA(I) .LE. 0.0) GOTO 100
        CALL SIMPUL(1, TSEEN, AREA(I), ITYPE, PROVE, DIGFLG)
  100 CONTINUE
C
C ****  DIGFLG = 0   No digitization has been performed
C
      IF ( DIGFLG .EQ. 0 ) THEN
        GO TO 999
      ENDIF
C
C ****  FADC response to the charge
C         SCDC(4) = 0. no biliner conversion;
C         SCDC(4) = 1. do biliner conversion;
C
      IF (SCDC(4) .NE. 0) THEN
        CALL ZCNVRT(PROVE, IARRAY)
      ENDIF
C
C ****  Addition of pedestal value
C
      LDPDL = LC ( LDPDH - IADADC(1) - IZDPDL)
      IF ( LDPDL .LE. 0 ) THEN
        WRITE(LOUT,*) '***** FICDD2 : Pedestal bank LDPDL not defined'
        CALL EXIT(1)
      ENDIF
      PLDPDL = LDPDL + (IADADC(2)*IC(LDPDL+4)+IADADC(3))
     &              *  IC(LDPDL+3)+4
      AMPED = C ( PLDPDL + 1 )
      FLPED = C ( PLDPDL + 2 )
C
C ****  Add pedestal
C
      CALL ZADPED( PROVE, IARRAY, AMPED, FLPED, IPROVE )
C
C ****  Zero suppression simulation
C
      CALL VZERO(ILEAD, NHMAX)
      CALL VZERO(ITRAIL, NHMAX)
      CALL ZERSUP(1, IARRAY, IPROVE, NCLUS, ILEAD, ITRAIL)
C
C ****  Pack and store data in ZEBRA bank
C
      IF ( NCLUS .EQ. 0) THEN
        GO TO 999
      END IF
C
      CALL CDCWRT ( IADADC, IPROVE, NCLUS, ILEAD, ITRAIL, NWDAT )
  999 RETURN
      END
