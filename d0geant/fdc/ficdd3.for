      SUBROUTINE FICDD3(IADADC, TIME, AREA, NHITS, NWDAT)
C-----------------------------------------------------------------------
C
C   Purpose and Methods : Converts hits to digitization in the FADC
C                         and performs the Zero-suppression, then fills
C                         the bank CDD3
C
C   Inputs  :   IADADC(6) = half, theta-quad/phi-0, sector, fadc, ident
C               TIME      = drift time (ns)
C               AREA      = pulse area
C               NHITS     = total number of hits
C   Outputs :   NWDAT     = Total number of data written in CDD3
C
C-   Created   9-MAR-1987   Ghita Rahal-Callot
C-   Updated   3-OCT-1988   Jeffrey Bantly  converted to FDC use
C-   Updated  06-NOV-1989   Peter Grudberg - set IARRAY to 256, call ZADPED
C
C------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:FDCLNK.INC/LIST'
      REAL TIME(*), AREA(*), AMP, FLUCT, AMPED, FLPED,
     +      VALPED
      INTEGER IARRAY, NWDAT
C
C ****  IARRAY = Length of the FADC
C
      PARAMETER ( IARRAY = 256 )
      REAL PROVE(0:IARRAY-1), TMAX
      INTEGER IADADC(6), IPROVE( 0:IARRAY-1 )
      INTEGER NCLUS, ILEAD( 64 ), ITRAIL( 64 )
      INTEGER NHITS, ITYPE, I, DIGFLG, MAXCNT, ISTART
      INTEGER LCFPSE,LCFPDL,GZFPSE
      DATA MAXCNT /255/
      REAL TSEEN
      DATA TMAX / 2600. /
C-----------------------------------------------------------------------
C
      IF ( NHITS.LE.0 ) GO TO 999
C
      CALL VZERO ( PROVE(0), IARRAY )
      CALL VZERO ( IPROVE(0), IARRAY )
C
C ****  ITYPE to distinguish between sense wire and delay line
C
      ITYPE = 0
      IF ( IADADC(5) .GE. 8 .AND. IADADC(2) .LE. 0 ) ITYPE = 1
C
C ****  Pulse simulation
C
      DIGFLG = 0
      DO 100 I =1 ,NHITS
        TSEEN = TIME (I)
        IF(TSEEN . GE. TMAX .OR. AREA(I) .LE. 0.0) GOTO 100
        CALL SIMPUL(2,TSEEN,AREA(I),ITYPE,PROVE,DIGFLG)
  100 CONTINUE
C
C ****  DIGFLG = 0 No digitization has been performed
C
      IF ( DIGFLG .EQ. 0 ) GOTO 999
C
C ****  FADC response to the charge
C         SFDC(4) = 0. no bilinear conversion;
C         SFDC(4) = 1. do bilinear conversion;
C
      IF ( SFDC(4) .NE. 0. ) THEN
        CALL ZCNVRT ( PROVE, IARRAY )
      ENDIF
C
C ****  Addition of pedestal value   GET FROM THETA OR PHI
C
        LCFPSE = GZFPSE(IADADC(1),IADADC(2),IADADC(3),IADADC(4))
        IF ( LCFPSE .LE. 0 ) THEN
          CALL ERRMSG('Bank not defined','FICDD3',
     &                'Pedestal bank FPSE not defined','F')
        ENDIF
        LCFPDL = LCFPSE + 6 + IADADC(5) * IC(LCFPSE+4)
      AMPED = C ( LCFPDL + 1 )
      FLPED = C ( LCFPDL + 2 )
C
C ****  Temporarily make them Amped = 20. and fluctuation = 1.5
C
      AMPED = 20.
      FLPED = 1.5
C
C ****  Add pedestal
C
      CALL ZADPED( PROVE, IARRAY, AMPED, FLPED, IPROVE )
C
C ****  Zero suppression simulation
C         SFDC(5) = 0. enable zero-suppression;
C         SFDC(5) > 0. disable zero-suppression;
C
      CALL VZERO ( ILEAD, 64 )
      CALL VZERO ( ITRAIL, 64 )
      CALL ZERSUP ( 2, IARRAY, IPROVE, NCLUS, ILEAD,ITRAIL )
C      DO 103 I = 1, NCLUS
C        IF ( ILEAD(I) .LT. 0 ) ILEAD(I) = 0
C  103 CONTINUE
C
C ****  Pack and store data in ZEBRA bank
C
      IF ( NCLUS .EQ. 0) GO TO 999
C
      CALL FDCWRT ( IADADC, IPROVE, NCLUS, ILEAD, ITRAIL, NWDAT )
C
C-------------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
