      SUBROUTINE FIDCDA ( DHITS, NDHITS, IFADC )
C======================================================================
C
C   Purpose and Methods : load hits from one sense wire into ZEBRA bank "DCDA".
C-                        The bank " DCDA " is booked if not already booked
C-                        earlier.
C-                        The number of hits in the sector bank " DCDA ",
C-                        layer bank " DLYR " and CDC chamber bank CDCH is
C-                        incremented IF not already done.
C
C   Inputs  : DHITS   =  Array containing the data to be stored in DCDA
C                        ( filled in DVECDA )
C-            NDHITS  =  number of hits on wire:  J = 1,NDHITS
C-            IFADC   =  layer, sector and wire number
C
C-    Created   3-FEB-1988   Ghita Rahal-Callot
C-   Updated  29-JAN-1990   Qizhong Li-Demarteau  fix the call to MZPUSH 
C-   Updated  24-FEB-1991   Qizhong Li-Demarteau  fix array size for DHITS 
C
C======================================================================
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:CDCLNK.INC/LIST'
      INTEGER NDHITS, IFADC(*), NWDSHT, NPTWHT
      INTEGER IPTR, IPTRHT,IHIT,NHTS,LBASE,IWDHT,LCDCDA,LKCLYR,LKCDCH
      INTEGER  I, NPULS
      INTEGER NHH, NHMAX, NWORDS, NFULL
      PARAMETER ( NHH = 18, NHMAX = 50 )
      REAL DHITS ( NHH, 2*NHMAX)
C======================================================================
C
C ****  Get the link LCDCDA for sector
C
      LCDCDA = LDCDA ( IFADC(2), IFADC(1) )
C
C ****  Book the bank if needed
C
      NPULS = 100
      IF (LCDCDA .EQ. 0) CALL BKDCDA (IFADC(1), IFADC(2), NPULS, LCDCDA)
      IPTR = 4 + IFADC(3)
      NHTS = IQ (LCDCDA + IPTR)
      NWDSHT = IQ ( LCDCDA + 3 )
      NPTWHT = IQ ( LCDCDA + 2 )
C
C ****  Check if data is not already present
C
      IF (NHTS .NE. 0) THEN
        WRITE(LOUT,110) (IFADC(I), I=1,3)
        GO TO 999
      ENDIF
C
C ****   Load number of hits for the wire
C
      IQ(LCDCDA+IPTR)=NDHITS
C
C ****   Load pointer to first hit
C
      IPTRHT = 4 + NPTWHT * 2 + NWDSHT * IQ(LCDCDA+1) - 1
      IQ (LCDCDA + IPTR + NPTWHT) = IPTRHT
C
C ****  Increment # hits in the sector
C
      IQ (LCDCDA + 1) = IQ (LCDCDA + 1) + NDHITS
C
C ****  If the sectors banks DSEC have been really filled, the # of hits
C ****  in DLYR and CDCH have been already filled...don't do it twice
C
      IF ( SCDC(2) .NE. 1 ) THEN
C
C ****  Increment # hits in the layer
C
        LKCLYR = LQ (LCDCDA + 1)
        IQ(LKCLYR+1) = IQ (LKCLYR+1) + NDHITS
C
C ****  Increment # hits in the CDC chamber
C
        LKCDCH = LQ (LKCLYR + 1)
        IQ ( LKCDCH + 1) = IQ (LKCDCH + 1) + NDHITS
      ENDIF
C
C ****  Increase DCDA if needed, depending on the size of the cluster I
C ****  have to fill
C
        NWORDS = IQ ( LCDCDA - 1)
        NFULL  = IQ ( LCDCDA + 1) * NWDSHT + 2 * NPTWHT
        IF ( NWORDS - NFULL .LE. 0 ) THEN
          NPULS = MAX ( NDHITS * NWDSHT, 100 )
          CALL MZPUSH(IXCOM, LCDCDA, 0, NPULS, 'R')
          LCDCDA = LDCDA ( IFADC(2), IFADC(1) )
        ENDIF

C
C ****  Transfer hits to ZEBRA bank "DCDA".
C
      DO 200 IHIT = 1, NDHITS
        LBASE = LCDCDA + IPTRHT + NWDSHT*(IHIT-1)
        IQ(LBASE+1) = IQ ( LCDCDA - 5) + DHITS(1,IHIT)
        DO 201 IWDHT = 2, NWDSHT
          Q(LBASE+IWDHT) = DHITS(IWDHT,IHIT)
  201   CONTINUE
  200 CONTINUE
C
C
  999 CONTINUE
  110 FORMAT(10X, 'Error in subroutine FIDCDA : attempting overwrite',
     &  /,10X,'Layer',1X,I2,1X,'Sector',1X,I3,1X,'Cell',1X,I3)
      RETURN
      END
