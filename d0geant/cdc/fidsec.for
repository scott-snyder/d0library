      SUBROUTINE FIDSEC ( DHITS, NDHITS, IFADC )
C======================================================================
C
C   Purpose and Methods : load hits from one sense wire into ZEBRA bank "DSEC".
C-                        The bank " DSEC " is booked if not already booked
C-                        earlier.
C-                        The number of hits in the sector bank " DSEC ",
C-                        layer bank " DLYR " and CDC chamber bank CDCH is 
C-                        incremented.
C
C   Inputs  :
C-
C          IDHITS(1)   B   WIRE number
C           DHITS(2)   F   Position of the +phi solution in the cell frame (cm)
C           DHITS(3)   F   Position of the -phi solution in the cell frame (cm)
C           DHITS(4)   F   Z position ( cm )
C           DHITS(5)   F   Error on the position ( cm )
C           DHITS(6)   F   Error on Z position
C           DHITS(7)   F   Ionisation of the hit in M.I.P. units
C           DHITS(8)   F   Error on the previous value
C          IDHITS(9)   B   STATUS word  ( not filled )
C          IDHITS(10)  I   Track number
C-
C-      NDHITS       =  number of hits on wire:  J = 1,NDHITS
C-      IFADC        =  layer, sector and wire number
C
C-   Created   4-JAN-1987   T. Trippe  
C             15-APR-1987   G. Rahal   adapted to CDC
C-   Updated  11-FEB-1988   Ghita Rahal-Callot: Adapted to the new format
C-                          of DSEC   
C-   Updated  29-JAN-1990   Qizhong Li-Demarteau   fix the call to MZPUSH
C     
C
C======================================================================
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:CDCLNK.INC/LIST'
C
      INTEGER NDHITS, IFADC(*), NWDSHT, NPTWHT
      INTEGER IPTR, IPTRHT,IHIT,NHTS,LBASE,IWDHT,LCDSEC,LKCLYR,LKCDCH
      INTEGER LEBANK , I, NFULL, NWORDS
      INTEGER NHH, NHMAX
      PARAMETER ( NHH = 18, NHMAX = 50 )
      REAL DHITS ( NHH, NHMAX)
C======================================================================
C
C ****  Get the link LCDSEC for sector
C
      LCDSEC = LDSEC ( IFADC(2), IFADC(1) )
C
C ****  Book the bank if needed
C
      IF (LCDSEC .EQ. 0) THEN
        LEBANK = 100
        CALL BKDSEC (IFADC(1), IFADC(2), LEBANK, LCDSEC)    
      ENDIF
      IPTR = 4 + IFADC(3)
      NHTS = IQ (LCDSEC + IPTR)
      NWDSHT = IQ ( LCDSEC + 3 )
      NPTWHT = IQ ( LCDSEC + 2 )
C
C ****  Check if data is not already present
C
      IF (NHTS .NE. 0) THEN      
        WRITE(LOUT,110) IFADC(1), IFADC(2)
        GO TO 999
      ENDIF
C
C ****   Load number of hits for the wire
C
      IQ(LCDSEC+IPTR)=NDHITS        
C
C ****   Load pointer to first hit
C
      IPTRHT = 4 + NPTWHT * 2 + NWDSHT * IQ(LCDSEC+1) - 1   
      IQ (LCDSEC + IPTR + NPTWHT) = IPTRHT         
C
C ****  Increment # hits in the sector
C
      IQ (LCDSEC + 1) = IQ (LCDSEC + 1) + NDHITS       
C
C ****  Increment # hits in the layer
C
      LKCLYR = LQ (LCDSEC + 1)
      IQ(LKCLYR+1) = IQ (LKCLYR+1) + NDHITS     
C
C ****  Increment # hits in the CDC chamber
C
      LKCDCH = LQ (LKCLYR + 1)
      IQ ( LKCDCH + 1) = IQ (LKCDCH + 1) + NDHITS       
C
C
C ****  Increase DSEC if needed, depending on the size of the cluster I
C ****  have to fill
C
        NWORDS = IQ ( LCDSEC - 1)
        NFULL  = IQ ( LCDSEC + 1) * NWDSHT + 2 * NPTWHT
        IF ( NWORDS - NFULL .LE. 0 ) THEN
          LEBANK = MAX ( NDHITS * NWDSHT, 100 )
          CALL MZPUSH(IXCOM, LCDSEC, 0, LEBANK, 'R')
          LCDSEC = LDSEC ( IFADC(2), IFADC(1) )
        ENDIF

C ****  Transfer hits to ZEBRA bank "DSEC".
C
      DO 200 IHIT = 1, NDHITS
        LBASE = LCDSEC + IPTRHT + NWDSHT*(IHIT-1)
        CALL UCOPY( DHITS(1,IHIT), Q(LBASE+1), NWDSHT )
        IQ(LBASE+1) = IQ ( LCDSEC - 5) + IQ(LBASE+1)
  200 CONTINUE
C
C
  999 CONTINUE
  110 FORMAT(10X, 'Error in subroutine FIDSEC : attempting overwrite',
     &  /,10X,'Layer',1X,I2,1X,'Sector',1X,I3,1X,'Cell',1X,I3)
      RETURN
      END
