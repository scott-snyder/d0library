      SUBROUTINE VTXWRT ( IFADC , IPROVE, NCLUS, ILEAD, ITRAIL, NWDAT )
C======================================================================
C
C   Purpose and Methods :  Fills the CDD1 Zebra bank with the digitized
C                          data according to the raw data FADC structure.
C                          See the note:
C                              D0 NOTE #500
C                              D. DeGroot
C
C   Inputs  :   IFADC (3) : layer, sector and sense wire number
C               IPROVE (IARRAY) : Time slices data for one FADC
C               NCLUS           : Number of clusters
C               ILEAD           : Leading edge of each cluster
C               ITRAIL          : Trailing edge of each cluster
C               NWDAT           : Total # of data written in CDD1
C
C   Outputs :   NONE
C
C-   Created  18-MAR-1987   Ghita Rahal-Callot (CDCWRT for CDC)
C            09-SEP-1987   INCRE corrected
C-   Updated  11-FEB-1988   Ghita Rahal-Callot : Increase CDD2 at each cluster
C-                                               if needed
C    Modified  5-OCT-1988   Tom Trippe (modified for VTX, CDD1)
C    Modified 21-NOV-1988   Peter Grudberg - added z-strip address
C    Updated  29-JUN-1989   P.G. - updated comments for channel address
C                                  about detector type and active flag.
C-   Updated  26-OCT-1989   Jeffrey Bantly   add Crate Header words
C-   Updated  26-OCT-1989   P.G. - change to version 2 format (length words)
C-   Updated  30-JAN-1990   P.G. - IXMAIN => IXCOM in MZPUSH call
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:VTXLNK.INC'
C
      INTEGER IFADC(*), IPROVE(0:*), ILEAD(*), ITRAIL(*), NCLUS
      INTEGER INCRE, IDAT(4), NBIT(4), IOUT, I, J,
     &        LENTOT, NTOTAL
      INTEGER NWORDS, NWDAT, IVER, ISKP, IFL
C======================================================================
C
C
C ****  Create the bank if it doesn't exist
C
      IF( LCDD1 .LE. 0) THEN
        NWDAT = 0
        CALL BKCDD1
        IVER = 2                        ! Version 2
        ISKP = 0                        ! Header is desired, offset =0
        IFL  = 1                        ! Header is desired
        CALL CDD1FL(IVER,ISKP,IFL)      ! Add in Crate Header
        NWDAT = 6
      ENDIF
      IF ( LCDD1  .EQ. 0 ) GO TO 999
      INCRE = NWDAT
C
C
C ****  Data packing:
C             4 time-slice data are packed in a word of 32 bits wide.
C             The order is:
C             MSB  (i)  (i+1)  (i+2)  (i+3)  LSB
C
      LENTOT = 0
      DO 100 J = 1, NCLUS
C
C ****  Increase CDD1 if needed, depending on the size of the cluster I
C ****  have to fill
C
        NWORDS = IQ ( LCDD1 - 1)
        I = (ITRAIL(J) - ILEAD(J))/4 + 3
        IF ( NWORDS-INCRE-I .LE. 0 ) THEN
          I = MAX ( I, 1000 )
          CALL MZPUSH ( IXCOM, LCDD1, 0, I, 'R' )
          NWORDS = IQ ( LCDD1 - 1 )
        ENDIF
        DO 101 I = ILEAD (J), ITRAIL (J), 4
          CALL VFILL ( NBIT, 4, 8 )
          IDAT (1) = IPROVE ( I+3 )
          IDAT (2) = IPROVE ( I+2 )
          IDAT (3) = IPROVE ( I+1 )
          IDAT (4) = IPROVE ( I )
          CALL DATAPK( 4, IDAT, NBIT, IOUT)
          INCRE = INCRE + 1
          IQ (LCDD1 + INCRE) = IOUT
  101   CONTINUE
C
C ****   The following word contains :
C ****         - # of data bytes + 4 (includes address and byte count)
C ****         - The address of the last time-slice used
C
        NTOTAL = ITRAIL (J) - ILEAD (J) + 1
        IDAT(1) = NTOTAL + 4
        IDAT(2) = ITRAIL (J)
        CALL VFILL ( NBIT, 2, 16 )
        CALL DATAPK (2, IDAT, NBIT, IOUT)
        INCRE = INCRE + 1
        IQ ( LCDD1 + INCRE) = IOUT
C
C ****  LENTOT contains the total # of bytes written
C
        LENTOT = LENTOT + NTOTAL + 4
  100 CONTINUE

C
C ****  Total data length and FADC address storage
C
C
C ****  LENGTH  =   16 bits (include channel trailer word)
C
      IDAT(1) = LENTOT + 4
      NBIT(1) = 16
C
      IF ( IFADC(1) .LE. 2 ) THEN
C
C ****   WIRES:
C ****   Address =   WIRE-END, SECTOR, LAYER
C ****                 4 bits, 5 bits, 7 bits
C
        IDAT(2) = IFADC(3)
        NBIT(2) = 4
C
        IDAT(3) = IFADC(2)
        NBIT(3) = 5
C
        IDAT(4) = IFADC(1)
        NBIT(4) = 7
C
      ELSE
C
C ****   Z-STRIPS:
C ****   Address =   STRIP-END, Z-LAYER+8
C ****                9 bits,     7 bits
C
        IDAT(2) = IFADC(3)
        NBIT(2) = 9
C
        IDAT(3) = IFADC(1)
        NBIT(3) = 7
C
        IDAT(4) = 0
        NBIT(4) = 0
C
      ENDIF
C Pack IOUT with  layer, sector, wire-end, #bytes for wires
C in bits         28-25,  24-20,   19-16,   15-0,
C        or with  z-layer+8, strip-end, #bytes  for strips
C in bits         28-25,        24-16,   15-0
C where bit 0 is the lowest order (i.e. rightmost) bit.
C *** Changes: detector type and channel active flag in bits 29-31.
C Bits 29 and 30 are for detector type (VTX=0, so no need to fill explicitly)
C Bit 31 is a channel-active flag (0=active channel; again, no need to fill)
C
      CALL DATAPK ( 4, IDAT, NBIT, IOUT )
      INCRE = INCRE + 1
      IQ ( LCDD1 + INCRE ) = IOUT
      NWDAT = INCRE
C
C===========================================================================
  999 RETURN
      END
