      SUBROUTINE FDCWRT ( IFADC , IPROVE, NCLUS, ILEAD, ITRAIL, NWDAT )
C-------------------------------------------------------------------------
C
C   Purpose and Methods :  Fills the CDD3 Zebra bank with the digitized
C                          data according to the raw data FADC structure.
C                          See the note:
C                              D0 NOTE #500
C                              D. DeGroot
C
C   Inputs  :   IFADC (6)       : half, unit, quadrant/sector or sector,
C                                and sense wire number
C               IPROVE (IARRAY) : Time slices data for one FADC
C               NCLUS           : Number of clusters
C               ILEAD           : Leading edge of each cluster
C               ITRAIL          : Trailing edge of each cluster
C               NWDAT           : Total # of data written in CDD3
C
C   Outputs :   NONE
C
C-   Created  18-MAR-1987   Ghita Rahal-Callot
C            09-SEP-1987   INCRE corrected
C-   Updated  11-FEB-1988   Ghita Rahal-Callot : Increase CDD3 at each cluster
C-                                               if needed
C-   Updated   5-OCT-1988   Jeffrey Bantly : modified for use with the FDC
C-   Updated   7-AUG-1989   Jeffrey Bantly : modify to duplicate real data
C-   Updated  26-OCT-1989   Jeffrey Bantly : add in Crate Header words
C
C---------------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:FDCLNK.INC'
C
      INTEGER IFADC(6), IPROVE(0:*), ILEAD(64), ITRAIL(64), NCLUS
      INTEGER INCRE, IDAT(4), NBIT(4), IOUT, I, J,
     &        LENTOT, NTOTAL
      INTEGER NWORDS, NWDAT, LKCDD3, IVER, ISKP, IFL
C----------------------------------------------------------------------------
C
C ****  Create the bank if it doesn't exist
C
      IF( LCDD3 .LE. 0) THEN
        NWDAT = 0
        CALL BKCDD3(LKCDD3)
        IVER = 2                        ! Version 2
        ISKP = 0                        ! Header is desired, offset =0
        IFL  = 1                        ! Header is desired
        CALL CDD3FL(IVER,ISKP,IFL)      ! Add in Crate Header
        NWDAT = 6
      ENDIF
      IF ( LCDD3  .EQ. 0 ) GO TO 999
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
C ****  Increase CDD3 if needed, depending on the size of the cluster I
C ****  have to fill
C
        NWORDS = IQ ( LCDD3 - 1)
        I = (ITRAIL(J) - ILEAD(J))/4 + 3
        IF ( NWORDS-INCRE-I .LE. 0 ) THEN
          I = MAX ( I, 1000 )
          CALL MZPUSH ( IXCOM, LCDD3, 0, I, 'R' )
          NWORDS = IQ ( LCDD3 - 1 )
        ENDIF
        DO 101 I = ILEAD (J), ITRAIL (J), 4
          CALL VFILL ( NBIT, 4, 8 )
          IDAT (1) = IPROVE ( I+3 )
          IDAT (2) = IPROVE ( I+2 )
          IDAT (3) = IPROVE ( I+1 )
          IDAT (4) = IPROVE ( I )
          CALL DATAPK( 4, IDAT, NBIT, IOUT)
          INCRE = INCRE + 1
          IQ (LCDD3 + INCRE) = IOUT
  101   CONTINUE
C
C ****   The following word contains :
C ****         - # of data bytes + 2 ( # of address bytes)
C ****         - The address of the last time-slice used
C
        NTOTAL = ITRAIL (J) - ILEAD (J) + 1
        IDAT(1) = NTOTAL + 4
        IDAT(2) = ITRAIL (J)
        CALL VFILL ( NBIT, 2, 16 )
        CALL DATAPK (2, IDAT, NBIT, IOUT)
        INCRE = INCRE + 1
        IQ ( LCDD3 + INCRE) = IOUT
C
C ****  LENTOT contains the total # of bytes written
C
        LENTOT = LENTOT + NTOTAL + 4
  100 CONTINUE

C
C ****  Total data length and FADC address storage
C ****
C ****  LENGTH  =   On 16 bits
C
      IDAT(1) = LENTOT + 4
      NBIT(1) = 16
C
C ****   Address =   SECTOR,UNIT,HALF OR SECTOR/QUAD,UNIT,HALF,ETC
C ****               on 16 bits
C
      IDAT(2) = IFADC(6)
      NBIT(2) = 16
C
      CALL DATAPK ( 2, IDAT, NBIT, IOUT )
      INCRE = INCRE + 1
      IQ ( LCDD3 + INCRE ) = IOUT
      NWDAT = INCRE
C
C---------------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
