      SUBROUTINE CDCWRT ( IFADC , IPROVE, NCLUS, ILEAD, ITRAIL, NWDAT )
C======================================================================
C
C   Purpose and Methods :  Fills the CDD2 Zebra bank with the digitized
C                          data according to the raw data FADC structure.
C
C   Inputs  :   IFADC (3) : layer, sector and sense wire number
C               IPROVE (IARRAY) : Time slices data for one FADC
C               NCLUS           : Number of clusters
C               ILEAD           : Leading edge of each cluster
C               ITRAIL          : Trailing edge of each cluster
C               NWDAT           : Total # of data written in CDD2
C
C   Outputs :   NONE
C
C-   Created  18-MAR-1987   Ghita Rahal-Callot
C            09-SEP-1987   INCRE corrected
C-   Updated  11-FEB-1988   Ghita Rahal-Callot : Increase CDD2 at each cluster
C-                                               if needed
C-   Updated  26-OCT-1989   Jeffrey Bantly       add Crate Header words
C-   Updated  27-OCT-1989   Qizhong Li-Demarteau adjust CDD2 data format
C-   Updated  29-JAN-1990   Qizhong Li-Demarteau  fix the call to MZPUSH 
C
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:CDCLNK.INC'
C
      INTEGER IFADC(*), IPROVE(0:*), ILEAD(*), ITRAIL(*), NCLUS
      INTEGER INCRE, IDAT(5), NBIT(5), IOUT, I, J,
     &        LENTOT, NTOTAL
      INTEGER NWORDS, NWDAT, IVER, ISKP, IFL
C======================================================================
C
C
C ****  Create the bank if it doesn't exist
C
      IF( LCDD2 .LE. 0) THEN
        NWDAT = 0
        CALL BKCDD2
        IVER = 2                        ! Version 2
        ISKP = 0                        ! Header is desired, offset =0
        IFL  = 1                        ! Header is desired
        CALL CDD2FL(IVER,ISKP,IFL)      ! Add in Crate Header
        NWDAT = 6
      ENDIF
      IF ( LCDD2  .EQ. 0 ) GO TO 999
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
C ****  Increase CDD2 if needed, depending on the size of the cluster I
C ****  have to fill
C
        NWORDS = IQ ( LCDD2 - 1)
        I = (ITRAIL(J) - ILEAD(J))/4 + 3
        IF ( NWORDS-INCRE-I .LE. 0 ) THEN
          I = MAX ( I, 1000 )
          CALL MZPUSH(IXCOM, LCDD2, 0, I, 'R')
          NWORDS = IQ ( LCDD2 - 1 )
        ENDIF
        DO 101 I = ILEAD (J), ITRAIL (J), 4
          CALL VFILL ( NBIT, 4, 8 )
          IDAT (1) = IPROVE ( I+3 )
          IDAT (2) = IPROVE ( I+2 )
          IDAT (3) = IPROVE ( I+1 )
          IDAT (4) = IPROVE ( I )
          CALL DATAPK( 4, IDAT, NBIT, IOUT)
          INCRE = INCRE + 1
          IQ (LCDD2 + INCRE) = IOUT
  101   CONTINUE
C
C ****   The following word contains :
C ****         - # of data bytes + 4 ( # of adress bytes)
C ****         - The adress of the last time-slice used
C
        NTOTAL = ITRAIL (J) - ILEAD (J) + 1
        IDAT(1) = NTOTAL + 4
        IDAT(2) = ITRAIL (J)
        CALL VFILL ( NBIT, 2, 16 )
        CALL DATAPK (2, IDAT, NBIT, IOUT)
        INCRE = INCRE + 1
        IQ ( LCDD2 + INCRE) = IOUT
C
C ****  LENTOT contains the total # of bytes written
C
        LENTOT = LENTOT + NTOTAL + 4
  100 CONTINUE

C
C ****  Total data length and FADC adress storage
C
C
C ****  LENGTH  =   On 16 bits
C
      IDAT(1) = LENTOT + 4
      NBIT(1) = 16
C
C ****   Adress =  CELL + SECTOR * 2**4 + LAYER * 2**9
C ****             + ITYPE * 2 **13         (for CDC: ITYPE = 1)
C
      IDAT(2) = IFADC(3)
      NBIT(2) = 4
C
      IDAT(3) = IFADC(2)
      NBIT(3) = 5
C
      IDAT(4) = IFADC(1)
      NBIT(4) = 4
C
      IDAT(5) = 1
      NBIT(5) = 3
C
      CALL DATAPK (5, IDAT, NBIT, IOUT)
      INCRE = INCRE + 1
      IQ ( LCDD2 + INCRE ) = IOUT
      NWDAT = INCRE
C
C
  999 RETURN
      END
