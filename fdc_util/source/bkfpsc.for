      SUBROUTINE BKFPSC(HALF,SECTOR,NHITS,LKFPSC)
C-----------------------------------------------------------------------
C-
C-  Purpose and Methods : Books the bank "FPSC" for a specified
C-                        FDC Phi sector - Version 0
C-
C-  Inputs: HALF,SECTOR = FDC Phi Sector address
C-          NHITS       = Number of hits to store in bank
C-  Output: LKFPSC = Link to "FPSC" bank, 0 if error detected.
C-
C-   Created   4-JAN-1987   Tom Trippe
C-   Updated   x-MAR-1987   Daria Zieminska
C-   Updated   7-OCT-1988   Jeffrey Bantly  for use with new hit format
C-   Updated  19-MAR-1990   Jeffrey Bantly  use logical format
C-   Updated   2-JUL-1990   Jeffrey Bantly  use FDCLNK,use ISETVN,add SAVE 
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:FDCLNK.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFPSC.LINK'
      INTEGER HALF,SECTOR,LOGCHA
      INTEGER LKFPSC,LKFPHI,IXFPSC,IVERS,NHITS
      INTEGER NPTWHT,NWDSHT,NHMXV,MXFPSC,LHFPSC
      INTEGER GZFPSC,GZFDUN,ISETVN
C
      CHARACTER*20 FMFPSC
      CHARACTER*80 VARMSG
C
      LOGICAL FSFPSC
C
      SAVE LHFPSC,IXFPSC,NPTWHT,NWDSHT,IVERS
      DATA FSFPSC/.TRUE./
      DATA IVERS / 0 /
C--------------------------------------------------------------------------
C
C  Book 'FPHI' if needed.
C
      LKFPHI=GZFDUN(HALF,1)
      IF(LKFPHI.EQ.0) CALL BKFPHI(HALF,LKFPHI)
C
      IF(FSFPSC) THEN
        FSFPSC=.FALSE.
        NWDSHT=IQ(LFDCH+6)
        NPTWHT=IQ(LFDCH+7)
        LHFPSC=3+NPTWHT*2
        WRITE(FMFPSC,100) LHFPSC
  100   FORMAT(I2,'I / 1B 7F 1B 3I')
        CALL MZFORM('FPSC',FMFPSC,IXFPSC)
      ENDIF
C
C  Book 'FPSC' if needed.
C
      LKFPSC=GZFPSC(HALF,SECTOR)
      IF(LKFPSC.EQ.0)THEN   ! OK to book sector
        MXFPSC = LHFPSC + NWDSHT*NHITS
        CALL MZBOOK(IXMAIN,LFPSC(SECTOR,HALF),LFDUN(1,HALF),
     +            -IZFPSC-SECTOR,'FPSC',1,1,MXFPSC,IXFPSC,LHFPSC)
        LKFPSC=LFPSC(SECTOR,HALF)
        CALL FCODER(LOGCHA,HALF,1,0,SECTOR,0,0,2)
        IQ(LKFPSC-5)= LOGCHA           ! set numeric bank id
        IQ(LKFPSC)  = ISETVN( IQ(LKFPSC), IVERS )     ! version number
        IQ(LKFPSC+2)= NPTWHT           ! IQ(LFDCH+7)
        IQ(LKFPSC+3)= NWDSHT           ! IQ(LFDCH+6)
      ELSE
        WRITE(VARMSG,10)
   10   FORMAT(' *** BKFPSC *** - ATTEMPTED REBOOKING OF',
     +          ' PREVIOUSLY BOOKED SECTOR')
        CALL ERRMSG('FDC-ALREADY BOOKED','BKFPSC',VARMSG,'I')
        LKFPSC=0
      ENDIF
C
C-------------------------------------------------------------------------
      RETURN
      END
