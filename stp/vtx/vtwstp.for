      PROGRAM VTWSTP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write Geometry file for the VTX
C-
C- Input:
C-
C- Output:
C-    IERR       error code
C-    STP-bank sturcture
C-
C-
C-   Created  15-SEP-1988   Ghita Rahal-Callot
C-   Updated  21-OCT-1988   Ghita Rahal-Callot  : add BLVTMH 
C-   Updated  19-MAR-1991   Peter M. Grudberg  remove call to BLVWAL 
C-   Updated   1-AUG-1992   Peter M. Grudberg  add processing of VTX_D0STPFILE,
C-                          DTM maps, tzeros.  Change to x-change mode output.  
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      REAL DPHI(3), XZERO(3), YZERO(3)
      INTEGER LUN, IER, LKSVTX
      CHARACTER*25 FILENAME
C----------------------------------------------------------------------
C
C ****  Initialize the Zebra structure
C
      CALL MZEBRA(0)
      CALL INZSTP
C
      CALL INRCP('VTWSTP_RCP',IER)
      IF ( IER .NE. 0 ) THEN
        WRITE(6,*) ' ***** Error accessing RCP file; abort'
        GO TO 999
      ENDIF
C
C ****  Book SVTX
C
      CALL BKSVTX(LKSVTX)
C
C ****  Create and fill geometry banks
C
      CALL BLVGEH
C
C ****  Create and fill alignment banks
C
      CALL BLVALH
C
C ****  Create and fill gain, pedestal and time banks
C
      CALL BLVGNH
      CALL BLVPDH
      CALL BLVTMH
C
C ****  Now write out the MC STP structure
C
      CALL GTUNIT(666,LUN,IER)
      FILENAME = 'VTX_STPFILE'
      CALL VTSTOR(LUN,FILENAME)
C
C ****  Now do the modifications to VTX_STPFILE needed to change it to
C ****  VTX_D0STPFILE.  First, drop old (version 0) VTMW banks and book new
C ****  (version 1) banks.  Fill the new VTMW banks with tzeros based on data.
C ****  AND SCALE FACTOR FOR DTM (12-FEB-1993)
C
      CALL VTMW_UPDATE
C
C ****  Next, read ASCII DTM maps into VDTM banks hanging from the VTMW banks 
C ****  (and if requested, write the VDTM banks into a ZEBRA format file).
C
      CALL VDTM_INI
C
C ****  Now read gain parameters for real data into the VGNL banks
C
      CALL VGNL_UPDATE
C
C ****  Now GENERATE NEW ALIGNMENT BANKS
C
      CALL VALS_UPDATE
C
C ****  Finally, write out the modified VTX STP structure
C
      FILENAME = 'VTX_D0STPFILE'
      CALL VTSTOR(LUN,FILENAME)
C
      CALL RLUNIT(666,LUN,IER)
C
C
  999 CONTINUE
      END
