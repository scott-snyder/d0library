        SUBROUTINE BKVTXH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the banks up to VTXH, hanging from the
C-              bank 'LNKNAM', either 'GEAN' or 'RECO'. Initialize the
C-              VTX link area.
C-
C-   Inputs  : LNKNAM [C] : Name of the bank to hang on, 'GEAN' or 'RECO'
C-   Outputs : none, bank VTXH and link area initialised.
C-
C-   Created   4-OCT-1986   Tom Trippe
C-   Updated  12-OCT-1988   Ghita Rahal-Callot  : Added arguments 
C-   Updated  19-JAN-1989   Peter Grudberg  Use PATHGT, include GCUNIT
C-                          to eliminate arguments; add 2 words
C-   Updated  29-JUN-1989   P.G. - remove GCUNIT, explicit ZEBRA calls
C-                          Add reference link (for pointing to HSTR)
C-   Updated  10-JUL-1989   P.G. - fix call to MZLINT
C-   Updated   9-FEB-1990   P.G. - check if MZLINT call needed
C-   Updated  14-JUN-1990   Peter Grudberg - MZLIFT : IXCOM => IXMAIN
C-   Updated  20-AUG-1991   P.G. - Add struct. link to hang VHIT
C-   Updated   4-NOV-1991   Peter M. Grudberg Remove link area init
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      INCLUDE 'D0$LINKS:IZVTXH.LINK'
      INCLUDE 'D0$INC:VTXLNK.INC/LIST'
C
      CHARACTER*4 LNKNAM
      INTEGER GZRECO, GZGEAN, GZHITS, GZVTXH
      INTEGER LTOPB, LTHITS, NWVSEC, NWVWDA, NWVZLA, NWVZDA
      INTEGER MPVTXH(5), IVERS, ISETVN
      LOGICAL START
      DATA    START / .TRUE. /
      DATA MPVTXH / 0, 11, 10, 14, 2 /
      DATA IVERS / 1 /
C
C----------------------------------------------------------------------
C
      IF ( START ) THEN
        START = .FALSE.
        CALL UCTOH( 'VTXH', MPVTXH(1), 4, 4 )
      ENDIF
C
C ****  Test for HEAD bank, abort if doesn't exist
C
      IF ( LHEAD .EQ. 0 ) THEN
        CALL ERRMSG(' Error in booking VTXH','BKVTXH',
     &    ' HEAD bank does not exist','W')
        CALL EXIT(1)
      ENDIF
C
C ****  First book if needed, and set length of VSEC and VWDA datas
C
      CALL PATHGT(LNKNAM)
      IF ( LNKNAM .EQ. 'GEAN' ) THEN
        NWVSEC = 11
        NWVWDA = 9
        NWVZLA = 11
        NWVZDA = 9
        LTOPB = GZGEAN()
        IF ( LTOPB .EQ. 0 ) THEN
          CALL BKGEAN(LTOPB)
        ENDIF
      ELSEIF ( LNKNAM .EQ. 'RECO' ) THEN
        NWVSEC = 10
        NWVWDA = 8
        NWVZLA = 10
        NWVZDA = 8
        LTOPB  = GZRECO()
        IF ( LTOPB .EQ. 0 ) THEN
          CALL BKRECO(LTOPB)
        ENDIF
      ELSE
        CALL ERRMSG(' Error in booking VTXH', 'BKVTXH',
     &    ' Illegal or nonexistent PATH', 'W' )
        CALL EXIT(1)
      ENDIF
C
C ****  Book HITS bank
C
      LTHITS = GZHITS()
      IF ( LTHITS .EQ. 0 ) THEN
        CALL BKHITS(LTHITS)
      ENDIF
C
C
C ****  Book VTXH and store VSEC,VWDA length inside
C
      LVTXH = GZVTXH()
      IF ( LVTXH .EQ. 0 ) THEN
        CALL MZLIFT ( IXMAIN, LVTXH, LTHITS, -IZVTXH, MPVTXH, 0 )
      ENDIF
      IQ( LVTXH + 4 ) = NWVSEC
      IQ( LVTXH + 5 ) = 8               ! Number of wires / sector
      IQ( LVTXH + 6 ) = NWVWDA
      IQ( LVTXH + 7 ) = 16              ! Number of FADC / sector
      IQ( LVTXH + 8 ) = NWVZLA
      IQ( LVTXH + 9 ) = NWVZDA
      IQ( LVTXH + 10 ) = 0              ! Hitfinding status words
      IQ( LVTXH + 11 ) = 0              ! No hitfinding done yet
      IQ( LVTXH + 12 ) = 0
      IQ( LVTXH + 13 ) = 0
C
C ****  Set version number
C
      IQ(LVTXH) = ISETVN(IQ(LVTXH),IVERS)
C
  999 CONTINUE
      RETURN
      END
