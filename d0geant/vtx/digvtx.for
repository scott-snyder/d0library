      LOGICAL FUNCTION DIGVTX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-  Subroutine DIGVTX creates digitizations from the GEANT hits stored
C-  by subroutine STPVTX.  There are three possible digitization outputs
C-  controlled by switches SVTX(I):
C-  Switch   ZEBRA Banks        Contents
C-  SVTX(1)     CDD1      FADC data to mimic actual detector output.
C-  SVTX(2)  VSEC, VZLA   Hits with wire ends combined and z-clusters.
C-  SVTX(3)  VWDA, VZDA   Channel "data": times, pulse heights, widths.
C-
C-   Returned value  : TRUE
C-   Inputs  :     Hits stored by GEANT in JHITS structure
C-   Outputs :     Bank  CDD1
C-                 Bank  VSEC, VZLA
C-                 Bank  VWDA, VZDA
C-   Controls: D0LOG.INC
C-
C-   Created  29-Dec-1986   Tom Trippe
C-   Updated   9-OCT-1988  Tom Trippe: follows CDC switch handling
C-   Updated  24-NOV-1988   Ghita Rahal-Callot  Drop banks no longer needed
C-   Updated  15-DEC-1988   Peter Grudberg  Eliminate arguments in
C-                         call to BKVTXH;  fix to drop existent banks only
C-   Updated  26-JUN-1989   P.G. - update print structure, routine structure
C-   Updated  17-JUL-1989   Harrison B. Prosper
C-   Made into a pbd interface function
C-   Updated  12-DEC-1989   P.G. - new VTX digitization structure
C-   Updated   8-FEB-1990   P.G. - disable VTXLNK before exiting
C-   Updated  23-AUG-1990   Peter Grudberg  Drop empty hit banks 
C-   Updated   1-OCT-1992   Peter M. Grudberg Remove z-strips
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:VTXLNK.INC/LIST'
      INTEGER LAY, SEC, LAYMAX, SECMAX, LVRFT, GZVRFT
C-----------------------------------------------------------------------
      DIGVTX = .TRUE.
      IF ( DVTX .LT. 3 ) GOTO 999
C
C ****  Initialize link area
C
      IF ( VTXLNK(1) .EQ. 0 ) THEN
        CALL MZLINT(IXCOM, '/VTXLNK/', VTXLNK, LCDD1, VTXLNK)
      ENDIF
C
C ****  Book the Monte Carlo VTXH ZEBRA Bank (and higher banks if needed)
C
      CALL BKVTXH
C
C **** Loop over chamber layers and sectors to make wire hit, data banks
C
      LVRFT = GZVRFT(0)
      IF ( LVRFT .LE. 0 ) THEN
        WRITE (LOUT,*) ' **** DIGVTX:  bank VRFT not defined'
        CALL EXIT(1)
      ENDIF
      LAYMAX = IC( LVRFT + 1 )
      DO LAY = 0 ,  LAYMAX - 1
        SECMAX = IC( LVRFT + 2 + 7*LAY )
        DO SEC = 0 ,  SECMAX - 1
          CALL MKVTXW( LAY, SEC )
        ENDDO
      ENDDO
C
C **** Create the raw FADC bank CDD1 if SVTX(1) = 1.
C
      IF ( SVTX(1) .EQ. 1. ) CALL BLCDD1
C
C ****  Drop VTXH on down if only raw data is requested
C
      IF ( SVTX(2) .NE. 1. .AND. SVTX(3) .NE. 1 ) THEN
        CALL MZDROP(IXCOM, LVTXH, 'L')
C
C ****  Drop data banks VWDA if not asked for
C
      ELSEIF ( SVTX(3) .NE. 1 ) THEN
        DO 10 LAY =  0, 2
          DO 11 SEC = 0, 31
            IF ( LVWDA ( SEC, LAY) .GT. 0 )
     &           CALL MZDROP( IXCOM, LVWDA(SEC,LAY), ' ')
   11     CONTINUE
   10   CONTINUE
      ENDIF
C
C  Print output if print control flag DDIG = 1.
C  Print level flag PVTX = level of printout
C   (0=none, 1=minimal, 2=more detailed, 3=full content printed)
C
      IF ( DDIG .EQ. 1 ) THEN
        IF ( SVTX(3) .EQ. 1. .OR. SVTX(2) .EQ. 1. ) THEN
          CALL PRVTX(LOUT,PVTX)         ! Print hit, data banks
        ENDIF
        IF ( SVTX(1) .EQ. 1. ) THEN
          CALL PRCDD1( LOUT, 0, 0, 'ALL', PVTX ) ! Print raw data
        ENDIF
      ENDIF
C
C ****  Deactivate temporary link area
C
      VTXLNK(1) = 0
C
C
  999 RETURN
      END
