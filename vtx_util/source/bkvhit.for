      SUBROUTINE BKVHIT(NHITS, LVHIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the bank VHIT, which hangs from the VTX hit
C-                         header bank VTXH.  VHIT contains VTX hit info in
C-                         compressed format.
C-
C-   Inputs  : NHITS = number of VTX hits
C-   Outputs : LVHIT = pointer to bank
C-   Controls: 
C-
C-   Created  23-AUG-1991   Peter M. Grudberg
C-   Updated  25-OCT-1991   Peter M. Grudberg  Fix mistake in MZFORM call 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVHIT.LINK'
C
      INTEGER NHITS, LVHIT
C
      INTEGER MPVHIT(5), GZVTXH, LVTXH, NWORDS, IVERS
      LOGICAL FIRST
C
      DATA FIRST / .TRUE. /
      DATA MPVHIT / 4HVHIT, 0, 0, 0, 0 /
      DATA NWORDS / 3 /                 ! # words / hit
      DATA IVERS  / 0 /                 ! bank version number
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('VHIT','3I / 1B 2I', MPVHIT(5))
      ENDIF
      LVTXH = GZVTXH()
      IF ( LVTXH .LE. 0 ) CALL BKVTXH
      LVTXH = GZVTXH()
      IF ( LVTXH .LE. 0 ) GO TO 999
C
C ****  Book VHIT bank and fill header words (do not fill the NHITS header word
C ****  - increment that word while filling the bank)
C
      MPVHIT(4) = 3 + NHITS * NWORDS
      CALL MZLIFT(IXMAIN, LVHIT, LVTXH, -IZVHIT, MPVHIT, 0)
C
      IQ(LVHIT + 1) = IVERS
      IQ(LVHIT + 3) = NWORDS
C
  999 RETURN
      END
