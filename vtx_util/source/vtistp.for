
      SUBROUTINE VTISTP ( FILNAM, IERR )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read STP file in ZEBSTP and return error code:
C-
C-   Inputs  : FILNAM [C*] : input file name
C-   Outputs : IERR [I]    : 0 if OK
C-
C-   Created  23-MAY-1988   Ghita Rahal-Callot
C-   Updated   6-FEB-1990   Peter Grudberg  Avoid reading stp file twice 
C-   Updated  30-OCT-1990   Peter Grudberg  Release input unit 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSVTX.LINK'
      INCLUDE 'D0$LINKS:IZVPDH.LINK'
      INCLUDE 'D0$LINKS:IZVGNH.LINK'
      INCLUDE 'D0$LINKS:IZVTMH.LINK'
      INCLUDE 'D0$LINKS:IZVGEH.LINK'
C
      CHARACTER*(*) FILNAM
      INTEGER IERR, LUNIN, LSTPC, ILEN
      LOGICAL FIRST,OK
      CHARACTER*12 XCHOP
      SAVE FIRST
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
C
      IERR = 0
C ****  Open the requested file
C ****  Avoid reading stp file in twice
C
      CALL GTUNIT(1,LUNIN,IERR)
      CALL D0OPEN(LUNIN, FILNAM, 'IU', OK)
      IF (.NOT.OK) then
        IERR = 1
        GOTO 999
      ENDIF
      CALL XZRECL(ILEN,XCHOP)
      CALL FZFILE( LUNIN, ILEN, XCHOP )
C
C
C  drop the SVTX bank before read in STP file for another run
C
      LSTPC = LC ( LSTPH - IZSTPC )
      LSVTX = LC(LSTPC - IZSVTX)
      IF (LSVTX .GT. 0) CALL MZDROP(IXSTP,LSVTX,' ')
C
      CALL FZIN  ( LUNIN, IDVSTP, LSTPC, -IZSVTX, ' ', 0, 0 )
      LSTPC = LC ( LSTPH - IZSTPC )
      LSVTX = LC( LSTPC - IZSVTX )
      LVPDH = LC( LSVTX - IZVPDH )
      LVGNH = LC( LSVTX - IZVGNH )
      LVTMH = LC( LSVTX - IZVTMH )
      LVGEH = LC( LSVTX - IZVGEH )
      CALL FZENDI( LUNIN, 'TU' )
      CALL CLOFIL ( LUNIN )
      CALL RLUNIT(1,LUNIN,IERR)
  999 RETURN
      END
