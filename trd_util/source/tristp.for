      SUBROUTINE TRISTP( FILNAM, IERR )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read STP file in ZEBSTP and return error code
C-                              Based on CDISTP by Olivier Callot
C-
C-   Inputs  : FILNAM [C*] : input file name
C-   Outputs : IERR [I]    : 0 if OK
C-
C-   Created  21-MAR-1988   A.M.Jonckheere
C-   Updated  23-MAY-1988   Ghita Rahal-Callot  : Modify the way to open
C-                          and close a file. Also suppress the intermediate
C-                          routine TRDSTP.
C-   Updated   6-JUN-1990   J.Fr.Glicenstein  Change from OPNFIL to D0OPEN
C-                                            if STRD bank exists, drops
C-                                            it
C-   Updated  30-JUN-1992   Alain PLUQUET  Use exchange mode
C-   Updated   7-MAR-1994   A. Zylberstejn   Prevent multi calls
C-   Updated   5-OCT-1994   Alain PLUQUET   Add error flag if wrong or no STP
C-                                          file
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSTRD.LINK'
      INCLUDE 'D0$LINKS:IZTPDH.LINK'
      INCLUDE 'D0$LINKS:IZTGAI.LINK'
      INCLUDE 'D0$LINKS:IZTGEN.LINK'
      INCLUDE 'D0$LINKS:IZTGEO.LINK'
      INTEGER LSTPC
      CHARACTER*(*) FILNAM
      CHARACTER*12 XCHOP
      INTEGER IERR, LUNIN, GZSTRD,ILEN,LTPHY
      LOGICAL OK,FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C     Open the requested file
C----------------------------------------------------------------------
      IF(.NOT.FIRST)RETURN
      FIRST=.FALSE.
      IERR = 0
      CALL GTUNIT(1,LUNIN,IERR)
      CALL D0OPEN( LUNIN, FILNAM,'IU',OK)
      IF (.NOT.OK) THEN
        IERR=1
        GO TO 999
      ENDIF
      CALL XZRECL(ILEN,XCHOP)
      CALL FZFILE(LUNIN,ILEN,XCHOP)
      LSTRD = GZSTRD()
      IF (LSTRD.GT.0) THEN
        CALL MZDROP(IXSTP,LSTRD,' ')
      ENDIF
      LSTPC = LC ( LSTPH - IZSTPC )
      CALL FZIN  ( LUNIN, IDVSTP, LSTPC, -IZSTRD, ' ', 0, 0 )
      LSTPC = LC( LSTPH - IZSTPC )
      LSTRD = LC( LSTPC - IZSTRD )
      LTPDH = LC( LSTRD - IZTPDH )
      LTGAI = LC( LSTRD - IZTGAI )
      LTGEN = LC( LSTRD - IZTGEN )
      LTGEO = LC( LSTRD - IZTGEO )
      CALL FZENDI( LUNIN, 'TU' )
      CALL CLOFIL ( LUNIN )
  999 RETURN
      END
