      SUBROUTINE FDISTP ( FILNAM, IERR )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read STP file in ZEBSTP and return error code:
C-
C-   Inputs  : FILNAM [C*] : input file name
C-   Outputs : IERR [I]    : 0 if OK
C-
C-   Created  23-MAY-1988   Ghita Rahal-Callot
C-   Updated  21-SEP-1988   Jeffrey Bantly   for new FDC_STPFILE
C-   Updated   6-JUN-1990   Jeffrey Bantly   drop old banks if present 
C-   Updated  19-FEB-1991   Robert E. Avery  force garbage collection
C-   Updated  21-JUL-1992   Susan K. Blessing  Changes to allow STP files
C-    to be in exchange mode.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSFDC.LINK'
      INCLUDE 'D0$LINKS:IZFPDH.LINK'
      INCLUDE 'D0$LINKS:IZFGNH.LINK'
      INCLUDE 'D0$LINKS:IZFTMH.LINK'
      INCLUDE 'D0$LINKS:IZFGEH.LINK'
C
      INTEGER LSTPC,FDCID
      INTEGER IERR, LUNIN
      INTEGER ILEN
      CHARACTER*(*) FILNAM
      CHARACTER*2 XCHOP
      LOGICAL OK
      SAVE FDCID
      DATA FDCID /301/
C----------------------------------------------------------------------
C
C ****  Check for FDC STP banks already present and drop them.
C
      LSTPC = LC ( LSTPH - IZSTPC )
      LSFDC = LC ( LSTPC - IZSFDC )
      IF ( LSFDC .NE. 0 ) THEN
        CALL INTMSG(' FDC STP banks present, old banks being dropped.')
        CALL MZDROP ( IXSTP, LSFDC, 'L' )
        CALL MZGARB(IDVSTP,0) 
      ENDIF
C
C ****  Get a unit number and open the requested file.
C
      IERR = 0
      CALL GTUNIT( FDCID, LUNIN, IERR)
      IF ( IERR .NE. 0 ) THEN
        CALL ERRMSG('No unit avail','FDISTP',
     &            'No unit available for input file','F')
        GOTO 999
      ENDIF
      OK = .TRUE.
      CALL D0OPEN ( LUNIN, FILNAM, 'IU', OK )
      IF ( .NOT. OK ) THEN
        CALL ERRMSG('Bad read of file','FDISTP',
     &            'Could not read in FDC STP file','I')
        IERR=1
        GO TO 999
      ENDIF
C
      CALL XZRECL(ILEN,XCHOP)
C ****  Rewind and read in requested file.   Get link values.
C
      CALL FZFILE( LUNIN, ILEN, XCHOP )
      LSTPC = LC ( LSTPH - IZSTPC )
      CALL FZIN  ( LUNIN, IDVSTP, LSTPC, -IZSFDC, ' ', 0, 0 )
      LSTPC = LC ( LSTPH - IZSTPC )
      LSFDC = LC ( LSTPC - IZSFDC )
      LFPDH = LC ( LSFDC - IZFPDH )
      LFGNH = LC ( LSFDC - IZFGNH )
      LFTMH = LC ( LSFDC - IZFTMH )
      LFGEH = LC ( LSFDC - IZFGEH )
      CALL FZENDI( LUNIN, 'TU' )
C
C ****  Release input unit.
C
      IERR = 0
      CALL RLUNIT ( FDCID, LUNIN, IERR )
      IF ( IERR .NE. 0 ) THEN
        CALL ERRMSG('Unit not released','FDISTP',
     &            'Input unit not released properly','I')
        GO TO 999
      ENDIF
C-----------------------------------------------------------------------
  999 RETURN
      END
