      SUBROUTINE SAISTP ( FILNAM, IERR )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read STP file in ZEBSTP and return error code:
C-
C-   Inputs  : FILNAM [C*] : input file name
C-   Outputs : IERR [I]    : 0 if OK
C-
C-   Created  28-SEP-1990   V. Glebov & A. Efimov & V. Podstavkov
C-   Updated   1-OCT-1990   V. Glebov & A. Efimov & V. Podstavkov   
C-   Updated  21-APR-1991   Vladimir Glebov  ! Move SSAM under STPC 
C-   Updated   3-MAY-1991   Andrei Kiryunin: open and close file with         
C-                          standard subroutines 
C-   Updated  21-Sep-1992   Herbert Greenlee
C-      Change D0OPEN call to be compatible with exchange mode STP file
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSSAM.LINK'
C
      INTEGER LSTPC
      CHARACTER*(*) FILNAM
      INTEGER IERR, LUNIN, IUSER
      PARAMETER( IUSER = 109 )
      CHARACTER*80 MSGSTR               ! Error message
      LOGICAL OK
      INTEGER XLEN
      CHARACTER*8 XCHOP
      REAL    RNDM
C----------------------------------------------------------------------
C
C ****  Open the requested file
C
      IERR = 0
      CALL GTUNIT (IUSER,LUNIN,IERR)
      CALL D0OPEN (LUNIN, FILNAM, 'IU', OK)
      IF (.NOT.OK) THEN
        IERR = 1
        GOTO 999
      ENDIF
      CALL XZRECL( XLEN, XCHOP)
      CALL FZFILE( LUNIN, XLEN, XCHOP)
      LSTPC = LC ( LSTPH - IZSTPC )
C
      CALL FZIN  ( LUNIN, IDVSTP, LSTPC, -IZSSAM, ' ', 0, 0 )
      CALL FZENDI( LUNIN, 'TU' )
      CALL CLOFIL ( LUNIN )
      CALL RLUNIT (IUSER,LUNIN,IERR)
C
  999 RETURN
      END
