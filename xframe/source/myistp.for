      SUBROUTINE MYISTP( filnam, ierr )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read STP file in ZEBSTP and return error code
C-
C-   Inputs  : FILNAM [C*] : input file name
C-   Outputs : IERR [I]    : 0 if OK
C-
C-   Modified 23-AUG-1992   Drew Baden
C-   Created   1-JUN-1988   Ghita Rahal-Callot
C-   Updated  19-JUL-1989   Rajendran Raja  Got rid of BMPIPE.INC
C-                          Will set up geometry directly using VOLPOZ.
C-                          CALMV.INC is obsolete.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-AUG-1992   Drew Baden
C-
      IMPLICIT NONE
C
      INCLUDE 'D0$XFRAME$SOURCE:d0map.inc'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSGEN.LINK'
      INCLUDE 'D0$LINKS:IZSGBP.LINK'
      INCLUDE 'D0$LINKS:IZSGMC.LINK'
C
      INCLUDE 'D0$INC:MVOLS.INC/LIST'
      INCLUDE 'D0$INC:CENMV.INC/LIST'
      INCLUDE 'D0$INC:SRCPR.INC'
C
      INTEGER LZFIND
      INTEGER LSTPC, LSGEN, LSGBP, LSGMC
      CHARACTER*(*) FILNAM
      INTEGER IERR, LUNIN, ILEN
      CHARACTER*12 XCHOP
C----------------------------------------------------------------------
C
C ****  Open the requested file
C
      IERR = 0
C
C ****  Open a file
C
      CALL GTUNIT(D0XUSERUNIT,LUNIN,IERR)
      CALL OPNFIL (LUNIN,FILNAM,IERR)
      IF ( IERR.NE.0 ) GOTO 999
C
      CALL XZRECL(ILEN,XCHOP)
      CALL FZFILE( LUNIN, ILEN, XCHOP )
CCCCC      CALL FZFILE( LUNIN, 0, 'I' )
      LSTPC = LC ( LSTPH - IZSTPC )
      CALL FZIN  ( LUNIN, IDVSTP, LSTPC, -IZSGEN, ' ', 0, 0 )
      CALL FZENDI( LUNIN, 'TU' )
      CALL CLOFIL ( LUNIN )
C
      LSGEN = LC( LSTPC - IZSGEN )
      LSGBP = LC(LSGEN - IZSGBP)
      LSGMC = LC(LSGEN - IZSGMC)
      CALL EZINIT
  999 CALL RLUNIT(D0XUSERUNIT,LUNIN,IERR)
      END
