      SUBROUTINE CDISTP( FILNAM, IERR )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read STP file in ZEBSTP and return error code
C-
C-   Inputs  : FILNAM [C*] : input file name
C-   Outputs : IERR [I]    : 0 if OK
C-
C-   Created  18-FEB-1988   Olivier Callot
C-   Updated   1-JUN-1988   Ghita Rahal-Callot  Open and Close files done
C-                                              in .INC files
C-   Updated   5-JUN-1990   Qizhong Li-Demarteau     drop SCDC bank before
C-                                        read in STP file for another run 
C-                                        and use GTUNIT and D0OPEN
C-   Updated  10-JUL-1992   Qizhong Li-Demarteau  to be able to read the
C-                                                exchange mode STP file
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSCDC.LINK'
      INCLUDE 'D0$LINKS:IZDPDH.LINK'
      INCLUDE 'D0$LINKS:IZDGNH.LINK'
      INCLUDE 'D0$LINKS:IZDTMH.LINK'
      INCLUDE 'D0$LINKS:IZDGEH.LINK'
C
      INTEGER LSTPC, ILEN
      INTEGER IERR, LUNIN
      CHARACTER*(*) FILNAM
      CHARACTER*12  XCHOP
      LOGICAL OK
C----------------------------------------------------------------------
C
      IERR = 0
C
C  open the STP file
C
      CALL GTUNIT(1,LUNIN,IERR)
      CALL D0OPEN(LUNIN,FILNAM,'IU',OK)
      IF (.NOT.OK) then
        IERR = 1
        GOTO 999
      ENDIF
      CALL XZRECL(ILEN,XCHOP)
      CALL FZFILE(LUNIN,ILEN,XCHOP)
      LSTPC = LC(LSTPH - IZSTPC)
C
C  drop the SCDC bank before read in STP file for another run
C
      LSCDC = LC(LSTPC - IZSCDC)
      IF (LSCDC .GT. 0) CALL MZDROP(IXSTP,LSCDC,' ')
C
      CALL FZIN  ( LUNIN, IDVSTP, LSTPC, -IZSCDC, ' ', 0, 0 )
      LSTPC = LC ( LSTPH - IZSTPC )
      LSCDC = LC( LSTPC - IZSCDC )
      LDPDH = LC( LSCDC - IZDPDH )
      LDGNH = LC( LSCDC - IZDGNH )
      LDTMH = LC( LSCDC - IZDTMH )
      LDGEH = LC( LSCDC - IZDGEH )
      CALL FZENDI( LUNIN, 'TU' )
      CALL CLOFIL ( LUNIN )
      CALL RLUNIT(1,LUNIN,IERR)
C
  999 RETURN
      END
