      SUBROUTINE D0ISTP( FILNAM, IERR )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read STP file in ZEBSTP and return error code
C-
C-   Inputs  : FILNAM [C*] : input file name
C-   Outputs : IERR [I]    : 0 if OK
C-
C-   Created   1-JUN-1988   Ghita Rahal-Callot
C-   Updated  19-JUL-1989   Rajendran Raja  Got rid of BMPIPE.INC
C-                          Will set up geometry directly using VOLPOZ. 
C-                          CALMV.INC is obsolete.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
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
      INTEGER IERR, LUNIN 
C----------------------------------------------------------------------
C
C ****  Open the requested file
C
      IERR = 0
C
C ****  Open a file
C
      CALL GTUNIT(80,LUNIN,IERR)
      CALL OPNFIL (LUNIN,FILNAM,IERR)
      IF ( IERR.NE.0 ) GOTO 999
C
      CALL FZFILE( LUNIN, 0, 'I' )
      LSTPC = LC ( LSTPH - IZSTPC )
      CALL FZIN  ( LUNIN, IDVSTP, LSTPC, -IZSGEN, ' ', 0, 0 )
      CALL FZENDI( LUNIN, 'TU' )
      CALL CLOFIL ( LUNIN )
C
      LSGEN = LC( LSTPC - IZSGEN )
      LSGBP = LC(LSGEN - IZSGBP)
      LSGMC = LC(LSGEN - IZSGMC)
C
      CALL EZNAME('GNWSTP_RCP',LSGEN,3) ! NAME THE SRCP BANK.
      CALL EZPICK('GNWSTP_RCP')
C
C ****  Now fill MVOL into MVOLS.INC Just in case some one uses it in CD
C
      CALL EZGET('OVERALL_MOTHER_VOLUME',SRCPAR,IERR)
      NMVOL = NAME
      CALL UCOPY(SRCPAR(12),MVOL,3)    ! TUBE PARAMETERS
      MMVOL = NUMED
      CALL UCOPY(SRCPAR(8),PMMVL,3)     ! POSITIONS
C
C ****  Now fill MCEN into CENMV.INC for use with CD
C
      CALL EZGET('CENTRAL_DETECTOR_MOTHER_VOLUME',SRCPAR,IERR)
      NMCEN = NAME
      CALL UCOPY(SRCPAR(12),MCEN,3)    ! TUBE PARAMETERS
      MMCEN = NUMED
      CALL UCOPY(SRCPAR(8),PMCEN,3)     ! POSITIONS
C
C ****  BMPIPE.INC, CALMV.INC are obsolete. 
C ****  Geant positions Mother volumes
C ****  directly from Zebra now.
C
      CALL EZRSET
  999 CALL RLUNIT(80,LUNIN,IERR)
      END
