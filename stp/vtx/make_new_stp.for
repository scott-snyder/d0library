      PROGRAM MAKE_NEW_STP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read in current version of VTX_D0STPFILE, drop old
C-                         time banks and replace them with new ones.
C-                         Basically, this is to change the VDTM banks that are
C-                         stored in the standard VTX_D0STPFILE.
C-
C-   Inputs  : D0$STP:VTX_D0STPFILE
C-   Outputs : new VTX_D0STPFILE (in default area)
C-   Controls: in VTRAKS.RCP
C-
C-   Created   5-JUN-1992   Peter Grudberg
C-   Updated  19-APR-1994   Adam L. Lyon: Fixed to compile under IBM/AIX
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER IER, LUN, IUSER
      CHARACTER*20 FILENAME
      LOGICAL OK
      DATA IUSER / 666 /
C----------------------------------------------------------------------
C
C ****  Initialize ZEBRA and all that other good stuff
C
      CALL MZEBRA(0)                    ! initialize ZEBRA
      CALL INZSTP                       ! initialize ZEBSTP
C
C ****   Read in VTRAKS_RCP
C
      CALL INRCP('VTRAKS_RCP',IER)
      IF ( IER .NE. 0 ) THEN
        PRINT *, 'Error reading VTRAKS_RCP, abort'
        CALL EXIT
      ENDIF
C
C ****  Read in the library VTX_D0STPFILE
C
      CALL VTISTP('D0$STP:VTX_D0STPFILE.DAT',IER)
      IF ( IER .NE. 0 ) THEN
        PRINT *, 'Error reading VTX_D0STPFILE, abort'
        CALL EXIT
      ENDIF
C
C ****  Make the changes to the time banks
C
      CALL INIT_VDTM(OK)
      IF ( .NOT. OK ) THEN
        PRINT *, 'Error in INIT_VDTM, abort'
        CALL EXIT
      ENDIF
C
C ****  Now write out the VTX STP structure in VTX_D0STPFILE in default area
C
      CALL GTUNIT(IUSER,LUN,IER)
      FILENAME = 'VTX_D0STPFILE.DAT'
      CALL D0OPEN(LUN,FILENAME,'OU',OK)
      IF ( .NOT. OK ) THEN
        PRINT *, 'Error opening output file, abort'
        CALL EXIT
      ENDIF
      CALL FZFILE(LUN,0,'O')
      IF ( LSVTX .LE. 0 ) THEN
        PRINT *, 'SVTX does not exist, abort'
        CALL EXIT
      ENDIF
      CALL FZOUT(LUN,IDVSTP,LSVTX,1,' ',0,0)
C
      CLOSE(LUN)
      CALL RLUNIT(IUSER,LUN,IER)
C
      END
