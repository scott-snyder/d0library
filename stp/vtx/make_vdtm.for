      PROGRAM MAKE_VDTM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read VTX distance time map from the binary file
C-                         produced by the electrostatics machinery, fill the
C-                         VDTM bank, then write out the VDTM bank to a ZEBRA
C-                         file.
C-
C-   Inputs  : Binary file containing distance-time map
C-   Outputs : ZEBRA file with the same information.
C-
C-   Created   2-JUN-1992   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      REAL X, Y
      INTEGER IUSER, IER, INLUN, OUTLUN
      INTEGER LAYER, WIRE, VERS, ISETVN, CATEG
      INTEGER LVTMW, GZVTMW, LVDTM, NWIRE, ITEMS, SIZE
      CHARACTER*50 INFILE, OUTFILE
      LOGICAL OPENED
      DATA IUSER / 666 /
C----------------------------------------------------------------------
C
C ****  Initialize ZEBRA and all that other good stuff
C
      CALL MZEBRA(0)                    ! initialize ZEBRA
      CALL INZSTP                       ! initialize ZEBSTP
C
      INFILE = 'DTM_FILE'
      CALL TXTRD('Input filename',INFILE)
      CALL GTUNIT(IUSER,INLUN,IER)
      CALL D0OPEN(INLUN,INFILE,'IU',OPENED)
      IF ( .NOT. OPENED ) THEN
        WRITE(*,*) 'Cannot open ', INFILE, ' Aborting . . .'
        CALL EXIT
      ENDIF
C
      OUTFILE = 'VDTM_FILE'
      CALL TXTRD('Output filename',OUTFILE)
      CALL GTUNIT(IUSER,OUTLUN,IER)
      CALL D0OPEN(OUTLUN,OUTFILE,'OU',OPENED)
      IF ( .NOT. OPENED ) THEN
        WRITE(*,*) 'Cannot open ', OUTFILE, ' Aborting . . .'
        CALL EXIT
      ENDIF
      CALL FZFILE(OUTLUN,0,'O')
C
C ****  Read in standard VTX STP structure, drop the old VTMW bank, and book a
C ****  new one in its place (use layer 0)
C
      CALL VTISTP('D0$STP:VTX_STPFILE.DAT',IER)
      IF ( IER .NE. 0 ) THEN
        WRITE(*,*) 'Error reading VTX_STPFILE, aborting . . .'
        CALL EXIT
      ENDIF
      LAYER = 0
      VERS = 1
      CATEG = 0
      LVTMW = GZVTMW(LAYER)
      IF ( LVTMW .GT. 0 ) CALL MZDROP(IXSTP,LVTMW,' ')
      CALL BKVTMW(LAYER,VERS,LVTMW)
C
C ****  Now read from the input binary file
C
      LAYER = 0
      CATEG = 0
      READ(INLUN) ITEMS, NWIRE, SIZE
      CALL BKVDTM(LAYER,CATEG,NWIRE,ITEMS,SIZE,LVDTM)
C
C ****  Set version number
C
      IF ( ITEMS .LE. 3 ) THEN
        VERS = 0
      ELSE
        VERS = 1
      ENDIF
      IC(LVDTM) = ISETVN(IC(LVDTM),VERS)
      CALL FILL_VDTM(LAYER,CATEG,INLUN)
C
C ****  Rotate the "dumb" wire positions by -pi/2 for version 0
C
      IF ( VERS .EQ. 0 ) THEN
        DO WIRE = 0, 7
          X = C(LVDTM+7+WIRE)
          Y = C(LVDTM+15+WIRE)
          C(LVDTM+7+WIRE) = Y
          C(LVDTM+15+WIRE) = - X
        ENDDO
      ENDIF
C
C ****  VDTM done; write it to the output file
C
      CALL FZOUT(OUTLUN,IDVSTP,LVDTM,1,' ',0,0)
C
      CLOSE(INLUN)
      CLOSE(OUTLUN)
      CALL RLUNIT(IUSER,INLUN,IER)
      CALL RLUNIT(IUSER,OUTLUN,IER)
C
      END
