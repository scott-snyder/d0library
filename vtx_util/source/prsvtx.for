      SUBROUTINE PRSVTX ( PRUNIT, KSVTX, NSVTX, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : print vertex chamber static parameter
C-                         header bank SVTX.  Also can do full
C-                         printout of all supported banks.
C-
C-   Inputs  :  PRUNIT: unit number for printout
C-              KSVTX : bank address
C-              NSVTX : bank number - not used
C-              CFL   : flag to control printout
C-                      'ONE' to print only SVTX (at address KSVTX)
C-                      'FULL' to print SVTX and all supported banks
C-                      KSVTX be given for both ONE and FULL printouts
C-              IFL   : flag to control partial printout - not used
C-
C-   Created   3-NOV-1988   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVPDH.LINK'
      INCLUDE 'D0$LINKS:IZVGNH.LINK'
      INCLUDE 'D0$LINKS:IZVTMH.LINK'
      INCLUDE 'D0$LINKS:IZVGEH.LINK'
      INCLUDE 'D0$LINKS:IZVALH.LINK'
      INTEGER PRUNIT, KSVTX, NSVTX, IFL, LVALH
      CHARACTER CFL*(*)
      INTEGER LOWRUN, HIGRUN
C----------------------------------------------------------------------
C       Print which banks (do they exist)?
C----------------------------------------------------------------------
      IF ( CFL .NE. 'ONE' .AND. CFL .NE. 'FULL' ) THEN
        WRITE ( PRUNIT, 900 )
  900   FORMAT (//,10X,' Routine PRSVTX: ',
     &    '******** You must set CFL to ONE or FULL ********')
        GO TO 999
      ENDIF
      LSVTX = KSVTX
C       Check for existence of SVTX at LSVTX
      IF ( LSVTX .LE. 0 ) THEN
        WRITE ( PRUNIT, 910 ) LSVTX
  910   FORMAT (//,10X,' Routine PRSVTX: ',
     &    '**** Bank SVTX does not exist, LSVTX = ',I8,' ****')
        GO TO 999
      ENDIF
C----------------------------------------------------------------------
C       Print out bank(s)
C----------------------------------------------------------------------
      LOWRUN = IC(LSVTX+1)
      HIGRUN = IC(LSVTX+2)
      WRITE ( PRUNIT, 30 ) LOWRUN, HIGRUN
      IF ( CFL .EQ. 'ONE' ) GO TO 999
C       Print VPDH and supported banks
      LVPDH = LC(LSVTX-IZVPDH)
      CALL PRVPDH ( PRUNIT, LVPDH, 0, 'FULL', 0 )
C       Print VGNH and supported banks
      LVGNH = LC(LSVTX-IZVGNH)
      CALL PRVGNH ( PRUNIT, LVGNH, 0, 'FULL', 0 )
C       Print VTMH and supported banks
      LVTMH = LC(LSVTX-IZVTMH)
      CALL PRVTMH ( PRUNIT, LVTMH, 0, 'FULL', 0 )
C       Print VGEH and supported banks
      LVGEH = LC(LSVTX-IZVGEH)
      CALL PRVGEH ( PRUNIT, LVGEH, 0, 'FULL', 0 )
C       Print VALH and supported banks
      LVALH = LC(LSVTX-IZVALH)
      CALL PRVALH ( PRUNIT, LVALH, 0, 'FULL', 0 )
C       If new banks are added, they must be inserted here
C----------------------------------------------------------------------
   30 FORMAT (//,' **** Vertex Chamber static parameters headers bank',
     &  ' SVTX ****',/,
     &  ' **** Lower/Upper valid run #: ',I6,4X,I6)
  999 RETURN
      END
