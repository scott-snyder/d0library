      SUBROUTINE PRVALH (PRUNIT, KVALH, NVALH, CFL, IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : print alignment constants header bank VALH
C-
C-   Inputs  :  PRUNIT: unit number for printout
C-              KVALH : bank address                
C-              NVALH : bank number - not used
C-              CFL   : how many banks to print 
C-                      'ONE': print just VALH
C-                      'FULL': print VALH and all supported banks
C-                      KVALH must be given for both ONE and FULL
C-              IFL   : flag controlling type of printout - not used
C-
C-   Created  18-OCT-1988   Peter Grudberg
C-   Updated  14-JUN-1990   Peter Grudberg  IXSTP ==> IDVSTP 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVRFT.LINK'
      INTEGER PRUNIT, KVALH, LVALH, NVALH, IFL
      CHARACTER CFL*(*)
      INTEGER LOWRUN, HIGRUN
      INTEGER LVRFT, LZLOC, NLAYER, ILAY, LVALL
C----------------------------------------------------------------------
C       DOES BANK EXIST?
C----------------------------------------------------------------------
      IF ( CFL .NE. 'ONE' .AND. CFL .NE. 'FULL' ) THEN
        WRITE ( PRUNIT, 900 )
  900   FORMAT (//,10X,' Routine PRVALH: ',
     &    '******** You must set CFL to ONE or FULL ********')
      ENDIF
      LVALH = KVALH
      IF ( LVALH .LE. 0 ) THEN
        WRITE ( PRUNIT, 910 ) LVALH
  910   FORMAT (//,10X,' Routine PRVALH: ',
     &    '**** Bank VALH does not exist, LVALH = ',I8,' ****')
        GO TO 999
      ENDIF
C----------------------------------------------------------------------
C       PRINT OUT BANK
C----------------------------------------------------------------------
      LOWRUN = IC(LVALH+1)
      HIGRUN = IC(LVALH+2)
      WRITE (PRUNIT, 30) LOWRUN, HIGRUN
      IF ( CFL .EQ. 'ONE' ) GO TO 999
C       loop over layers; first, how many?
      LVRFT = LZLOC(IDVSTP, 'VRFT', IZVRFT)
      NLAYER = IC(LVRFT+1)
      DO 100 ILAY = 1 , NLAYER
        LVALL = LC(LVALH-ILAY)
        CALL PRVALL ( PRUNIT, LVALL, 0, ' ', 0 )
        CALL PRVALS ( PRUNIT, LVALL, 0, 'ALL', 0 )
  100 CONTINUE
C       Now do the z-layers
      CALL PRVALZ ( PRUNIT, 0, 0, 'ALL', 0 )
C----------------------------------------------------------------------
   30 FORMAT (//,' **** Vertex chamber alignment constants header bank',
     &  ' VALH ****',/,
     &  ' **** Lower/Upper valid run #: ',I6,4X,I6)
  999 RETURN
      END
