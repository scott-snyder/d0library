      SUBROUTINE PRVPDH (PRUNIT, KVPDH, NVPDH, CFL, IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : print pedestal header bank VPDH
C-
C-   Inputs  :  PRUNIT: unit number for printout
C-              KVPDH : bank address                
C-              NVPDH : bank number - not used
C-              CFL   : how many banks to print 
C-                      'ONE' just prints VPDH
C-                      'FULL' prints VPDH and all supprted banks
C-                      KVPDH must be given for either case
C-              IFL   : flag controlling type of printout - not used
C-
C-   Created  18-OCT-1988   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER PRUNIT, KVPDH, NVPDH, IFL
      CHARACTER CFL*(*)
      INTEGER LOWRUN, HIGRUN
C----------------------------------------------------------------------
C       DOES BANK EXIST?
C----------------------------------------------------------------------
      IF ( CFL .NE. 'ONE' .AND. CFL .NE. 'FULL' ) THEN
        WRITE ( PRUNIT, 900 )
  900   FORMAT (//,10X,' Routine PRVPDH: ',
     &    '******** You must set CFL to ONE or FULL ********')
        GO TO 999
      ENDIF
C       Check for existence of VPDH at LVPDH
      LVPDH = KVPDH
      IF ( LVPDH .LE. 0 ) THEN
        WRITE ( PRUNIT, 910 ) LVPDH
  910   FORMAT (//,10X,' Routine PRVPDH: ',
     &    '**** Bank VPDH does not exist, LVPDH = ',I8,' ****')
        GO TO 999
      ENDIF
C----------------------------------------------------------------------
C       PRINT OUT BANK
C----------------------------------------------------------------------
      LOWRUN = IC(LVPDH+1)
      HIGRUN = IC(LVPDH+2)
      WRITE (PRUNIT, 30) LOWRUN, HIGRUN 
      IF ( CFL .EQ. 'ONE' ) GO TO 999
C       Print out layer pedestal banks 
      CALL PRVPDL ( PRUNIT, 0, 0, 'ALL', 0 )
      CALL PRVPDZ ( PRUNIT, 0, 0, 'ALL', 0 )
C----------------------------------------------------------------------
   30 FORMAT (//,' **** Vertex chamber pedestal header bank',
     &  ' VPDH ****',/,
     &  ' **** Lower/Upper valid run #: ',I6,4X,I6)
  999 RETURN
      END
