      SUBROUTINE PRVTMH (PRUNIT, KVTMH, NVTMH, CFL, IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : print time-to-space header bank VTMH
C-
C-   Inputs  :  PRUNIT: unit number for printout
C-              KVTMH : bank address                
C-              NVTMH : bank number - not used
C-              CFL   : how many banks to print 
C-                      'ONE': print just VTMH
C-                      'FULL': print VTMH and all supported banks
C-                      KVTMH must be given for both ONE and FULL
C-              IFL   : flag controlling type of printout - not used
C-
C-   Created  18-OCT-1988   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER PRUNIT, KVTMH, NVTMH, IFL
      CHARACTER CFL*(*)
      INTEGER LOWRUN, HIGRUN
C----------------------------------------------------------------------
C       DOES BANK EXIST?
C----------------------------------------------------------------------
      IF ( CFL .NE. 'ONE' .AND. CFL .NE. 'FULL' ) THEN
        WRITE ( PRUNIT, 900 )
  900   FORMAT (//,10X,' Routine PRVTMH: ',
     &    '******** You must set CFL to ONE or FULL ********')
      ENDIF
      LVTMH = KVTMH
      IF ( LVTMH .LE. 0 ) THEN
        WRITE ( PRUNIT, 910 ) LVTMH
  910   FORMAT (//,10X,' Routine PRVTMH: ',
     &    '**** Bank VTMH does not exist, LVTMH = ',I8,' ****')
        GO TO 999
      ENDIF
C----------------------------------------------------------------------
C       PRINT OUT BANK
C----------------------------------------------------------------------
      LOWRUN = IC(LVTMH+1)
      HIGRUN = IC(LVTMH+2)
      WRITE (PRUNIT, 30) LOWRUN, HIGRUN 
      IF ( CFL .EQ. 'ONE' ) GO TO 999
C       Print out layer banks
      CALL PRVTMW ( PRUNIT, 0, 0, 'ALL', 0 )
      CALL PRVTMZ ( PRUNIT, 0, 0, 'ALL', 0 )
C----------------------------------------------------------------------
   30 FORMAT (//,' **** Vertex chamber time-to-space header bank',
     &  ' VTMH ****',/,
     &  ' **** Lower/Upper valid run #: ',I6,4X,I6)
  999 RETURN
      END
