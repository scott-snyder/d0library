      SUBROUTINE PRVGNH (PRUNIT, KVGNH, NVGNH, CFL, IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : print gain constants header bank VGNH
C-
C-   Inputs  :  PRUNIT: unit number for printout
C-              KVGNH : bank address                
C-              NVGNH : bank number - not used
C-              CFL   : how many banks to print
C-                      'ONE': print just VGNH
C-                      'FULL': print VGNH and all supported banks
C-                      KVGNH must be given for both ONE and FULL
C-              IFL   : flag controlling type of printout - not used
C-
C-   Created  18-OCT-1988   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER PRUNIT, KVGNH, NVGNH, IFL
      CHARACTER CFL*(*)
      INTEGER LOWRUN, HIGRUN
C----------------------------------------------------------------------
C       DOES BANK EXIST?
C----------------------------------------------------------------------
      IF ( CFL .NE. 'ONE' .AND. CFL .NE. 'FULL' ) THEN
        WRITE ( PRUNIT, 900 )
  900   FORMAT (//,10X,' Routine PRVGNH: ',
     &    '******** You must set CFL to ONE or FULL ********')
      ELSE
      ENDIF
      LVGNH = KVGNH
      IF ( LVGNH .LE. 0 ) THEN
        WRITE ( PRUNIT, 910 ) LVGNH
  910   FORMAT (//,10X,' Routine PRVGNH: ',
     &    '**** Bank VGNH does not exist, LVGNH = ',I8,' ****')
        GO TO 999
      ENDIF
C----------------------------------------------------------------------
C       PRINT OUT BANK
C----------------------------------------------------------------------
      LOWRUN = IC(LVGNH+1)
      HIGRUN = IC(LVGNH+2)
      WRITE (PRUNIT, 30) LOWRUN, HIGRUN 
      IF ( CFL .EQ. 'ONE' ) GO TO 999
C       Print out layer banks 
      CALL PRVGNL ( PRUNIT, 0, 0, 'ALL', 0 )
      CALL PRVGNZ ( PRUNIT, 0, 0, 'ALL', 0 )
C----------------------------------------------------------------------
   30 FORMAT (//,' **** Vertex chamber gain constants header bank',
     &  ' VGNH ****',/,
     &  ' **** Lower/Upper valid run #: ',I6,4X,I6)
  999 RETURN
      END
