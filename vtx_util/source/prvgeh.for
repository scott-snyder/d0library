      SUBROUTINE PRVGEH (PRUNIT, KVGEH, NVGEH, CFL, IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : print VTX geometry header bank VGEH
C-
C-   Inputs  :  PRUNIT: unit number for printout
C-              LVGEH : bank address 
C-              NVGEH : bank number - not used
C-              CFL   : how many banks to print
C-                      'ONE': just print VGEH
C-                      'FULL': print VGEH and all supported banks
C-                      KVGEH must be given for both ONE and FULL 
C-              IFL   : flag controlling type of printout - not used
C-
C-   Created  18-OCT-1988   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVMAT.LINK'
      INCLUDE 'D0$LINKS:IZVWAL.LINK'
      INCLUDE 'D0$LINKS:IZVRFT.LINK'
      INCLUDE 'D0$LINKS:IZVZST.LINK'
      INTEGER PRUNIT, KVGEH, NVGEH, IFL
      INTEGER LVMAT, LVWAL, LVRFT, LVZST
      CHARACTER CFL*(*)
      INTEGER I, J, LOWRUN, HIGRUN
C----------------------------------------------------------------------
C       DOES BANK EXIST?
C----------------------------------------------------------------------
      IF ( CFL .NE. 'ONE' .AND. CFL .NE. 'FULL' ) THEN
        WRITE ( PRUNIT, 900 )
  900   FORMAT (//,10X,' Routine PRVGEH: ',
     &    '******** You must set CFL to ONE or FULL ********')
      ELSE
      ENDIF
      LVGEH = KVGEH
      IF ( LVGEH .LE. 0 ) THEN
        WRITE ( PRUNIT, 910 ) LVGEH
  910   FORMAT (//,10X,' Routine PRVGEH: ',
     &    '**** Bank VGEH does not exist, LVGEH = ',I8,' ****')
        GO TO 999
      ENDIF
C-----------------------------------------------------------------------
C       PRINT OUT BANK
C-----------------------------------------------------------------------
C       Write header and valid run numbers
      LOWRUN = IC(LVGEH+1)
      HIGRUN = IC(LVGEH+2)
      WRITE ( PRUNIT, 30 )  IC(LVGEH+1), IC(LVGEH+2)
C       Write position of VTX frame in D0 frame
      WRITE ( PRUNIT, 40 )  (C(LVGEH+2+I), I=1,6)
C       Write the outside dimensions of the three layers
      WRITE ( PRUNIT, 50 )  (C(LVGEH+8+I), I=1,19)
      IF ( CFL .EQ. 'ONE' ) GO TO 999
C       Write banks VMAT, VWAL, VRFT (+ more if banks are added )
      LVMAT = LC(LVGEH-IZVMAT)
      CALL PRVMAT ( PRUNIT, LVMAT, 0, ' ', 0 )
C
      LVWAL = LC(LVGEH-IZVWAL)
      CALL PRVWAL ( PRUNIT, LVWAL, 0, ' ', 0 )
C
      LVRFT = LC(LVGEH-IZVRFT)
      CALL PRVRFT ( PRUNIT, LVRFT, 0, ' ', 0 )
C
      LVZST = LC(LVGEH-IZVZST)
      CALL PRVZST ( PRUNIT, LVZST, 0, ' ', 0 )
C-----------------------------------------------------------------------
   30 FORMAT (//,' **** Vertex Chamber geometrical constants header',
     &  ' bank VGEH ****',/,
     &  ' **** Lower/Upper valid run #: ',I6,4X,I6)
   40 FORMAT (' **** Position of VTX design frame in D0 frame: ',/,
     &  ' (X,Y,Z) at center: ',3(1X,F3.1,1X),
     &  ' (Theta, Phi, Omega): ',3(1X,F3.1,1X))     
   50 FORMAT (' **** Outside dimensions of VTX and drift volumes: ',/,
     &  ' Inner/Outer radius of the VTX volume  : ',2(1X,F5.2,1X),/,
     &  ' Inner/Outer radius of 1st drift volume: ',2(1X,F5.2,1X),/,
     &  ' Inner/Outer radius of 2nd drift volume: ',2(1X,F5.2,1X),/,
     &  ' Inner/Outer radius of 3rd drift volume: ',2(1X,F5.2,1X),/,
     &  ' Zmin/Zmax of end plate, layer number 0: ',2(1X,F5.2,1X),/,
     &  ' Zmin/Zmax of end plate, layer number 1: ',2(1X,F5.2,1X),/,
     &  ' Zmin/Zmax of end plate, layer number 2: ',2(1X,F5.2,1X),/,
     &  ' Length of wall VCF1(inner)/VCF2/VCF3/VCF4: ',4(1X,F5.2,1X),/,
     &  ' Length of the VTX volume: ',F5.2) 
  999 RETURN
      END
