      SUBROUTINE PRVMAT (PRUNIT, KVMAT, NVMAT, CFL, IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : print VTX materials bank VMAT
C-
C-   Inputs  :  PRUNIT: unit number for printout
C-              KVMAT : bank address
C-              NVMAT : bank number - not used
C-              CFL   : how many banks to print - not used
C-              IFL   : flag controlling type of printout - not used
C-
C-   Created  19-OCT-1988   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER PRUNIT, KVMAT, LVMAT, NVMAT, IFL
      CHARACTER CFL*(*)
      INTEGER I, J, NWMAT, NBMAT
      CHARACTER*20 MATNAM
C----------------------------------------------------------------------
C       Does bank exist?
C----------------------------------------------------------------------
      LVMAT = KVMAT
      IF ( LVMAT .LE. 0 ) THEN
        WRITE ( PRUNIT, 900 )
  900   FORMAT (/,10X,' Routine PRVMAT: ',
     &    '******** The bank VMAT does not exist ********')
        GO TO 999
      ENDIF
C----------------------------------------------------------------------
C       Print out bank
C----------------------------------------------------------------------
C       Write header and numbers of materials, words per material 
      NBMAT = IC(LVMAT+1)
      NWMAT = IC(LVMAT+2)
      WRITE (PRUNIT, 30) NBMAT, NWMAT 
C       Write material data
      LVMAT = LVMAT + 2
      DO 100 I = 0, NBMAT-1
        CALL UHTOC (C(LVMAT+2), 4, MATNAM, 20)
        WRITE (PRUNIT, 40) IC(LVMAT+1), MATNAM, (C(LVMAT+6+J), J=1,5)
        LVMAT = LVMAT + 11
  100 CONTINUE  
C----------------------------------------------------------------------
   30 FORMAT (/,' **** Vertex Chamber materials bank VMAT ****',/,
     &  ' **** Number of materials: ',I2,2X,'Words per material: ',I2,/,
     &  ' **** Material Characteristics:',/,
     &  1X,' NUMBER',10X,'NAME',11X,'<A>',3X,'<Z>',2X,
     &  'DENSITY(g/cm3)',2X,'RAD LNGTH',2X,'ABS LNGTH')
   40 FORMAT (4X,I2,4X,A20,2X,F5.2,2X,F3.1,7X,F4.2,9X,F4.1,7X,F4.1)
  999 RETURN
      END
