      SUBROUTINE PRVWAL (PRUNIT, KVWAL, NVWAL, CFL, IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : print bank VWAL:  design values for passive
C-                         materials and GEANT volumes
C-
C-   Inputs  :  PRUNIT: unit number for printout
C-              KVWAL : bank address
C-              NVWAL : bank number - not used
C-              CFL   : how many banks to print - not used
C-              IFL   : flag controlling type of printout - not used
C-
C-   Created  20-OCT-1988   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER PRUNIT, KVWAL, LVWAL, NVWAL, IFL
      CHARACTER CFL*(*)
      INTEGER NBVOL, NWVOL, IVOL, J
      CHARACTER*4 VOLNAM, VOLMOT
C----------------------------------------------------------------------
C       Does bank exist?
C----------------------------------------------------------------------
      LVWAL = KVWAL
      IF ( LVWAL .LE. 0 ) THEN
        WRITE ( PRUNIT, 900 )
  900   FORMAT (/,10X,' Routine PRVWAL: ',
     &    '******** The bank VWAL does not exist ********')
        GO TO 999
      ENDIF
C----------------------------------------------------------------------
C       Print out bank
C----------------------------------------------------------------------
C       Print header, numbers of volumes, parameters/volume, table
      NBVOL = IC(LVWAL+1)
      NWVOL = IC(LVWAL+2)
      WRITE ( PRUNIT, 30 ) NBVOL, NWVOL 
C       Print volume parameters
      LVWAL = LVWAL + 2
      DO 910 IVOL = 1, NBVOL
        CALL UHTOC(C(LVWAL+1), 4, VOLNAM, 4)
        CALL UHTOC(C(LVWAL+7), 4, VOLMOT, 4)
        WRITE ( PRUNIT, 40 ) VOLNAM, (IC(LVWAL+1+J), J=1,5), VOLMOT
        LVWAL = LVWAL + 7
  910 CONTINUE
      WRITE ( PRUNIT, 50 )
C----------------------------------------------------------------------
   30 FORMAT (/,' **** Vertex Chamber GEANT volume design value bank ',
     &  'VWAL ****',/,
     &  ' **** Number of volumes: ',2X,I2,2X,' Parameters/volume: ',
     &  2X,I2,/,
     &  ' **** Volume Parameters:',/,
     &  '   VOLUME',5X,'OFFSET IN BANK VGEH:',5X,'MATERIAL',4X,
     &  'MOTHER',/,
     &  '    NAME',5X,'Rmin',2X,'Rmax',2X,'Zmin',2X,'Zmax',5X,
     &  'NUMBER',5X,'VOLUME')
   40 FORMAT (4X,A4,6X,I2,4X,I2,4X,I2,4X,I2,8X,I2,8X,A4)
   50 FORMAT (' **** If Zmin=0, there is one volume symmetric around',
     &  ' Z=0',/,
     &  ' **** If Zmin>0, there are two copies of the volume symmetric',
     &  ' around Z=0')
  999 RETURN
      END
