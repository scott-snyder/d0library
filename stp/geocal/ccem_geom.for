      SUBROUTINE CCEM_GEOM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute CCEM geometry for GEANT
C-
C-      This is the driver routine for the preparation of the
C-      GEANT geometry for the ElectroMagnetic section of the
C-      D0 Central Calorimeter
C-
C-         ____________________________________
C- --------   ______________________________   ---------
C-            | __________________________ |
C-            | |                        | |
C-            | |                        | |         CCEM
C-            | |        Floor 4         | <----- Module volume
C-            | |                        | |
C-            |  ------------------------  |
C-             | |                      | |          CCEM
C-             | |       Floor 3        | |      Mother volume
C-             | |                      | |
C-             |  ----------------------  |
C-              | |                    | |
C-              | |      Floor 2       | |
C-              |  --------------------  |
C-               | |                  | |
C-               | |_____Floor 1______| |
C-               |______________________|
C-            ______________________________
C-    --------                              --------
C-
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  27-OCT-1988   Stuart Fuess
C-   Updated   6-JAN-1989   Stuart Fuess  Add backplate correction 
C-   Updated  13-JAN-1989   Stuart Fuess  Simplify structure 
C-   Updated  15-JAN-1989   Stuart Fuess  Add cell division info 
C-   Updated   4-FEB-1990   Stuart Fuess  Use EZ routines 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Integers
      INTEGER IER
      INTEGER FLOOR
      INTEGER NFLOORS
      INTEGER FIRST_FLOOR
      INTEGER LAST_FLOOR
C----------------------------------------------------------------------
C  CCEM Mother volume
C       The mother volume will be a 'TUBE' with inner radius given 
C       by the outer radius of the cryostat inner cold wall, outer 
C       radius given by the EM/FH boundary, length given by the EM 
C       module lengths, and filled with liquid argon.
C----------------------------------------------------------------------
      CALL CCEM_MOTHER
C----------------------------------------------------------------------
C  Set CCEM module rotation matrices
C----------------------------------------------------------------------
      CALL CCEM_ROTATION_MATRICES
C----------------------------------------------------------------------
C  CCEM Module volume
C       Determine the dimensions and positioning of the CCEM module
C       volume in which will be positioned Floor and Endplate volumes.
C----------------------------------------------------------------------
      CALL CCEM_MODULE
C----------------------------------------------------------------------
C  Zero accumulation of CCEM crack material
C----------------------------------------------------------------------
      CALL CCEM_CRACK('ZERO')
C----------------------------------------------------------------------
C  Get number of CCEM floors and first floor number
C----------------------------------------------------------------------
      CALL EZGET('CCEM_NUMBER_FLOORS',NFLOORS,IER)
      CALL EZGET('CCEM_FIRST_FLOOR',FIRST_FLOOR,IER)
      LAST_FLOOR = FIRST_FLOOR + NFLOORS - 1
C----------------------------------------------------------------------
C  Loop over CCEM floors
C       Each floor of an EM module will be a 'TRD1' with lateral 
C       dimensions such as to enclose the resitive coat sections of 
C       the readout boards, and inner and outer surfaces determined 
C       by the active regions of each floor. The volume is filled with 
C       a mixture representing the relative components.  The material 
C       from the module elements which lie outside of the floor volume 
C       are accumulated as contributing to the 'crack' mixture.
C----------------------------------------------------------------------
      DO FLOOR=FIRST_FLOOR,LAST_FLOOR
        CALL CCEM_FLOOR(FLOOR)
        CALL CCEM_CRACK('FLOOR')
        CALL CCEM_CELLS(FLOOR)
      ENDDO
C----------------------------------------------------------------------
C  CCEM endplate volumes
C       Each endplate (South=+, North=-) of an EM module will be a 
C       'TRD1', with the dimensions of the endplates and filled as a 
C       solid material.  Subtract the endplate volumes from the crack
C       volume.
C----------------------------------------------------------------------
      CALL CCEM_ENDPLATE
      CALL CCEM_CRACK('ENDPLATE')
C----------------------------------------------------------------------
C  CCEM Front plate volume
C       The Front plate of an EM module will be a 'BOX' with the
C       dimensions of the front plate and filled as a solid material.
C       Subtract the front plate volume from the crack volume.
C----------------------------------------------------------------------
      CALL CCEM_FRONTPLATE
      CALL CCEM_CRACK('FRONTPLATE')
C----------------------------------------------------------------------
C  CCEM Back plate volume
C       Subtract the back plate volume from the crack volume.
C----------------------------------------------------------------------
      CALL CCEM_BACKPLATE
      CALL CCEM_CRACK('BACKPLATE')
C----------------------------------------------------------------------
C  Get contribution to crack material from module skin
C----------------------------------------------------------------------
      CALL CCEM_CRACK('SKIN')
C----------------------------------------------------------------------
C  Determine Crack material from accumulated sums
C----------------------------------------------------------------------
      CALL CCEM_CRACK('MATERIAL')
      RETURN
      END
