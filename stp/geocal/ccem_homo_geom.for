      SUBROUTINE CCEM_HOMO_GEOM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute Homogenized CCEM geometry for GEANT
C-
C-            ______________________________
C-            | __________________________ |
C-            | |                        | |
C-            | |                        | |      CCEM homogenized
C-            | |        Floor 4         | <-----  Module volume
C-            | |                        | |        filled with
C-            |  ------------------------  |       crack material
C-             | |                      | |
C-             | |       Floor 3        | |
C-             | |                      | |
C-             |  ----------------------  |     + endplates not shown
C-              | |                    | |      + frontplate not shown
C-              | |      Floor 2       | |
C-              |  --------------------  |
C-               | |                  | |
C-               | |_____Floor 1______| |
C-               |______________________|
C-
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  09-OCT-1989   Stuart Fuess
C-   Updated   4-DEC-1989   Stuart Fuess  Remove CALL CCEM_ENDPLATE 
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
C  Select the CC SRCP file
C  Used both for EZGETs in this routine and in the CCEM_ routines
C----------------------------------------------------------------------
      CALL EZPICK ( 'CENTRAL' )
C----------------------------------------------------------------------
C  Zero accumulation of CCEM crack material
C----------------------------------------------------------------------
      CALL CCEM_CRACK('ZERO')
C----------------------------------------------------------------------
C  Get number of CCEM floors and first floor number
C----------------------------------------------------------------------
      CALL EZGET ( 'CCEM_NUMBER_FLOORS', NFLOORS, IER )
      CALL EZGET ( 'CCEM_FIRST_FLOOR', FIRST_FLOOR, IER )
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
C       solid material.  The endplate volumes are already created
C       externally.  Subtract the endplate volumes from the crack
C       volume.
C----------------------------------------------------------------------
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
C       There is no distinct volume created for the backplate, but the
C       size must be computed for the crack calculation.
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
