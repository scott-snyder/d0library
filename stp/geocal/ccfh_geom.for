      SUBROUTINE CCFH_GEOM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute CCFH geometry for GEANT
C-
C-      This is the driver routine for the preparation of the
C-      GEANT geometry for the Fine Hadronic section of the
C-      D0 Central Calorimeter
C-
C-         ____________________________________
C- --------   ______________________________   ---------
C-            | __________________________ |
C-            | |                        | |
C-            | |                        | |         CCFH
C-            | |        Floor 7         | <----- Module volume
C-             | |                      | |
C-             | |                      | |
C-             | ------------------------ |
C-             | |                      | |
C-             | |                      | |
C-             | |       Floor 6        | |          CCFH
C-              | |                    | |       Mother volume
C-              | |                    | |
C-              |  --------------------  |
C-              | |                    | |
C-              | |      Floor 5       | |
C-               | |                  | |
C-               | |__________________| |
C-               |______________________|
C-            ______________________________
C-    --------                              --------
C-
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  22-NOV-1988   Stuart Fuess
C-   Updated   6-JAN-1989   Stuart Fuess  Add front and back plates 
C-   Updated  13-JAN-1989   Stuart Fuess  Simplify structure 
C-   Updated  16-JAN-1989   Stuart Fuess  Add cell division info 
C-   Updated   4-FEB-1990   Stuart Fuess  New EZ routines; more general
C-                                        common blocks 
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
C  CCFH Mother volume
C       The mother volume will be a 'TUBE' with inner radius given by 
C       the EM/FH boundary, outer radius given by the FH/CH boundary, 
C       length given by the FH module lengths, and filled with liquid 
C       argon.
C----------------------------------------------------------------------
      CALL CCFH_MOTHER
C----------------------------------------------------------------------
C  Set CCFH module rotation matrices
C----------------------------------------------------------------------
      CALL CCFH_ROTATION_MATRICES
C----------------------------------------------------------------------
C  CCFH Module volume
C       Determine the dimensions and positioning of the CCFH module
C       volume in which will be positioned Floor and Endplate volumes.
C----------------------------------------------------------------------
      CALL CCFH_MODULE
C----------------------------------------------------------------------
C  Zero accumulation of CCFH crack material
C----------------------------------------------------------------------
      CALL CCFH_CRACK('ZERO')
C----------------------------------------------------------------------
C  Get number of CCFH floors and first floor number
C----------------------------------------------------------------------
      CALL EZGET('CCFH_NUMBER_FLOORS',NFLOORS,IER)
      CALL EZGET('CCFH_FIRST_FLOOR',FIRST_FLOOR,IER)
      LAST_FLOOR = FIRST_FLOOR + NFLOORS - 1
C----------------------------------------------------------------------
C  Loop over CCFH floors
C       Each floor of an FH module will be a 'TRD1' with lateral 
C       dimensions such as to enclose the resitive coat sections of the
C       readout boards, and inner and outer surfaces determined by the 
C       active regions of each floor. The volume is filled with a 
C       mixture representing the relative components.  The material 
C       from the module elements which lie outside of the floor volume 
C       are accumulated as contributing to the 'crack' mixture.  
C----------------------------------------------------------------------
      DO FLOOR=FIRST_FLOOR,LAST_FLOOR
        CALL CCFH_FLOOR(FLOOR)
        CALL CCFH_CRACK('FLOOR')
        CALL CCFH_CELLS(FLOOR)
      ENDDO
C----------------------------------------------------------------------
C  CCFH endplate volumes
C       Each endplate (South=+, North=-) of an FH module will be a 
C       'TRD1', with the dimensions of the endplates and filled as a 
C       solid material.  Subtract the endplate volumes from the crack
C       volume.
C----------------------------------------------------------------------
      CALL CCFH_ENDPLATE
      CALL CCFH_CRACK('ENDPLATE')
C----------------------------------------------------------------------
C  CCFH Front plate volume
C       Subtract the front plate volume from the crack volume.
C----------------------------------------------------------------------
      CALL CCFH_FRONTPLATE
      CALL CCFH_CRACK('FRONTPLATE')
C----------------------------------------------------------------------
C  CCFH Back plate volume
C       Subtract the back plate volume from the crack volume.
C----------------------------------------------------------------------
      CALL CCFH_BACKPLATE
      CALL CCFH_CRACK('BACKPLATE')
C----------------------------------------------------------------------
C  Get contribution to crack material from module skin
C----------------------------------------------------------------------
      CALL CCFH_CRACK('SKIN')
C----------------------------------------------------------------------
C  Determine Crack material from accumulated sums
C----------------------------------------------------------------------
      CALL CCFH_CRACK('MATERIAL')
      RETURN
      END
