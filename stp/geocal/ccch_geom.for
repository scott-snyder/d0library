      SUBROUTINE CCCH_GEOM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute CCCH geometry for GEANT
C-
C-      This is the driver routine for the preparation of the
C-      GEANT geometry for the Coarse Hadronic section of the
C-      D0 Central Calorimeter
C-
C-         ____________________________________
C- --------   ______________________________   ---------
C-            | __________________________ |
C-            | |                        | |
C-            | |                        | |         CCCH
C-            | |                        | <----- Module volume
C-            | |                        | |
C-            | |                        | |
C-             | |                      | |          CCCH
C-             | |                      | |      Mother volume
C-             | |       Floor 8        | |
C-             | |                      | |
C-              | |                    | |
C-              | |                    | |
C-              | |                    | |
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
C-   Created  27-OCT-1988   Stuart Fuess
C-   Updated   6-JAN-1989   Stuart Fuess  Add front and back plates 
C-   Updated   9-JAN-1989   Stuart Fuess  Simplify structure, 
C-                                        add Main Ring
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
C  CCCH Mother volume
C       The mother volumes will be a 'PCON' with inner radius given 
C       by the FH/CH boundary, outer radius given by the cryostat outer 
C       cold wall inner radius, length given by the CH module lengths, 
C       and filled with liquid argon.
C----------------------------------------------------------------------
      CALL CCCH_MOTHER
C----------------------------------------------------------------------
C  Set CCCH module rotation matrices
C----------------------------------------------------------------------
      CALL CCCH_ROTATION_MATRICES
C----------------------------------------------------------------------
C  CCCH Module volumes
C       Determine the dimensions and positioning of the CCCH module
C       volumes in which will be positioned Floor and Endplate volumes.  
C----------------------------------------------------------------------
      CALL CCCH_MODULE
C----------------------------------------------------------------------
C  Zero accumulation of CCCH crack material
C----------------------------------------------------------------------
      CALL CCCH_CRACK('ZERO')
C----------------------------------------------------------------------
C  Get number of CCCH floors and first floor number
C----------------------------------------------------------------------
      CALL EZGET('CCCH_NUMBER_FLOORS',NFLOORS,IER)
      CALL EZGET('CCCH_FIRST_FLOOR',FIRST_FLOOR,IER)
      LAST_FLOOR = FIRST_FLOOR + NFLOORS - 1
C----------------------------------------------------------------------
C  Loop over CCCH floors (only 1 floor for CCCH)
C       Each floor of an CH module will be a 'TRD2' with lateral 
C       dimensions such as to enclose the resitive coat sections of 
C       the readout boards, and inner and outer surfaces determined by 
C       the active regions of each floor. The volume is filled with a 
C       mixture representing the relative components.  The material 
C       from the module elements which lie outside of the floor volume 
C       are accumulated as contributing to the 'crack' mixture.  
C----------------------------------------------------------------------
      DO FLOOR=FIRST_FLOOR,LAST_FLOOR
        CALL CCCH_FLOOR(FLOOR)
        CALL CCCH_CRACK('FLOOR')
        CALL CCCH_CELLS(FLOOR)
      ENDDO
C----------------------------------------------------------------------
C  CCCH endplate volumes
C       Each endplate (South=+, North=-) of an CH module will be a 
C       'TRAP', with the dimensions of the endplates and filled as a 
C       solid material.  Subtract the endplate volumes from the crack
C       volume.
C----------------------------------------------------------------------
      CALL CCCH_ENDPLATE
      CALL CCCH_CRACK('ENDPLATE')
C----------------------------------------------------------------------
C  CCCH Front plate volume
C       Subtract the front plate volume from the crack volume.
C----------------------------------------------------------------------
      CALL CCCH_FRONTPLATE
      CALL CCCH_CRACK('FRONTPLATE')
C----------------------------------------------------------------------
C  CCCH Back plate volume
C       Subtract the back plate volume from the crack volume.
C----------------------------------------------------------------------
      CALL CCCH_BACKPLATE
      CALL CCCH_CRACK('BACKPLATE')
C----------------------------------------------------------------------
C  Get contribution to crack material from module skin
C----------------------------------------------------------------------
      CALL CCCH_CRACK('SKIN')
C----------------------------------------------------------------------
C  Determine Crack material from accumulated sums
C----------------------------------------------------------------------
      CALL CCCH_CRACK('MATERIAL')
C----------------------------------------------------------------------
C  CCCH special Main Ring module cutout volume
C----------------------------------------------------------------------
      CALL CCCH_MR_CUTOUT
C----------------------------------------------------------------------
C  CCCH Main Ring beampipe volume
C----------------------------------------------------------------------
      CALL CCCH_MR_BEAMPIPE
      RETURN
      END
