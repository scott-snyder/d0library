      SUBROUTINE GEUCEL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sets Up Central calorimeter EM section.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  16-NOV-1985   Rajendran Raja
C-   Updated  13-SEP-1988   Rajendran Raja
C-   Updated   2-DEC-1988   Stuart Fuess        Add floors using SRCP
C-   Updated  12-DEC-1988   Stuart Fuess        Add CCEM front plate
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INTEGER ISEG,NSGELU
      CHARACTER*32 CSTRNG
C----------------------------------------------------------------------
C  Get number of EM modules
C----------------------------------------------------------------------
      CALL GTSRCP('CCEM_NUMBER_MODULES',NSGELU,1)
C----------------------------------------------------------------------
C  Position each EM module
C----------------------------------------------------------------------
      DO 300 ISEG=1,NSGELU
        WRITE(CSTRNG,1000) ISEG
        CALL VOLPOS(CSTRNG)
  300 CONTINUE

C----------------------------------------------------------------------
C  Position Front plate of CCEM module
C----------------------------------------------------------------------
      CALL VOLPOS('CCEM_FRONTPLATE_VOLUME')
C----------------------------------------------------------------------
C  Position floors and endplates within each module
C----------------------------------------------------------------------
      CALL VOLPOS('CCEM_FLOOR1_VOLUME')
      CALL VOLPOS('CCEM_FLOOR2_VOLUME')
      CALL VOLPOS('CCEM_FLOOR3_VOLUME')
      CALL VOLPOS('CCEM_FLOOR4_VOLUME')
      CALL VOLPOS('CCEM_SOUTH_ENDPLATE_VOLUME')
      CALL VOLPOS('CCEM_NORTH_ENDPLATE_VOLUME')
      RETURN
 1000 FORMAT('CCEM_',I2.2,'_VOLUME')
      END
