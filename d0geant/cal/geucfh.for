      SUBROUTINE GEUCFH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sets Up Central calorimeter FH section.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  16-NOV-1985   Rajendran Raja
C-   Updated  13-SEP-1988   Rajendran Raja   
C-   Updated   2-DEC-1988   Stuart Fuess        Add floors using SRCP 
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INTEGER ISEG,NSGFHU
      CHARACTER*32 CSTRNG
C----------------------------------------------------------------------
C  Get number of FH modules
C----------------------------------------------------------------------
      CALL GTSRCP('CCFH_NUMBER_MODULES',NSGFHU,1)
C----------------------------------------------------------------------
C  Position each FH module
C----------------------------------------------------------------------
      DO 300 ISEG=1,NSGFHU
        WRITE(CSTRNG,1000) ISEG
        CALL VOLPOS(CSTRNG)
  300 CONTINUE
C----------------------------------------------------------------------
C  Position floors and endplates within each module
C----------------------------------------------------------------------
      CALL VOLPOS('CCFH_FLOOR5_VOLUME')
      CALL VOLPOS('CCFH_FLOOR6_VOLUME')
      CALL VOLPOS('CCFH_FLOOR7_VOLUME')
      CALL VOLPOS('CCFH_SOUTH_ENDPLATE_VOLUME')
      CALL VOLPOS('CCFH_NORTH_ENDPLATE_VOLUME')
      RETURN
 1000 FORMAT('CCFH_',I2.2,'_VOLUME')
      END
