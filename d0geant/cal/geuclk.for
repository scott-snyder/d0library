      SUBROUTINE GEUCLK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sets Up Central calorimeter CH section.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  16-NOV-1985   Rajendran Raja
C-   Updated  13-SEP-1988   Rajendran Raja   
C-   Updated   2-DEC-1988   Stuart Fuess        Add floors using SRCP 
C-   Updated  11-JAN-1989   Stuart Fuess        Add Main Ring module 
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INTEGER ISEG,NSGCHU
      CHARACTER*32 CSTRNG
C----------------------------------------------------------------------
C  Get number of CH modules
C----------------------------------------------------------------------
      CALL GTSRCP('CCCH_NUMBER_MODULES',NSGCHU,1)
C----------------------------------------------------------------------
C  Position each CH module
C----------------------------------------------------------------------
      DO 300 ISEG=1,NSGCHU
        WRITE(CSTRNG,1000) ISEG
        CALL VOLPOS(CSTRNG)
  300 CONTINUE
C----------------------------------------------------------------------
C  Position floors and endplates within each normal module
C----------------------------------------------------------------------
      CALL VOLPOS('CCCH_FLOOR8_VOLUME')
      CALL VOLPOS('CCCH_SOUTH_ENDPLATE_VOLUME')
      CALL VOLPOS('CCCH_NORTH_ENDPLATE_VOLUME')
C----------------------------------------------------------------------
C  Position floors, endplates, cutout, and beam pipe within special 
C  main ring module
C----------------------------------------------------------------------
      CALL VOLPOS('CCCH_MR_FLOOR8_VOLUME')
      CALL VOLPOS('CCCH_MR_SOUTH_ENDPLATE')
      CALL VOLPOS('CCCH_MR_NORTH_ENDPLATE')
      CALL VOLPOS('CCCH_MR_CUTOUT_VOLUME')
      CALL VOLPOS('CCCH_MR_BEAMPIPE_VOLUME')
      RETURN
 1000 FORMAT('CCCH_',I2.2,'_VOLUME')
      END
