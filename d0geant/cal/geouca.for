      SUBROUTINE GEOUCA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Central Calorimeter Geometry Steering Routine
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  04-DEC-1985   Rajendran Raja
C-   Updated  13-SEP-1988   Rajendran Raja
C-
C----------------------------------------------------------------------
C
C  units are centimeters
C  rectangular, right handed coordinate system with beam along z and y  up.
C
      IMPLICIT NONE
C
      INTEGER IVOLU
C----------------------------------------------------------------------
C Mother Volumes
      CALL VOLPOS('CC_EM_MOTHER_VOLUME')
      CALL VOLPOS('CC_FH_MOTHER_VOLUME')
      CALL VOLPOS('CC_CH_MOTHER_VOLUME')
C
      CALL GEUCEL          !Set up UC ELectromagnetic section
      CALL GEUCFH          !Set up UC Fine Hadronic section
      CALL GEUCLK          !Set up UC LeaKage section
C
      CALL GEUMSG          !Set up massless gaps
C
      CALL VOLORD('CC_EM_MOTHER_VOLUME',6)
C Ordering the contents of UCEM in Phi.
      CALL VOLORD('CC_FH_MOTHER_VOLUME',6)
C Ordering the contents of UCFH in Phi.
      CALL VOLORD('CC_CH_MOTHER_VOLUME',6)
C Ordering the contents of UCCH in Phi.
C
      RETURN
      END
