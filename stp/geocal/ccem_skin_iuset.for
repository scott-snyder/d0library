      SUBROUTINE CCEM_SKIN_IUSET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CCEM Side Skin Detector Sets 
C-
C-      Create the SRCP structures which define the Central Calorimeter
C-      EM Side Skin Detector Sets
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  14-DEC-1989   Stuart Fuess
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:IUSET.INC'
C  Integers
      INTEGER IER
      INTEGER LEN
      INTEGER IDTYPE
C----------------------------------------------------------------------
C  Select the CC SRCP file
C----------------------------------------------------------------------
      CALL EZPICK ( 'CENTRAL' )
C----------------------------------------------------------------------
C  CC Endplates
C----------------------------------------------------------------------
      CALL EZGETS ( 'IUSET_CC_SKIN_LABEL',1,IUSET_LABEL,LEN,IER )
      CALL EZGETS ( 'IUSET_CC_SKIN_NAME',1,IUSET_NAME,LEN,IER )
      CALL EZGET ( 'IUSET_CC_SKIN_IDTYPE', IDTYPE, IER )
      IUSET_NV = 2
      CALL EZGETS ( 'CCEM_LEFT_SKIN_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(1),LEN,IER )
      IUSET_IDTYPE(1) = IDTYPE
      CALL EZGETS ( 'CCEM_RIGHT_SKIN_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(2),LEN,IER )
      IUSET_IDTYPE(2) = IDTYPE
      CALL WRITE_IUSET
      RETURN
      END
