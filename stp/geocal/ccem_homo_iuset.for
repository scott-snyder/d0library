      SUBROUTINE CCEM_HOMO_IUSET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CCEM Detector Sets using homogenized volumes
C-
C-      Create the SRCP structures which define the Central Calorimeter
C-      EM Detector Sets using homogenized volumes
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  01-DEC-1989   Stuart Fuess
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
C  CCEM
C----------------------------------------------------------------------
      CALL EZGETS ( 'IUSET_CCEM_HOMO_LABEL',1,IUSET_LABEL,LEN,IER )
      CALL EZGETS ( 'IUSET_CCEM_HOMO_NAME',1,IUSET_NAME,LEN,IER )
      IUSET_NV = 6
C----------------------------------------------------------------------
C  CCEM Homogenized Module Shell
C----------------------------------------------------------------------
      CALL EZGETS ( 'CCEM_HOMO_MODULE_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(1),LEN,IER )
      CALL EZGET ( 'IUSET_CCEM_HOMO_MODULE_IDTYPE', 
     &              IUSET_IDTYPE(1), IER )
C----------------------------------------------------------------------
C  CCEM Floor 1
C----------------------------------------------------------------------
      CALL EZGETS ( 'CCEM_FLOOR1_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(2),LEN,IER )
      CALL EZGET ( 'IUSET_CCEM_FLOOR1_IDTYPE', IUSET_IDTYPE(2), IER )
C----------------------------------------------------------------------
C  CCEM Floor 2
C----------------------------------------------------------------------
      CALL EZGETS ( 'CCEM_FLOOR2_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(3),LEN,IER )
      CALL EZGET ( 'IUSET_CCEM_FLOOR2_IDTYPE', IUSET_IDTYPE(3), IER )
C----------------------------------------------------------------------
C  CCEM Floor 3
C----------------------------------------------------------------------
      CALL EZGETS ( 'CCEM_FLOOR3_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(4),LEN,IER )
      CALL EZGET ( 'IUSET_CCEM_FLOOR3_IDTYPE', IUSET_IDTYPE(4), IER )
C----------------------------------------------------------------------
C  CCEM Floor 4
C----------------------------------------------------------------------
      CALL EZGETS ( 'CCEM_FLOOR4_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(5),LEN,IER )
      CALL EZGET ( 'IUSET_CCEM_FLOOR4_IDTYPE', IUSET_IDTYPE(5), IER )
C----------------------------------------------------------------------
C  CCEM Frontplate
C----------------------------------------------------------------------
      CALL EZGETS ( 'CCEM_FRONTPLATE_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(6),LEN,IER )
      CALL EZGET ( 'IUSET_CCEM_FRONTPLATE_IDTYPE', 
     &              IUSET_IDTYPE(6), IER )
      CALL WRITE_IUSET
      RETURN
      END
