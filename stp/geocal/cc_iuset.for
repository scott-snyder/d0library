      SUBROUTINE CC_IUSET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CC Detector Sets
C-
C-      Create the SRCP structures which define the Central Calorimeter
C-      Detector Sets.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   8-DEC-1988   Stuart Fuess
C-   Updated  27-DEC-1988   Stuart Fuess  Better names, CCEM frontplate 
C-   Updated  28-DEC-1988   Stuart Fuess  IDTYPE for each volume 
C-   Updated  11-JAN-1989   Stuart Fuess  Add CCCH main ring module 
C-   Updated  19-JAN-1989   Stuart Fuess  Add Mother volumes in crack 
C-   Updated   4-FEB-1990   Stuart Fuess  New EZ routines; more general
C-                                        common blocks 
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
C  CCEM
C----------------------------------------------------------------------
      CALL EZGETS ('IUSET_CCEM_LABEL',1,IUSET_LABEL,LEN,IER)
      CALL EZGETS ('IUSET_CCEM_NAME',1,IUSET_NAME,LEN,IER)
      IUSET_NV = 4
C----------------------------------------------------------------------
C  CCEM Floor 1
C----------------------------------------------------------------------
      CALL EZGETS ('CCEM_FLOOR1_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(1),LEN,IER)
      CALL EZGET ('IUSET_CCEM_FLOOR1_IDTYPE',IUSET_IDTYPE(1),IER)
C----------------------------------------------------------------------
C  CCEM Floor 2
C----------------------------------------------------------------------
      CALL EZGETS ('CCEM_FLOOR2_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(2),LEN,IER)
      CALL EZGET ('IUSET_CCEM_FLOOR2_IDTYPE',IUSET_IDTYPE(2),IER)
C----------------------------------------------------------------------
C  CCEM Floor 3
C----------------------------------------------------------------------
      CALL EZGETS ('CCEM_FLOOR3_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(3),LEN,IER)
      CALL EZGET ('IUSET_CCEM_FLOOR3_IDTYPE',IUSET_IDTYPE(3),IER)
C----------------------------------------------------------------------
C  CCEM Floor 4
C----------------------------------------------------------------------
      CALL EZGETS ('CCEM_FLOOR4_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(4),LEN,IER)
      CALL EZGET ('IUSET_CCEM_FLOOR4_IDTYPE',IUSET_IDTYPE(4),IER)
      CALL WRITE_IUSET
C----------------------------------------------------------------------
C  CCFH
C----------------------------------------------------------------------
      CALL EZGETS ('IUSET_CCFH_LABEL',1,IUSET_LABEL,LEN,IER)
      CALL EZGETS ('IUSET_CCFH_NAME',1,IUSET_NAME,LEN,IER)
      IUSET_NV = 3
C----------------------------------------------------------------------
C  CCFH Floor 5
C----------------------------------------------------------------------
      CALL EZGETS ('CCFH_FLOOR5_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(1),LEN,IER)
      CALL EZGET ('IUSET_CCFH_FLOOR5_IDTYPE',IUSET_IDTYPE(1),IER)
C----------------------------------------------------------------------
C  CCFH Floor 6
C----------------------------------------------------------------------
      CALL EZGETS ('CCFH_FLOOR6_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(2),LEN,IER)
      CALL EZGET ('IUSET_CCFH_FLOOR6_IDTYPE',IUSET_IDTYPE(2),IER)
C----------------------------------------------------------------------
C  CCFH Floor 7
C----------------------------------------------------------------------
      CALL EZGETS ('CCFH_FLOOR7_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(3),LEN,IER)
      CALL EZGET ('IUSET_CCFH_FLOOR7_IDTYPE',IUSET_IDTYPE(3),IER)
      CALL WRITE_IUSET
C----------------------------------------------------------------------
C  CCCH
C----------------------------------------------------------------------
      CALL EZGETS ('IUSET_CCCH_LABEL',1,IUSET_LABEL,LEN,IER)
      CALL EZGETS ('IUSET_CCCH_NAME',1,IUSET_NAME,LEN,IER)
      CALL EZGET ('IUSET_CCCH_FLOOR8_IDTYPE',IDTYPE,IER)
      IUSET_NV = 2
C----------------------------------------------------------------------
C  CCCH Floor 8
C----------------------------------------------------------------------
      CALL EZGETS ('CCCH_FLOOR8_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(1),LEN,IER)
      IUSET_IDTYPE(1) = IDTYPE
      CALL EZGETS ('CCCH_MR_FLOOR8_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(2),LEN,IER)
      IUSET_IDTYPE(2) = IDTYPE
      CALL WRITE_IUSET
C----------------------------------------------------------------------
C  CC Massless Gaps
C----------------------------------------------------------------------
      CALL EZGETS ('IUSET_CC_MASSLESS_GAP_LABEL',1,IUSET_LABEL,LEN,IER)
      CALL EZGETS ('IUSET_CC_MASSLESS_GAP_NAME',1,IUSET_NAME,LEN,IER)
      CALL EZGET ('IUSET_CC_MASSLESS_GAP_IDTYPE',IDTYPE,IER)
      IUSET_NV = 2
      CALL EZGETS ('CC_SOUTH_MASSLESS_GAP_NAME',1,
     &              IUSET_VOLUME_NAME(1),LEN,IER)
      IUSET_IDTYPE(1) = IDTYPE
      CALL EZGETS ('CC_NORTH_MASSLESS_GAP_NAME',1,
     &              IUSET_VOLUME_NAME(2),LEN,IER)
      IUSET_IDTYPE(2) = IDTYPE
      CALL WRITE_IUSET
C----------------------------------------------------------------------
C  CC Crack Material, including Mother volumes and CCCH main ring 
C  module cutout and pipe
C----------------------------------------------------------------------
      CALL EZGETS ('IUSET_CC_CRACKS_LABEL',1,IUSET_LABEL,LEN,IER)
      CALL EZGETS ('IUSET_CC_CRACKS_NAME',1,IUSET_NAME,LEN,IER)
      CALL EZGET ('IUSET_CC_CRACKS_IDTYPE',IDTYPE,IER)
      IUSET_NV = 9
      CALL EZGETS ('CCEM_MOTHER_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(1),LEN,IER)
      IUSET_IDTYPE(1) = IDTYPE
      CALL EZGETS ('CCEM_MODULE_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(2),LEN,IER)
      IUSET_IDTYPE(2) = IDTYPE
      CALL EZGETS ('CCFH_MOTHER_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(3),LEN,IER)
      IUSET_IDTYPE(3) = IDTYPE
      CALL EZGETS ('CCFH_MODULE_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(4),LEN,IER)
      IUSET_IDTYPE(4) = IDTYPE
      CALL EZGETS ('CCCH_MOTHER_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(5),LEN,IER)
      IUSET_IDTYPE(5) = IDTYPE
      CALL EZGETS ('CCCH_MODULE_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(6),LEN,IER)
      IUSET_IDTYPE(6) = IDTYPE
      CALL EZGETS ('CCCH_MR_MODULE_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(7),LEN,IER)
      IUSET_IDTYPE(7) = IDTYPE
      CALL EZGETS ('CCCH_MR_CUTOUT_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(8),LEN,IER)
      IUSET_IDTYPE(8) = IDTYPE
      CALL EZGETS ('CCCH_MR_BEAMPIPE_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(9),LEN,IER)
      IUSET_IDTYPE(9) = IDTYPE
      CALL WRITE_IUSET
C----------------------------------------------------------------------
C  CC Endplates and CCEM Frontplate
C----------------------------------------------------------------------
      CALL EZGETS ('IUSET_CC_ENDPLATES_LABEL',1,IUSET_LABEL,LEN,IER)
      CALL EZGETS ('IUSET_CC_ENDPLATES_NAME',1,IUSET_NAME,LEN,IER)
      CALL EZGET ('IUSET_CC_ENDPLATES_IDTYPE',IDTYPE,IER)
      IUSET_NV = 9
      CALL EZGETS ('CCEM_SOUTH_ENDPLATE_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(1),LEN,IER)
      IUSET_IDTYPE(1) = IDTYPE
      CALL EZGETS ('CCEM_NORTH_ENDPLATE_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(2),LEN,IER)
      IUSET_IDTYPE(2) = IDTYPE
      CALL EZGETS ('CCFH_SOUTH_ENDPLATE_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(3),LEN,IER)
      IUSET_IDTYPE(3) = IDTYPE
      CALL EZGETS ('CCFH_NORTH_ENDPLATE_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(4),LEN,IER)
      IUSET_IDTYPE(4) = IDTYPE
      CALL EZGETS ('CCCH_SOUTH_ENDPLATE_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(5),LEN,IER)
      IUSET_IDTYPE(5) = IDTYPE
      CALL EZGETS ('CCCH_NORTH_ENDPLATE_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(6),LEN,IER)
      IUSET_IDTYPE(6) = IDTYPE
      CALL EZGETS ('CCCH_MR_SOUTH_ENDPLATE_NAME',1,
     &              IUSET_VOLUME_NAME(7),LEN,IER)
      IUSET_IDTYPE(7) = IDTYPE
      CALL EZGETS ('CCCH_MR_NORTH_ENDPLATE_NAME',1,
     &              IUSET_VOLUME_NAME(8),LEN,IER)
      IUSET_IDTYPE(8) = IDTYPE
      CALL EZGETS ('CCEM_FRONTPLATE_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(9),LEN,IER)
      IUSET_IDTYPE(9) = IDTYPE
      CALL WRITE_IUSET
      RETURN
      END
