      SUBROUTINE CCEM_PLATE_IUSET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CCEM Detector Sets using plate-level volumes
C-
C-      Create the SRCP structures which define the Central Calorimeter
C-      EM Detector Sets using plate-level volumes
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
      INTEGER INDEX
      INTEGER ACTIVE_IDTYPE
      INTEGER INACTIVE_IDTYPE
      INTEGER FLOOR
      INTEGER FIRST_FLOOR
      INTEGER LAST_FLOOR
      INTEGER ELEMENT
      INTEGER FIRST_ELEMENT
      INTEGER LAST_ELEMENT
C  Characters
      CHARACTER*4  ELEMENT_NAME
      CHARACTER*4  PREVIOUS_ELEMENT_NAME
      CHARACTER*32 NAME
C----------------------------------------------------------------------
C  Select the CC SRCP file
C----------------------------------------------------------------------
      CALL EZPICK ( 'CENTRAL' )
C----------------------------------------------------------------------
C  CCEM
C----------------------------------------------------------------------
      CALL EZGETS ( 'IUSET_CCEM_PLATE_LABEL',1,IUSET_LABEL,LEN,IER )
      CALL EZGETS ( 'IUSET_CCEM_PLATE_NAME',1,IUSET_NAME,LEN,IER )
C----------------------------------------------------------------------
C  CCEM Plate level Module Shell
C----------------------------------------------------------------------
      CALL EZGETS ( 'CCEM_PLATE_MODULE_VOLUME_NAME',1,
     &              IUSET_VOLUME_NAME(1),LEN,IER )
      CALL EZGET ( 'IUSET_CCEM_PLATE_MODULE_IDTYPE', 
     &              IUSET_IDTYPE(1), IER )
C----------------------------------------------------------------------
C  Get inactive elements IDTYPE
C----------------------------------------------------------------------
        WRITE(NAME,1001) FLOOR
        CALL EZGET ( 'IUSET_CCEM_INACTIVE_IDTYPE',INACTIVE_IDTYPE,IER )
C----------------------------------------------------------------------
C  CCEM plates:
C  Initialize index
C----------------------------------------------------------------------
      INDEX = 1
C----------------------------------------------------------------------
C  Initialize parameters of previous element
C----------------------------------------------------------------------
      PREVIOUS_ELEMENT_NAME = ' '
C----------------------------------------------------------------------
C  Get first and last floors in CCEM module
C----------------------------------------------------------------------
      CALL EZGET ( 'CCEM_FIRST_FLOOR', FIRST_FLOOR, IER )
      CALL EZGET ( 'CCEM_LAST_FLOOR', LAST_FLOOR, IER )
C----------------------------------------------------------------------
C  Loop over floors
C----------------------------------------------------------------------
      DO FLOOR=FIRST_FLOOR,LAST_FLOOR
C----------------------------------------------------------------------
C  Get IDTYPE for floor
C----------------------------------------------------------------------
        WRITE(NAME,1001) FLOOR
        CALL EZGET ( NAME, ACTIVE_IDTYPE, IER )
C----------------------------------------------------------------------
C  Get first and last element numbers in floor
C----------------------------------------------------------------------
        WRITE(NAME,1002) FLOOR
        CALL EZGET ( NAME, FIRST_ELEMENT, IER )
        WRITE(NAME,1003) FLOOR
        CALL EZGET ( NAME, LAST_ELEMENT, IER )
C----------------------------------------------------------------------
C  Loop over elements
C----------------------------------------------------------------------
        DO ELEMENT=FIRST_ELEMENT,LAST_ELEMENT
C----------------------------------------------------------------------
C  Encode element label
C----------------------------------------------------------------------
          WRITE ( NAME, 1004 ) ELEMENT
C----------------------------------------------------------------------
C  Extract element name
C----------------------------------------------------------------------
          CALL EZGETS ( NAME,1,ELEMENT_NAME,LEN,IER )
C----------------------------------------------------------------------
C  If previous element was a signal board, then need to create a
C  volume representing the argon gap between the signal board and the
C  current element.
C----------------------------------------------------------------------
          IF ( PREVIOUS_ELEMENT_NAME(1:2) .EQ. 'SG' ) THEN
            INDEX = INDEX + 1
            IUSET_VOLUME_NAME(INDEX) = 'A+' // 
     &                                 PREVIOUS_ELEMENT_NAME(3:4)
            IUSET_IDTYPE(INDEX) = ACTIVE_IDTYPE
          ENDIF
C----------------------------------------------------------------------
C  If this element is a signal board, then need to create a volume
C  representing the argon gap between the previous element and this
C  signal board.
C----------------------------------------------------------------------
          IF ( ELEMENT_NAME(1:2) .EQ. 'SG' ) THEN
            INDEX = INDEX + 1
            IUSET_VOLUME_NAME(INDEX) = 'A-' // ELEMENT_NAME(3:4)
            IUSET_IDTYPE(INDEX) = ACTIVE_IDTYPE
          ENDIF
C----------------------------------------------------------------------
C  Create a volume representing this element
C----------------------------------------------------------------------
          INDEX = INDEX + 1
          IUSET_VOLUME_NAME(INDEX) = ELEMENT_NAME(1:4)
          IUSET_IDTYPE(INDEX) = INACTIVE_IDTYPE
C----------------------------------------------------------------------
C  Retain parameters of current element
C----------------------------------------------------------------------
          PREVIOUS_ELEMENT_NAME = ELEMENT_NAME
        ENDDO
      ENDDO
C----------------------------------------------------------------------
C  Write detector set array
C----------------------------------------------------------------------
      IUSET_NV = INDEX
      CALL WRITE_IUSET
      RETURN
 1001 FORMAT('IUSET_CCEM_FLOOR',I1,'_IDTYPE')
 1002 FORMAT('CCEM_FLOOR',I1,'_FIRST_ELEMENT')
 1003 FORMAT('CCEM_FLOOR',I1,'_LAST_ELEMENT')
 1004 FORMAT('CCEM_ELEMENT_',I3.3)
      END
