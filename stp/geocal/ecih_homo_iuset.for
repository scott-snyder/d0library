      SUBROUTINE ECIH_HOMO_IUSET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ECIH Detector Sets using Homogeneous level 
C-                         volumes
C-
C-      Create the SRCP structures which define the Detector Sets 
C-      for the ECIH using homogeneous level volumes
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  26-MAR-1990   Stuart Fuess
C-   Updated   7-JUL-1990   Andrew J. Milder  Fix IDTYPE sublayer number 
C-   Updated   3-OCT-1990   Andrew Milder  Fix ECIH set name 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:IUSET.INC'
C  Integers
      INTEGER IER,IHSET
      INTEGER LEN
      INTEGER FIRST
      INTEGER FLOOR, FLOORS
      INTEGER STEP, STEPS
      INTEGER NV
      INTEGER HOMO_BASE_IDTYPE
C  Characters
      CHARACTER*2 HOMO_VOLUME_BASE_NAME
C----------------------------------------------------------------------
C  Select the EC SRCP file
C----------------------------------------------------------------------
      CALL EZPICK ( 'ENDCAP' )
C----------------------------------------------------------------------
C  ECIH
C----------------------------------------------------------------------
      CALL EZGETS ( 'IUSET_ECIH_HOMO_LABEL',1,IUSET_LABEL,LEN,IER )
      CALL EZGET ( 'IUSET_ECIH_HOMO_NAME', IHSET, IER )
      CALL UHTOC(IHSET,4,IUSET_NAME,4)
      NV = 0
C----------------------------------------------------------------------
C  Get parameters for Fine Hadronic section
C----------------------------------------------------------------------
      CALL EZGET ( 'ECIH_FIRST_FLOOR',      FIRST,  IER )
      CALL EZGET ( 'ECIFH_FLOORS',          FLOORS, IER )
      CALL EZGET ( 'ECIFH_STEPS_PER_FLOOR', STEPS,  IER )
C----------------------------------------------------------------------
C  Base NAME and IDTYPE for homogenized volumes
C----------------------------------------------------------------------
      CALL EZGETS ( 'ECIH_HOMO_VOLUME_BASE_NAME', 1,
     &               HOMO_VOLUME_BASE_NAME, LEN, IER )
      CALL EZGET ( 'IUSET_ECIH_HOMO_BASE_IDTYPE',
     &              HOMO_BASE_IDTYPE, IER )
C----------------------------------------------------------------------
C  ECIH Homogeneous level module shell
C----------------------------------------------------------------------
      NV = NV + 1
      CALL EZGETS ( 'ECIH_HOMO_MODULE_VOLUME_NAME', 1,
     &               IUSET_VOLUME_NAME(NV), LEN, IER )
      CALL EZGET ( 'IUSET_ECIH_HOMO_MODULE_IDTYPE', 
     &              IUSET_IDTYPE(NV), IER )
C----------------------------------------------------------------------
C  Support pipe
C----------------------------------------------------------------------
      NV = NV + 1
      CALL EZGETS ( 'ECIH_SUPPORT_PIPE_VOLUME_NAME', 1,
     &               IUSET_VOLUME_NAME(NV), LEN, IER )
      CALL EZGET ( 'IUSET_ECIH_SUPPORT_PIPE_IDTYPE',
     &              IUSET_IDTYPE(NV), IER )
C----------------------------------------------------------------------
C  Homogenized Floors and Steps
C----------------------------------------------------------------------
      DO FLOOR=FIRST,FIRST+FLOORS-1
        DO STEP=1,STEPS
          NV = NV + 1
          WRITE(IUSET_VOLUME_NAME(NV),1001) HOMO_VOLUME_BASE_NAME,
     &      FLOOR, STEP
          IUSET_IDTYPE(NV) = HOMO_BASE_IDTYPE + 10 * FLOOR + STEP-1
        ENDDO
      ENDDO
C----------------------------------------------------------------------
C  Get parameters for Coarse Hadronic section
C----------------------------------------------------------------------
      FLOOR = FIRST + FLOORS
      CALL EZGET ( 'ECICH_STEPS', STEPS, IER )
C----------------------------------------------------------------------
C  Loop over Coarse Hadronic section steps and gaps
C----------------------------------------------------------------------
      DO STEP=1,STEPS
        NV = NV + 1
        WRITE(IUSET_VOLUME_NAME(NV),1001) HOMO_VOLUME_BASE_NAME,
     &    FLOOR, STEP
        IUSET_IDTYPE(NV) = HOMO_BASE_IDTYPE + 10 * FLOOR + STEP-1
      ENDDO
C----------------------------------------------------------------------
C  Finish with Back plate
C----------------------------------------------------------------------
      NV = NV + 1
      CALL EZGETS ( 'ECIH_BACK_PLATE_VOLUME_NAME', 1,
     &               IUSET_VOLUME_NAME(NV), LEN, IER )
      CALL EZGET ( 'IUSET_ECIH_BACK_PLATE_IDTYPE',
     &              IUSET_IDTYPE(NV), IER )
C----------------------------------------------------------------------
C  Write detector set
C----------------------------------------------------------------------
      IUSET_NV = NV
      CALL WRITE_IUSET
      RETURN
 1001 FORMAT(A2,2I1)
      END
