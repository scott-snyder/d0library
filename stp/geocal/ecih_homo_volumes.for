      SUBROUTINE ECIH_HOMO_VOLUMES(NLINES,LINE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ECIH Homogeneous level volumes
C-
C-   Inputs  :  NLINES  Current index in LINE array
C-              LINE    Character array
C-   Outputs :  NLINES  Updated index in LINE array
C-   Controls:  none
C-
C-   Created  26-MAR-1990   Stuart Fuess
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Arguments
      INTEGER NLINES
      CHARACTER*(*) LINE(*)
C  Integers
      INTEGER IER
      INTEGER LEN
      INTEGER FIRST
      INTEGER FLOOR, FLOORS
      INTEGER STEP, STEPS
C  Characters
      CHARACTER*32 LABEL
C----------------------------------------------------------------------
C  Select the EC SRCP file
C----------------------------------------------------------------------
      CALL EZPICK ( 'ENDCAP' )
C----------------------------------------------------------------------
C  Get parameters for Fine Hadronic section
C----------------------------------------------------------------------
      CALL EZGET ( 'ECIH_FIRST_FLOOR',      FIRST,  IER )
      CALL EZGET ( 'ECIFH_FLOORS',          FLOORS, IER )
      CALL EZGET ( 'ECIFH_STEPS_PER_FLOOR', STEPS,  IER )
C----------------------------------------------------------------------
C  ECIH Homogeneous level module shell
C----------------------------------------------------------------------
      CALL EZGETS ( 'ECIH_HOMO_MODULE_VOLUME_LABEL', 1,
     &               LABEL, LEN, IER )
      NLINES = NLINES + 1
      LINE(NLINES) = '''' // LABEL(1:LEN) // ''''
C----------------------------------------------------------------------
C  Start with Support pipe
C----------------------------------------------------------------------
      CALL EZGETS ( 'ECIH_SUPPORT_PIPE_VOLUME_LABEL', 1,
     &               LABEL, LEN, IER )
      NLINES = NLINES + 1
      LINE(NLINES) = '''' // LABEL(1:LEN) // ''''
C----------------------------------------------------------------------
C  Loop over Fine Hadronic section floors and steps
C----------------------------------------------------------------------
      DO FLOOR=FIRST,FIRST+FLOORS-1
        DO STEP=1,STEPS
          NLINES = NLINES + 1
          WRITE(LINE(NLINES),1001) FLOOR,STEP
        ENDDO
      ENDDO
C----------------------------------------------------------------------
C  Get parameters for Coarse Hadronic section
C----------------------------------------------------------------------
      FLOOR = FIRST + FLOORS
      CALL EZGET ( 'ECICH_STEPS', STEPS, IER )
C----------------------------------------------------------------------
C  Loop over Coarse Hadronic section steps
C----------------------------------------------------------------------
      DO STEP=1,STEPS
        NLINES = NLINES + 1
        WRITE(LINE(NLINES),1001) FLOOR,STEP
      ENDDO
C----------------------------------------------------------------------
C  Finish with Back plate
C----------------------------------------------------------------------
      CALL EZGETS ( 'ECIH_BACK_PLATE_VOLUME_LABEL', 1,
     &               LABEL, LEN, IER )
      NLINES = NLINES + 1
      LINE(NLINES) = '''' // LABEL(1:LEN) // ''''
      RETURN
 1001 FORMAT('''ECIH_HOMO_',2I1,'_VOLUME''')
      END
