      SUBROUTINE ECIH_PLATE_VOLUMES(NLINES,LINE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ECIH Plate level volumes
C-
C-   Inputs  :  NLINES  Current index in LINE array
C-              LINE    Character array
C-   Outputs :  NLINES  Updated index in LINE array
C-   Controls:  none
C-
C-   Created  21-MAR-1990   Stuart Fuess
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
      INTEGER GAP, GAPS
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
      CALL EZGET ( 'ECIFH_GAPS_PER_STEP',   GAPS,   IER )
C----------------------------------------------------------------------
C  ECIH Plate level module shell
C----------------------------------------------------------------------
      CALL EZGETS ( 'ECIH_PLATE_MODULE_VOLUME_LABEL', 1,
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
C  Loop over Fine Hadronic section floors, steps, and gaps
C----------------------------------------------------------------------
      DO FLOOR=FIRST,FIRST+FLOORS-1
        DO STEP=1,STEPS
          DO GAP=1,GAPS
C----------------------------------------------------------------------
C  First gap of first step of first floor begins with Front plate
C  First gap of first step of other floors begins with Support plate
C  First gap of other steps begins with fine absorber
C  Other gaps begin with fine absorber
C  Remainder of gap has argon gaps and MLB
C----------------------------------------------------------------------
            IF (GAP.EQ.1) THEN
              IF (STEP.EQ.1) THEN
                IF (FLOOR.EQ.FIRST) THEN
                  CALL EZGETS ( 'ECIH_FRONT_PLATE_VOLUME_LABEL', 1,
     &                           LABEL, LEN, IER )
                  NLINES = NLINES + 1
                  LINE(NLINES) = '''' // LABEL(1:LEN) // ''''
                ELSE
                  NLINES = NLINES + 1
                  WRITE(LINE(NLINES),1001) FLOOR
                ENDIF
              ELSE
                NLINES = NLINES + 1
                WRITE(LINE(NLINES),1002) FLOOR,STEP,GAP
              ENDIF
            ELSE
              NLINES = NLINES + 1
              WRITE(LINE(NLINES),1002) FLOOR,STEP,GAP
            ENDIF

            NLINES = NLINES + 1
            WRITE(LINE(NLINES),1003) FLOOR,STEP,GAP,1

            NLINES = NLINES + 1
            WRITE(LINE(NLINES),1004) FLOOR,STEP,GAP

            NLINES = NLINES + 1
            WRITE(LINE(NLINES),1003) FLOOR,STEP,GAP,2

          ENDDO
        ENDDO
      ENDDO
C----------------------------------------------------------------------
C  Get parameters for Coarse Hadronic section
C----------------------------------------------------------------------
      FLOOR = FIRST + FLOORS
      CALL EZGET ( 'ECICH_STEPS',         STEPS, IER )
      CALL EZGET ( 'ECICH_GAPS_PER_STEP', GAPS,  IER )
C----------------------------------------------------------------------
C  Loop over Coarse Hadronic section steps and gaps
C----------------------------------------------------------------------
      DO STEP=1,STEPS
        DO GAP=1,GAPS
          NLINES = NLINES + 1
          WRITE(LINE(NLINES),1005) STEP,GAP

          NLINES = NLINES + 1
          WRITE(LINE(NLINES),1003) FLOOR,STEP,GAP,1

          NLINES = NLINES + 1
          WRITE(LINE(NLINES),1004) FLOOR,STEP,GAP

          NLINES = NLINES + 1
          WRITE(LINE(NLINES),1003) FLOOR,STEP,GAP,2
        ENDDO
      ENDDO
C----------------------------------------------------------------------
C  Finish with Back plate
C----------------------------------------------------------------------
      CALL EZGETS ( 'ECIH_BACK_PLATE_VOLUME_LABEL', 1,
     &               LABEL, LEN, IER )
      NLINES = NLINES + 1
      LINE(NLINES) = '''' // LABEL(1:LEN) // ''''
      RETURN
 1001 FORMAT('''ECIH_SUPPORT_PLATE_',I1,'_VOLUME''')
 1002 FORMAT('''ECIH_FINE_ABS_',3I1,'_VOLUME''')
 1003 FORMAT('''ECIH_ARGON_GAP_',4I1,'_VOLUME''')
 1004 FORMAT('''ECIH_MLB_',3I1,'_VOLUME''')
 1005 FORMAT('''ECIH_COARSE_ABS_',2I1,'_VOLUME''')
      END
