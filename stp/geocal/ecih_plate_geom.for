      SUBROUTINE ECIH_PLATE_GEOM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create ECIH GEANT volume structures which 
C-                         will be used in plate level descriptions
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  18-MAR-1990   Stuart Fuess
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:ECIH_MODULE.INC'
C  Integers
      INTEGER IER
      INTEGER FIRST
      INTEGER FLOOR, FLOORS
      INTEGER STEP, STEPS
      INTEGER GAP, GAPS
      INTEGER SUPPORT_PLATE_COPY
      INTEGER FINE_ABS_COPY
      INTEGER COARSE_ABS_COPY
      INTEGER MLB_COPY
      INTEGER ARGON_GAP_COPY
C  Reals
      REAL Z
C  Parameters
      REAL CM_PER_INCH
      PARAMETER ( CM_PER_INCH = 2.54 )
C----------------------------------------------------------------------
C  Select the EC SRCP file
C----------------------------------------------------------------------
      CALL EZPICK('ENDCAP')
C----------------------------------------------------------------------
C  Initialize Z position (inches) at module beginning
C----------------------------------------------------------------------
      Z = - 0.5 * ECIH_MODULE_LENGTH
C----------------------------------------------------------------------
C  Get parameters for Fine Hadronic section
C----------------------------------------------------------------------
      CALL EZGET ( 'ECIH_FIRST_FLOOR',      FIRST,  IER )
      CALL EZGET ( 'ECIFH_FLOORS',          FLOORS, IER )
      CALL EZGET ( 'ECIFH_STEPS_PER_FLOOR', STEPS,  IER )
      CALL EZGET ( 'ECIFH_GAPS_PER_STEP',   GAPS,   IER )
C----------------------------------------------------------------------
C  Initialize copy numbers
C----------------------------------------------------------------------
      SUPPORT_PLATE_COPY = 0
      FINE_ABS_COPY      = 0
      COARSE_ABS_COPY    = 0
      MLB_COPY           = 0
C----------------------------------------------------------------------
C  Loop over Fine Hadronic section floors, steps, and gaps
C  There will be different Argon Gap volumes for each step, so zero
C  copy number outside of GAP loop.
C----------------------------------------------------------------------
      DO FLOOR=FIRST,FIRST+FLOORS-1
        DO STEP=1,STEPS
          ARGON_GAP_COPY     = 0
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
                  CALL ECIH_FRONT_PLATE_GEOM(Z)
                ELSE
                  SUPPORT_PLATE_COPY = SUPPORT_PLATE_COPY + 1
                  CALL ECIH_SUPPORT_PLATE_GEOM(FLOOR,
     &              SUPPORT_PLATE_COPY,Z)
                ENDIF
              ELSE
                FINE_ABS_COPY = FINE_ABS_COPY + 1
                CALL ECIH_FINE_ABS_GEOM(FLOOR,STEP,GAP,FINE_ABS_COPY,Z)
              ENDIF
            ELSE
              FINE_ABS_COPY = FINE_ABS_COPY + 1
              CALL ECIH_FINE_ABS_GEOM(FLOOR,STEP,GAP,FINE_ABS_COPY,Z)
            ENDIF

            ARGON_GAP_COPY = ARGON_GAP_COPY + 1
            CALL ECIH_ARGON_GAP_GEOM(FLOOR,STEP,GAP,1,ARGON_GAP_COPY,Z)

            MLB_COPY = MLB_COPY + 1
            CALL ECIH_MLB_GEOM(FLOOR,STEP,GAP,MLB_COPY,Z)

            ARGON_GAP_COPY = ARGON_GAP_COPY + 1
            CALL ECIH_ARGON_GAP_GEOM(FLOOR,STEP,GAP,2,ARGON_GAP_COPY,Z)

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
C  There will be different Argon Gap volumes for each step, so zero
C  copy number outside of GAP loop.
C----------------------------------------------------------------------
      DO STEP=1,STEPS
        ARGON_GAP_COPY     = 0
        DO GAP=1,GAPS
          COARSE_ABS_COPY = COARSE_ABS_COPY + 1
          CALL ECIH_COARSE_ABS_GEOM(STEP,GAP,COARSE_ABS_COPY,Z)

          ARGON_GAP_COPY = ARGON_GAP_COPY + 1
          CALL ECIH_ARGON_GAP_GEOM(FLOOR,STEP,GAP,1,ARGON_GAP_COPY,Z)

          MLB_COPY = MLB_COPY + 1
          CALL ECIH_MLB_GEOM(FLOOR,STEP,GAP,MLB_COPY,Z)

          ARGON_GAP_COPY = ARGON_GAP_COPY + 1
          CALL ECIH_ARGON_GAP_GEOM(FLOOR,STEP,GAP,2,ARGON_GAP_COPY,Z)
        ENDDO
      ENDDO
C----------------------------------------------------------------------
C  Finish with Back plate
C----------------------------------------------------------------------
      CALL ECIH_BACK_PLATE_GEOM(Z)
  999 RETURN
      END
