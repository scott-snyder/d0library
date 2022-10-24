      SUBROUTINE ECIH_HOMO_GEOM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create ECIH GEANT volume structures which 
C-                         will be used in homogenous level descriptions
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  27-MAR-1990   Stuart Fuess
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:ECIH_MODULE.INC'
      INCLUDE 'D0$INC:MATERIAL.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
C  Integers
      INTEGER IER
      INTEGER LEN
      INTEGER LSTRING
      INTEGER FIRST
      INTEGER FLOOR, FLOORS
      INTEGER STEP, STEPS
      INTEGER GAP, GAPS
      INTEGER INDEX
      INTEGER N
      INTEGER FRONT_PLATE_INDEX
      INTEGER SUPPORT_PLATE_INDEX
      INTEGER FINE_ABS_INDEX
      INTEGER COARSE_ABS_INDEX
      INTEGER MLB_INDEX
      INTEGER ARGON_GAP_INDEX
C  Reals
      REAL Z
      REAL Z_FIRST
      REAL Z_LAST
      REAL FRONT_PLATE_IR
      REAL FRONT_PLATE_OR
      REAL FRONT_PLATE_THK
      REAL FRONT_PLATE_VOL
      REAL SUPPORT_PLATE_IR
      REAL SUPPORT_PLATE_OR
      REAL SUPPORT_PLATE_THK
      REAL SUPPORT_PLATE_VOL
      REAL FINE_ABS_IR
      REAL FINE_ABS_OR
      REAL FINE_ABS_THK
      REAL FINE_ABS_VOL
      REAL COARSE_ABS_IR
      REAL COARSE_ABS_OR
      REAL COARSE_ABS_THK
      REAL COARSE_ABS_VOL
      REAL MLB_IR
      REAL MLB_OR
      REAL MLB_THK
      REAL MLB_VOL
      REAL ARGON_GAP_IR
      REAL ARGON_GAP_OR
      REAL ARGON_GAP_THK
      REAL ARGON_GAP_AREA
      REAL ARGON_GAP_VOL
      REAL VI(4)
      REAL DELTA_VI
      REAL TOTAL_VI
      REAL GRAND_TOTAL_VI
      REAL VC(4)
      REAL TOTAL_VC
      REAL EXCESS_VC
C  Characters
      CHARACTER*32 NAME
      CHARACTER*2 BASE_NAME
      CHARACTER*4 CHAR_NAME
C  Parameters
      REAL CM_PER_INCH
      PARAMETER ( CM_PER_INCH = 2.54 )
C  Data
      CHARACTER*20 INDEX_NAME(4)
      DATA INDEX_NAME / 'STAINLESS_STEEL_CODE',
     &                  'URANIUM_CODE',
     &                  'G10_CODE',
     &                  'LIQUID_ARGON_CODE' /
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
      CALL EZGET_i ( 'ECIH_FIRST_FLOOR',      FIRST,  IER )
      CALL EZGET_i ( 'ECIFH_FLOORS',          FLOORS, IER )
      CALL EZGET_i ( 'ECIFH_STEPS_PER_FLOOR', STEPS,  IER )
      CALL EZGET_i ( 'ECIFH_GAPS_PER_STEP',   GAPS,   IER )
C----------------------------------------------------------------------
C  Get the dimensions of the various components of the IH
C  Set the material index code:
C       1 = Stainless Steel
C       2 = Uranium
C       3 = G10
C       4 = Liquid Argon
C  Front Plate
C----------------------------------------------------------------------
      CALL EZGET ( 'ECIH_FRONT_PLATE_INNER_RADIUS',
     &  FRONT_PLATE_IR, IER )
      CALL EZGET ( 'ECIH_FRONT_PLATE_OUTER_RADIUS',
     &  FRONT_PLATE_OR, IER )
      CALL EZGET ( 'ECIH_FRONT_PLATE_THICKNESS',
     &  FRONT_PLATE_THK, IER )
      FRONT_PLATE_VOL = FRONT_PLATE_THK * PI * 
     &  (FRONT_PLATE_OR**2 - FRONT_PLATE_IR**2)
      FRONT_PLATE_INDEX = 1
C----------------------------------------------------------------------
C  Support Plate
C----------------------------------------------------------------------
      CALL EZGET ( 'ECIH_SUPPORT_PLATE_INNER_RADIUS',
     &  SUPPORT_PLATE_IR, IER )
      CALL EZGET ( 'ECIH_SUPPORT_PLATE_OUTER_RADIUS',
     &  SUPPORT_PLATE_OR, IER )
      CALL EZGET ( 'ECIH_SUPPORT_PLATE_THICKNESS',
     &  SUPPORT_PLATE_THK, IER )
      SUPPORT_PLATE_VOL = SUPPORT_PLATE_THK * PI * 
     &  (SUPPORT_PLATE_OR**2 - SUPPORT_PLATE_IR**2)
      SUPPORT_PLATE_INDEX = 1
C----------------------------------------------------------------------
C  Fine Absorber Plate
C----------------------------------------------------------------------
      CALL EZGET ( 'ECIH_FINE_ABS_INNER_RADIUS',
     &  FINE_ABS_IR, IER )
      CALL EZGET ( 'ECIH_FINE_ABS_OUTER_RADIUS',
     &  FINE_ABS_OR, IER )
      CALL EZGET ( 'ECIH_FINE_ABS_THICKNESS',
     &  FINE_ABS_THK, IER )
      FINE_ABS_VOL = FINE_ABS_THK * PI * 
     &  (FINE_ABS_OR**2 - FINE_ABS_IR**2)
      FINE_ABS_INDEX = 2
C----------------------------------------------------------------------
C  Coarse Absorber Plate
C----------------------------------------------------------------------
      CALL EZGET ( 'ECIH_COARSE_ABS_INNER_RADIUS',
     &  COARSE_ABS_IR, IER )
      CALL EZGET ( 'ECIH_COARSE_ABS_OUTER_RADIUS',
     &  COARSE_ABS_OR, IER )
      CALL EZGET ( 'ECIH_COARSE_ABS_THICKNESS',
     &  COARSE_ABS_THK, IER )
      COARSE_ABS_VOL = COARSE_ABS_THK * PI * 
     &  (COARSE_ABS_OR**2 - COARSE_ABS_IR**2)
      COARSE_ABS_INDEX = 1
C----------------------------------------------------------------------
C  MLB Board
C----------------------------------------------------------------------
      CALL EZGET ( 'ECIH_MLB_INNER_RADIUS',
     &  MLB_IR, IER )
      CALL EZGET ( 'ECIH_MLB_OUTER_RADIUS',
     &  MLB_OR, IER )
      CALL EZGET ( 'ECIH_MLB_THICKNESS',
     &  MLB_THK, IER )
      MLB_VOL = MLB_THK * PI * 
     &  (MLB_OR**2 - MLB_IR**2)
      MLB_INDEX = 3
C----------------------------------------------------------------------
C  Argon Gap
C----------------------------------------------------------------------
      CALL EZGET ( 'ECIH_ARGON_GAP_INNER_RADIUS',
     &  ARGON_GAP_IR, IER )
      CALL EZGET ( 'ECIH_ARGON_GAP_OUTER_RADIUS',
     &  ARGON_GAP_OR, IER )
      CALL EZGET ( 'ECIH_ARGON_GAP_THICKNESS',
     &  ARGON_GAP_THK, IER )
      ARGON_GAP_AREA = PI * (ARGON_GAP_OR**2 - ARGON_GAP_IR**2)
      ARGON_GAP_VOL = ARGON_GAP_THK * ARGON_GAP_AREA
      ARGON_GAP_INDEX = 4
C----------------------------------------------------------------------
C  Zero grand total sum of material in active region
C----------------------------------------------------------------------
      GRAND_TOTAL_VI = 0.
C----------------------------------------------------------------------
C  Zero sums of material in 'crack'
C----------------------------------------------------------------------
      DO INDEX=1,4
        VC(INDEX) = 0.
      ENDDO
C----------------------------------------------------------------------
C  Loop over Fine Hadronic section floors, steps
C  A homogeneous volume will be created for each STEP
C----------------------------------------------------------------------
      DO FLOOR=FIRST,FIRST+FLOORS-1
        DO STEP=1,STEPS
          Z_FIRST = Z
          DO INDEX=1,4
            VI(INDEX) = 0.
          ENDDO
C----------------------------------------------------------------------
C  Loop over gaps and accumulate relative volumes of different
C  materials
C----------------------------------------------------------------------
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
                  Z = Z + FRONT_PLATE_THK
                  DELTA_VI = FRONT_PLATE_THK * ARGON_GAP_AREA
                  VI(FRONT_PLATE_INDEX) = VI(FRONT_PLATE_INDEX) +
     &              DELTA_VI
                  VC(FRONT_PLATE_INDEX) = VC(FRONT_PLATE_INDEX) +
     &              FRONT_PLATE_VOL - DELTA_VI
                ELSE
                  Z = Z + SUPPORT_PLATE_THK
                  DELTA_VI = SUPPORT_PLATE_THK * ARGON_GAP_AREA
                  VI(SUPPORT_PLATE_INDEX) = VI(SUPPORT_PLATE_INDEX) +
     &              DELTA_VI
                  VC(SUPPORT_PLATE_INDEX) = VC(SUPPORT_PLATE_INDEX) +
     &              SUPPORT_PLATE_VOL - DELTA_VI
                ENDIF
              ELSE
                Z = Z + FINE_ABS_THK
                DELTA_VI = FINE_ABS_THK * ARGON_GAP_AREA
                VI(FINE_ABS_INDEX) = VI(FINE_ABS_INDEX) +
     &            DELTA_VI
                VC(FINE_ABS_INDEX) = VC(FINE_ABS_INDEX) +
     &            FINE_ABS_VOL - DELTA_VI
              ENDIF
            ELSE
              Z = Z + FINE_ABS_THK
              DELTA_VI = FINE_ABS_THK * ARGON_GAP_AREA
              VI(FINE_ABS_INDEX) = VI(FINE_ABS_INDEX) +
     &          DELTA_VI
              VC(FINE_ABS_INDEX) = VC(FINE_ABS_INDEX) +
     &          FINE_ABS_VOL - DELTA_VI
            ENDIF

            Z = Z + ARGON_GAP_THK
            VI(ARGON_GAP_INDEX) = VI(ARGON_GAP_INDEX) +
     &        ARGON_GAP_VOL

            Z = Z + MLB_THK
            DELTA_VI = MLB_THK * ARGON_GAP_AREA
            VI(MLB_INDEX) = VI(MLB_INDEX) +
     &        DELTA_VI
            VC(MLB_INDEX) = VC(MLB_INDEX) +
     &        MLB_VOL - DELTA_VI

            Z = Z + ARGON_GAP_THK
            VI(ARGON_GAP_INDEX) = VI(ARGON_GAP_INDEX) +
     &        ARGON_GAP_VOL

          ENDDO
C----------------------------------------------------------------------
C  Determine the material properties of this homogeneous volume
C----------------------------------------------------------------------
          WRITE(MATERIAL_LABEL,1001) FLOOR,STEP
          CALL ADDSTR ( MATERIAL_LABEL, '_NAME', NAME, LSTRING )
          CALL EZGET_iarr ( NAME, MATERIAL_NAME, IER )
          CALL ADDSTR ( MATERIAL_LABEL, '_CODE', NAME, LSTRING )
          CALL EZGET_i ( NAME, MATERIAL_CODE, IER )

          TOTAL_VI = 0.
          DO INDEX=1,4
            TOTAL_VI = TOTAL_VI + VI(INDEX)
          ENDDO
          GRAND_TOTAL_VI = GRAND_TOTAL_VI + TOTAL_VI

          N = 0
          DO INDEX=1,4
            IF (VI(INDEX) .GT. 0.) THEN
              N = N + 1
              CALL EZGET_i ( INDEX_NAME(INDEX), COMPONENT_CODE(N), IER )
              COMPONENT_FRACTION(N) = VI(INDEX) / TOTAL_VI
            ENDIF
          ENDDO
          NUMBER_COMPONENTS = N
C----------------------------------------------------------------------
C  Store the homogeneous material
C----------------------------------------------------------------------
          CALL STORE_MATERIAL
C----------------------------------------------------------------------
C  Set the volume parameters of this homogeneous volume
C----------------------------------------------------------------------
          Z_LAST = Z
          WRITE(VOLUME_LABEL,1002) FLOOR,STEP
          CALL EZGETS ( 'ECIH_HOMO_VOLUME_BASE_NAME', 1,
     &      BASE_NAME, LEN, IER )
          WRITE(CHAR_NAME,1003) BASE_NAME,FLOOR,STEP
          CALL UCTOH ( CHAR_NAME, VOLUME_NAME, 4, 4 )
          CALL UCTOH ( 'TUBE', VOLUME_SHAPE, 4, 4 )
          VOLUME_MATERIAL_CODE = MATERIAL_CODE
          CALL EZGET_i ( 'ECIH_HOMO_MODULE_VOLUME_NAME', VOLUME_MOTHER,
     &      IER )
          CALL UCTOH ( 'POS', POSITIONING, 4, 3 )
          ROTATION_MATRIX = 1
          COPY_NUMBER = 1
          X_POSITION = 0.
          Y_POSITION = 0.
          Z_POSITION = 0.5 * CM_PER_INCH * (Z_FIRST + Z_LAST)
          NUMBER_PARAMS = 3
          PARAM(1) = CM_PER_INCH * ARGON_GAP_IR
          PARAM(2) = CM_PER_INCH * ARGON_GAP_OR
          PARAM(3) = 0.5 * CM_PER_INCH * (Z_LAST - Z_FIRST)
C----------------------------------------------------------------------
C  Write the homogeneous volume
C----------------------------------------------------------------------
          CALL WRITE_VOLUME
        ENDDO
      ENDDO
C----------------------------------------------------------------------
C  Get parameters for Coarse Hadronic section
C----------------------------------------------------------------------
      FLOOR = FIRST + FLOORS
      CALL EZGET_i ( 'ECICH_STEPS',         STEPS, IER )
      CALL EZGET_i ( 'ECICH_GAPS_PER_STEP', GAPS,  IER )
C----------------------------------------------------------------------
C  Loop over Coarse Hadronic section steps and gaps
C----------------------------------------------------------------------
      DO STEP=1,STEPS
        Z_FIRST = Z
        DO INDEX=1,4
          VI(INDEX) = 0.
        ENDDO
        DO GAP=1,GAPS
          Z = Z + COARSE_ABS_THK
          DELTA_VI = COARSE_ABS_THK * ARGON_GAP_AREA
          VI(COARSE_ABS_INDEX) = VI(COARSE_ABS_INDEX) +
     &      DELTA_VI
          VC(COARSE_ABS_INDEX) = VC(COARSE_ABS_INDEX) +
     &      COARSE_ABS_VOL - DELTA_VI

          Z = Z + ARGON_GAP_THK
          VI(ARGON_GAP_INDEX) = VI(ARGON_GAP_INDEX) +
     &      ARGON_GAP_VOL

          Z = Z + MLB_THK
          DELTA_VI = MLB_THK * ARGON_GAP_AREA
          VI(MLB_INDEX) = VI(MLB_INDEX) +
     &      DELTA_VI
          VC(MLB_INDEX) = VC(MLB_INDEX) +
     &      MLB_VOL - DELTA_VI

          Z = Z + ARGON_GAP_THK
          VI(ARGON_GAP_INDEX) = VI(ARGON_GAP_INDEX) +
     &      ARGON_GAP_VOL
        ENDDO
C----------------------------------------------------------------------
C  Determine the material properties of this homogeneous volume
C----------------------------------------------------------------------
        WRITE(MATERIAL_LABEL,1001) FLOOR,STEP
        CALL ADDSTR ( MATERIAL_LABEL, '_NAME', NAME, LSTRING )
        CALL EZGET_iarr ( NAME, MATERIAL_NAME, IER )
        CALL ADDSTR ( MATERIAL_LABEL, '_CODE', NAME, LSTRING )
        CALL EZGET_i ( NAME, MATERIAL_CODE, IER )

        TOTAL_VI = 0.
        DO INDEX=1,4
          TOTAL_VI = TOTAL_VI + VI(INDEX)
        ENDDO
        GRAND_TOTAL_VI = GRAND_TOTAL_VI + TOTAL_VI

        N = 0
        DO INDEX=1,4
          IF (VI(INDEX) .GT. 0.) THEN
            N = N + 1
            CALL EZGET_i ( INDEX_NAME(INDEX), COMPONENT_CODE(N), IER )
            COMPONENT_FRACTION(N) = VI(INDEX) / TOTAL_VI
          ENDIF
        ENDDO
        NUMBER_COMPONENTS = N
C----------------------------------------------------------------------
C  Store the homogeneous material
C----------------------------------------------------------------------
        CALL STORE_MATERIAL
C----------------------------------------------------------------------
C  Set the volume parameters of this homogeneous volume
C----------------------------------------------------------------------
        Z_LAST = Z
        WRITE(VOLUME_LABEL,1002) FLOOR,STEP
        CALL EZGETS ( 'ECIH_HOMO_VOLUME_BASE_NAME', 1,
     &    BASE_NAME, LEN, IER )
        WRITE(CHAR_NAME,1003) BASE_NAME,FLOOR,STEP
        CALL UCTOH ( CHAR_NAME, VOLUME_NAME, 4, 4 )
        CALL UCTOH ( 'TUBE', VOLUME_SHAPE, 4, 4 )
        VOLUME_MATERIAL_CODE = MATERIAL_CODE
        CALL EZGET_i ( 'ECIH_HOMO_MODULE_VOLUME_NAME', VOLUME_MOTHER, 
     &    IER )
        CALL UCTOH ( 'POS', POSITIONING, 4, 3 )
        ROTATION_MATRIX = 1
        COPY_NUMBER = 1
        X_POSITION = 0.
        Y_POSITION = 0.
        Z_POSITION = 0.5 * CM_PER_INCH * (Z_FIRST + Z_LAST)
        NUMBER_PARAMS = 3
        PARAM(1) = CM_PER_INCH * ARGON_GAP_IR
        PARAM(2) = CM_PER_INCH * ARGON_GAP_OR
        PARAM(3) = 0.5 * CM_PER_INCH * (Z_LAST - Z_FIRST)
C----------------------------------------------------------------------
C  Write the homogeneous volume
C----------------------------------------------------------------------
        CALL WRITE_VOLUME
      ENDDO
C----------------------------------------------------------------------
C  Determine the material properties of the 'crack' homogeneous volume
C----------------------------------------------------------------------
      CALL EZGETS ( 'ECIH_CRACK_MATERIAL_LABEL', 1,
     &  MATERIAL_LABEL, LEN, IER )
      CALL EZGET_arr ( 'ECIH_CRACK_MATERIAL_NAME', MATERIAL_NAME, IER )
      CALL EZGET_i ( 'ECIH_CRACK_MATERIAL_CODE', MATERIAL_CODE, IER )

      DO INDEX=1,4
        TOTAL_VC = TOTAL_VC + VC(INDEX)
      ENDDO

      EXCESS_VC = ECIH_MODULE_VOLUME - TOTAL_VC - GRAND_TOTAL_VI
      TOTAL_VC = TOTAL_VC + EXCESS_VC
      VC(ARGON_GAP_INDEX) = VC(ARGON_GAP_INDEX) + EXCESS_VC

      N = 0
      DO INDEX=1,4
        IF (VC(INDEX) .GT. 0.) THEN
          N = N + 1
          CALL EZGET_i ( INDEX_NAME(INDEX), COMPONENT_CODE(N), IER )
          COMPONENT_FRACTION(N) = VC(INDEX) / TOTAL_VC
        ENDIF
      ENDDO
      NUMBER_COMPONENTS = N
C----------------------------------------------------------------------
C  Store the homogeneous material
C----------------------------------------------------------------------
      CALL STORE_MATERIAL
  999 RETURN
 1001 FORMAT('ECIH_HOMO_',2I1,'_MATERIAL')
 1002 FORMAT('ECIH_HOMO_',2I1,'_VOLUME')
 1003 FORMAT(A2,2I1)
      END
