      SUBROUTINE ECEM_PLATE_GEOM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute Plate level ECEM geometry for GEANT
C-
C-      Each plate volume will be a 'TUBE'.  The plate volumes are
C-      grouped in cells, each of which consists of the following
C-      elements:  absorber, argon, G10, copper, argon.  The inner
C-      radius, outer radius and thickness of each element is read
C-      from the SRCP file and converted to the appropriate geometry
C-      constants.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  30-NOV-1989   Natalie Roe
C-   Updated   6-DEC-1989   Stuart Fuess  Mother-->Module,
C-                                        Module-->Floor
C-   Updated  14-DEC-1989   Stuart Fuess  was TBM_90_ECEM_PLATE_GEOM
C-   Updated  27-FEB-1989   Natalie Roe   add EM 1&2 readout board,
C-            add support pipe, change mother volume of plates to EEM+
C-   Updated  13-MAR-1990   Natalie Roe   move EM 1&2 readout board,
C-                    support pipe, and strongback to ECEM_MISC_GEOM.
C-   Updated   3-OCT-1990   Andrew Milder  Volume structure redone with copies
C-   Updated  20-FEB-1996   Alan Jonckheere, fix misplaced ENDIF/ENDDO
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
C  Local Variables
      INTEGER ICODE,IER,K   ! I,J,
      REAL CM_PER_INCH,MOTHERZ,Z_BEGIN,MLO,MHI  ! VAL(4),
      PARAMETER ( CM_PER_INCH = 2.54 )
      CHARACTER*1 MAT(5),ABSORB
      CHARACTER*4 MATERIAL_CODE,CHAR_NAME
      CHARACTER*8 PLATE_TYPE(5)
      INTEGER NUM_SUBLAYERS(4),NUM_PLATES(4),LAYER,SUBLAYER,PLATE
      INTEGER PLATE_IN_SUBLAYER,COPY,ARGON_COPY,MAT_LAYER
      DATA PLATE_TYPE    /'ABSORBER',
     &                    'ARGON_A ',
     &                    'MLB     ',
     &                    'COPPER  ',
     &                    'ARGON_B '/
      DATA NUM_SUBLAYERS / 1,1,3,2 /
      DATA NUM_PLATES    / 2,2,2,5 /
      DATA MAT           / 'A','Y','M','C','Y' /
C----------------------------------------------------------------------
D     WRITE(6,*)'IN ECEM_PLATE_GEOM'
C----------------------------------------------------------------------
      CALL EZPICK('ENDCAP')
      CALL EZGETA('ECEM+Z_MODULE_VOLUME',5,5,1,MLO,IER)
      CALL EZGETA('ECEM+Z_MODULE_VOLUME',11,11,1,MHI,IER)
      MOTHERZ=(MLO+MHI)/2.
C----------------------------------------------------------------------
C EM 1-4 PLATES: 2,2,6,8 LAYERS OF: ABSORBER, LAR, MLB, CU, LAR
C----------------------------------------------------------------------
      CALL EZGETA('ECEM+Z_FLOOR1_VOLUME',6,6,1,Z_BEGIN,IER)
      PLATE = 0
      DO LAYER =  1,4
        DO SUBLAYER = 1,NUM_SUBLAYERS(LAYER)
          COPY = 0
          ARGON_COPY = 0
          DO PLATE_IN_SUBLAYER = 1,NUM_PLATES(LAYER)
            IF ( LAYER.NE.4 .OR. SUBLAYER.NE.1 .OR.
     &              PLATE_IN_SUBLAYER.NE.5 ) THEN
              COPY = COPY + 1
              PLATE = PLATE + 1
              DO MAT_LAYER = 1,5
                IF (PLATE.EQ.19 .AND. MAT_LAYER.NE.1) GOTO 100
C----------------------------------------------------------------------
C  Different copy numbers for argon gaps - want argon gaps in the same
C   sublayer to have the same name
C----------------------------------------------------------------------
                IF (MAT_LAYER.EQ.2 .OR. MAT_LAYER.EQ.5) THEN
                  ARGON_COPY = ARGON_COPY + 1
                  COPY_NUMBER = ARGON_COPY
                ELSE
                  COPY_NUMBER = COPY
                ENDIF
C----------------------------------------------------------------------
C  Hardwire in name change for layer 4 (7), sublayer 1, first 2
C   absorber plates. Due to change in material type
C----------------------------------------------------------------------
                ABSORB = 'A'
                IF (PLATE.EQ.19) THEN
                  ABSORB = 'B'
                  COPY_NUMBER = 1
                ENDIF
                IF (LAYER.EQ.4 .AND. SUBLAYER.EQ.1 .AND. MAT_LAYER.EQ.1)
     &            THEN
                  IF (PLATE_IN_SUBLAYER.LE.2) THEN
                    ABSORB = 'B'
                  ELSE
                    ABSORB = 'A'
                    COPY_NUMBER = COPY_NUMBER - 2
                  ENDIF
                ENDIF
                WRITE(VOLUME_LABEL,900) PLATE,PLATE_TYPE(MAT_LAYER)
  900           FORMAT('ECEM+Z_',I2.2,'_',A8)
                CALL EZGETA(VOLUME_LABEL,2,2,1,ICODE,IER)
                CALL UHTOC(ICODE,4,MATERIAL_CODE,4)
                CALL EZGET(MATERIAL_CODE,VOLUME_MATERIAL_CODE,IER)
                IF (MAT_LAYER.EQ.1) THEN
                  WRITE(CHAR_NAME,901) LAYER,SUBLAYER,ABSORB
                ELSE
                  WRITE(CHAR_NAME,901) LAYER,SUBLAYER,MAT(MAT_LAYER)
                ENDIF
  901           FORMAT('E',2I1.1,A1)
                CALL UCTOH(CHAR_NAME,VOLUME_NAME,4,4)
                CALL EZGETA(VOLUME_LABEL,3,3,1,VOLUME_MOTHER,IER)
                CALL UCTOH('TUBE',VOLUME_SHAPE,4,4)
                CALL UCTOH('POSP',POSITIONING,4,4)
                NUMBER_PARAMS = 3
                CALL EZGETA(VOLUME_LABEL,4,6,1,PARAM(1),IER)
                X_POSITION = 0.
                Y_POSITION = 0.
                Z_POSITION = CM_PER_INCH * (Z_BEGIN - MOTHERZ + PARAM(3)
     &            /
     &            2.)
                Z_BEGIN    = Z_BEGIN + PARAM(3)
                IF ( PLATE.NE.11 .OR. MAT_LAYER.EQ.1) THEN
                  PARAM(3)   = PARAM(3) / 2.
                  DO K=1,NUMBER_PARAMS
                    PARAM(K) = PARAM(K) * CM_PER_INCH
                  ENDDO
                  ROTATION_MATRIX = 1               ! Identity
                  CALL WRITE_VOLUME
                ENDIF
              ENDDO
            ENDIF
          ENDDO
        ENDDO
      ENDDO
  100 CONTINUE
C----------------------------------------------------------------------
 1000 FORMAT(' IER EQ',I3,' IN EZGET FOR ',A12,' IN ECEM_PLATE_GEOM')
D1001 FORMAT('MODULE ',I3,' PLATE ',I3,': PARAM ',I3,' = ',F12.6)
 1002 FORMAT('ECEM+Z_',I2.2,'_',A8)
D1003 FORMAT(' Z AT END OF CELL',I3,' = ',F12.6)
C----------------------------------------------------------------------
      RETURN
      END
