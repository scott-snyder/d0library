      SUBROUTINE ECEM_PLATE_IUSET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ECEM Detector Sets using Plate level volumes
C-
C-      Create the SRCP structures which define the Detector Sets
C-      for the ECEM using plate level volumes
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  02-DEC-1989   Stuart Fuess
C-   Updated   3-OCT-1990   Andrew Milder  Volume structure redone,copies used 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:IUSET.INC'
C  Integers
      INTEGER I, J, IER
      INTEGER LEN
      INTEGER LSTRING,NUM_MAT
      INTEGER IDTYPE,NUM_SUBLAYERS(4),NUM_PLATES(4)
      INTEGER LAYER,SUBLAYER,PLATE,PLATE_IN_SUBLAYER,MAT_LAYER
C  Characters
      CHARACTER*1 MAT(5),ABSORB
      CHARACTER*4 NAME
      CHARACTER*32 LABEL
C  Data
      CHARACTER*8 PLATE_TYPE(5)
      DATA PLATE_TYPE     / 'ABSORBER',
     &                      'ARGON_A',
     &                      'MLB',
     &                      'COPPER',
     &                      'ABSORBER' /
      DATA NUM_SUBLAYERS  / 1,1,3,2 /
      DATA NUM_PLATES     / 2,2,2,5 /
      DATA MAT            / 'A','Y','M','C','B' /
C----------------------------------------------------------------------
C  Select the EC SRCP file
C----------------------------------------------------------------------
      CALL EZPICK ( 'ENDCAP' )
C----------------------------------------------------------------------
C  ECEM
C----------------------------------------------------------------------
      CALL EZGETS ( 'IUSET_ECEM_PLATE_LABEL', 1, IUSET_LABEL, LEN, IER )
      CALL EZGETS ( 'IUSET_ECEM_PLATE_NAME', 1, IUSET_NAME, LEN, IER )
C----------------------------------------------------------------------
C  ECEM Plate level module shell
C----------------------------------------------------------------------
      IUSET_NV = 1
      CALL EZGETS ( 'ECEM_PLATE_MODULE_VOLUME_NAME', 1,
     &              IUSET_VOLUME_NAME(1), LEN, IER )
      CALL EZGET ( 'IUSET_ECEM_PLATE_MODULE_IDTYPE',
     &              IUSET_IDTYPE(1), IER )
C----------------------------------------------------------------------
C  ECEM Plates
C----------------------------------------------------------------------
      PLATE = 0
      DO LAYER = 1,4
        DO SUBLAYER = 1,NUM_SUBLAYERS(LAYER)
          DO PLATE_IN_SUBLAYER = 1,NUM_PLATES(LAYER)
            IF (LAYER.EQ.4.AND.SUBLAYER.EQ.1.AND.PLATE_IN_SUBLAYER.EQ.5)
     &        GOTO 101
            PLATE = PLATE + 1
            IF (PLATE_IN_SUBLAYER .EQ. 1) THEN
              NUM_MAT = 4
              IF (LAYER.EQ.4) NUM_MAT = 5
              DO MAT_LAYER = 1,NUM_MAT
                IF (PLATE.EQ.19 .AND. MAT_LAYER.NE.1 .AND.
     &            MAT_LAYER.NE.5) GOTO 101
                IUSET_NV = IUSET_NV + 1
C----------------------------------------------------------------------
C  Hardwire in name change for layer 4 (7), sublayer 1, first 2
C   absorber plates. Due to change in material type
C----------------------------------------------------------------------
                WRITE(LABEL,900) PLATE, PLATE_TYPE(MAT_LAYER)
  900           FORMAT('ECEM+Z_',I2.2,'_',A8)
                WRITE(IUSET_VOLUME_NAME(IUSET_NV),901) LAYER,SUBLAYER,
     &              MAT(MAT_LAYER)
  901           FORMAT('E',2I1.1,A1)
                CALL ADDSTR ( 'IUSET_'//LABEL, '_IDTYPE',LABEL,LSTRING)
                CALL EZGET ( LABEL, IUSET_IDTYPE(IUSET_NV), IER )
              ENDDO
            ENDIF
  101       CONTINUE
          ENDDO
        ENDDO
      ENDDO
  100 CONTINUE
C----------------------------------------------------------------------
C  ECEM 1&2 Readout Board
C----------------------------------------------------------------------
      IUSET_NV = IUSET_NV + 1
      CALL EZGETS ( 'ECEM+Z_READOUT_12_MLB', 1,
     &  IUSET_VOLUME_NAME(IUSET_NV), LEN, IER )
      IF(IER.NE.0) WRITE(6,1000) IER, 'NAME'
      CALL EZGET ('IUSET_ECEM+Z_RDT_12_MLB_IDTYPE',
     &  IUSET_IDTYPE(IUSET_NV), IER )
      IUSET_NV = IUSET_NV + 1
      CALL EZGETS ( 'ECEM+Z_READOUT_12_COPPER', 1,
     &  IUSET_VOLUME_NAME(IUSET_NV), LEN, IER )
      IF(IER.NE.0) WRITE(6,1000) IER, 'NAME'
      CALL EZGET ('IUSET_ECEM+Z_RDT_12_CU_IDTYPE',
     &  IUSET_IDTYPE(IUSET_NV), IER )
C----------------------------------------------------------------------
C  Support Pipe
C----------------------------------------------------------------------
      IUSET_NV = IUSET_NV + 1
      CALL EZGETS ( 'ECEM+Z_SUPPORT_PIPE', 1,
     &  IUSET_VOLUME_NAME(IUSET_NV), LEN, IER )
      IF(IER.NE.0) WRITE(6,1000) IER, 'NAME'
      CALL EZGET ('IUSET_ECEM+Z_SUPRT_PIPE_IDTYPE',
     &  IUSET_IDTYPE(IUSET_NV), IER )
C----------------------------------------------------------------------
C  Write the set
C----------------------------------------------------------------------
      CALL WRITE_IUSET
      RETURN
C
 1000 FORMAT(' IER EQ',I3,' IN EZGET FOR ',A12,' IN ECEM_PLATE_IUSET')
 1002 FORMAT('ECEM+Z_',I2.2,'_',A8)
      END
