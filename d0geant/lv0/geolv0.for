      LOGICAL FUNCTION GEOLV0
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Steering routine for LEVEL 0 Geometry
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-
C-   CREATED  27-NOV-1988   Chip Stewart
C-   Updated  14-APR-1989   Chip Stewart  - change MXCAL to MXLV0
C-   Updated   2-JUN-1989   Harrison B. Prosper
C-   Made into a program-builder interface routine
C-   Updated  17-AUG-1989   N.A. Graf VOLORD calls put in
C-   Updated  19-MAR-1992   Freedy Nang   Change to new geometry
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C
      CHARACTER SLNAME*32,SPNAME*32,DVNAME*32,ZEE(2)*1
      CHARACTER*32 BOXNAME, PMTYPE, TRD1NAME
      CHARACTER*32 TBNAME
      INTEGER IW,IX,IZ,IS,IT,IP
      DATA ZEE/'+','-'/
C----------------------------------------------------------------------
C
      GEOLV0 = .TRUE.
      IF ( DLV0 .LT. 1 ) GOTO 999
C
      CALL EZPICK('SRCP_LV0')
      CALL MXLV0                        !Set up LV0 Mixtures
C
C----------------------------------------------------------------------
C
C
C ****  SET UP ROTATION MATRICES
C
      CALL SETROT ('LV0_ROTATION_MATRICES')
C
C ****  GET MOTHER VOLUME
C
      CALL VOLPOS('LV0_MOTHER_VOLUME+1')
      CALL VOLPOS('LV0_MOTHER_VOLUME-1')
C
C ****  LOOP OVER +/- Z
C
      DO IZ = 1,2   !+/- Z
C
C ****  GET TOP/BOTTOM COVER
C
        IX=1
        DO 112 IS = 1 ,1  ! ONE TOP_BOT
          WRITE(TBNAME,161)IS,ZEE(IZ),IX
          CALL VOLPOS(TBNAME)
  112   CONTINUE
        DO IX=1,4    !LOOP OVER COPIES
C
C ****  GET TILES
C
          DO IP = 1,5
            WRITE(SLNAME,101)IP,ZEE(IZ),IX
            CALL VOLPOS(SLNAME)
          END DO
C
C ****  GET COVERS
C
          DO IW=1,6  !LOOP OVER BOXES
            WRITE(BOXNAME,141)IW,ZEE(IZ),IX
            CALL VOLPOS(BOXNAME)
          END DO
          DO 111 IS = 1 ,4  !LOOP OVER TRD1
            WRITE(TRD1NAME,151)IS,ZEE(IZ),IX
            CALL VOLPOS(TRD1NAME)
  111     CONTINUE
C
C ****  GET PMT SIMULATORS
C
          DO IS=1,3  !LOOP OVER PMT TYPES
            WRITE(PMTYPE,171)IS,ZEE(IZ),IX
            CALL VOLPOS(PMTYPE)
          END DO
        ENDDO
C
C ****  GET DIVISIONS
C
        IX=1
        DO IP = 1,5
          WRITE(DVNAME,121)IP,ZEE(IZ),IX
          CALL VOLPOS(DVNAME)
        END DO
      END DO
C
C ****  DO ORDERING FOR TRACKING
C
      CALL VOLORD('LV0_MOTHER_VOLUME+1',3)
      CALL VOLORD('LV0_MOTHER_VOLUME-1',3)
C
C ****  Select previously selected bank
C
      CALL EZRSET
C
C ****  Define Sets and digitization
C
      IF ( DLV0 .GE. 2 ) CALL DETLV0
C
  101 FORMAT('LV0_SLAT',I1,A1,I1)
  121 FORMAT('LV0_DIV',I1,A1,I1)
  141 FORMAT('LV0_BOX_',I1,A1,I1)
  151 FORMAT('LV0_TRD1_',I1,A1,I1)
  161 FORMAT('LV0_TOP_BOT_',I1,A1,I1)
  171 FORMAT('LV0_PMT_',I1,A1,I1)
  999 RETURN
      END
