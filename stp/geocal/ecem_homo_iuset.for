      SUBROUTINE ECEM_HOMO_IUSET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ECEM Detector Sets using homogenized volumes
C-
C-      Create the SRCP structures which define the Detector Sets 
C-      for the ECEM Beam using homogenized volumes
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  01-DEC-1989   Stuart Fuess
C-   Modified 20-MAY-1990   Natalie Roe  Add EM3A,B,C and EM4A,B
C-   Updated  29-JUL-1990   Andy Milder  Fixed labeling (A,B,C) for sublayers
C-   Updated  30-SEP-1990   Andy Milder  Changed stongback from E11A to E41B
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:IUSET.INC'
C  Integers
      INTEGER I
      INTEGER IER
      INTEGER LEN
      INTEGER IDTYPE
C  Characters
      CHARACTER*4 NAME
      CHARACTER*32 LABEL
      CHARACTER*1 LETTER(3)
      DATA LETTER/'A','B','C'/
C----------------------------------------------------------------------
C  Select the EC SRCP file
C----------------------------------------------------------------------
      CALL EZPICK ( 'ENDCAP' )
C----------------------------------------------------------------------
C  ECEM
C----------------------------------------------------------------------
      CALL EZGETS ( 'IUSET_ECEM_HOMO_LABEL',1,IUSET_LABEL,LEN,IER )
      CALL EZGETS ( 'IUSET_ECEM_HOMO_NAME',1,IUSET_NAME,LEN,IER )
C----------------------------------------------------------------------
C  ECEM Homogenized Module Shell
C----------------------------------------------------------------------
      IUSET_NV = 1
      CALL EZGETS ( 'ECEM_HOMO_MODULE_VOLUME_NAME', 1,
     &              IUSET_VOLUME_NAME(1), LEN, IER )
      CALL EZGET ( 'IUSET_ECEM_HOMO_MODULE_IDTYPE', 
     &              IUSET_IDTYPE(1), IER )
C----------------------------------------------------------------------
C  ECEM Floors
C----------------------------------------------------------------------
      DO I=1,2
        IUSET_NV = IUSET_NV + 1
        WRITE (LABEL,1001) I
        CALL EZGETS ( LABEL, 1,
     &              IUSET_VOLUME_NAME(IUSET_NV), LEN, IER )
        WRITE (LABEL,1002) I
        CALL EZGET ( LABEL, IUSET_IDTYPE(IUSET_NV), IER )
      ENDDO
C EM3
      DO I=1,3
        IUSET_NV = IUSET_NV + 1
        WRITE (LABEL,1005) LETTER(I)
        CALL EZGETS ( LABEL, 1,
     &              IUSET_VOLUME_NAME(IUSET_NV), LEN, IER )
        WRITE (LABEL,1006) LETTER(I)
        CALL EZGET ( LABEL, IUSET_IDTYPE(IUSET_NV), IER )
      ENDDO
C EM4
      DO I=1,2
        IUSET_NV = IUSET_NV + 1
        WRITE (LABEL,1007) LETTER(I)
        CALL EZGETS ( LABEL, 1,
     &              IUSET_VOLUME_NAME(IUSET_NV), LEN, IER )
        WRITE (LABEL,1008) LETTER(I)
        CALL EZGET ( LABEL, IUSET_IDTYPE(IUSET_NV), IER )
      ENDDO
C----------------------------------------------------------------------
C  ECEM Ring outside Floors
C----------------------------------------------------------------------
      DO I=1,4
        IUSET_NV = IUSET_NV + 1
        WRITE (LABEL,1003) I
        CALL EZGETS ( LABEL, 1,
     &              IUSET_VOLUME_NAME(IUSET_NV), LEN, IER )
        WRITE (LABEL,1004) I
        CALL EZGET ( LABEL, IUSET_IDTYPE(IUSET_NV), IER )
      ENDDO
C----------------------------------------------------------------------
C  ECEM Support Pipe
C----------------------------------------------------------------------
      IUSET_NV = IUSET_NV + 1
      CALL EZGETS ( 'ECEM+Z_SUPPORT_PIPE', 1,
     &              IUSET_VOLUME_NAME(IUSET_NV), LEN, IER )
      CALL EZGET ( 'IUSET_ECEM+Z_SUPRT_PIPE_IDTYPE', 
     &              IUSET_IDTYPE(IUSET_NV), IER )
C----------------------------------------------------------------------
C  ECEM Strongback 
C----------------------------------------------------------------------
      IUSET_NV = IUSET_NV + 1
      CALL EZGETS ( 'ECEM+Z_11_ABSORBER', 1,
     &              IUSET_VOLUME_NAME(IUSET_NV), LEN, IER )
      IUSET_VOLUME_NAME(IUSET_NV) = 'E41B'
      CALL EZGET ( 'IUSET_ECEM+Z_11_HOMO_ABS_IDTYPE',
     &              IUSET_IDTYPE(IUSET_NV), IER )
C----------------------------------------------------------------------
C  ********THE FOLLOWING IS COMMENTED OUT*********
C----------------------------------------------------------------------
C  EM1&2 READOUT RING 
C----------------------------------------------------------------------
C      IUSET_NV = IUSET_NV + 1
C      CALL EZGETS ( 'ECEM+Z_READOUT_12_MLB', 1,
C     &              IUSET_VOLUME_NAME(IUSET_NV), LEN, IER )
C      CALL EZGET ( 'IUSET_ECEM+Z_RDT_12_MLB_IDTYPE', 
C     &              IUSET_IDTYPE(IUSET_NV), IER )
C      IUSET_NV = IUSET_NV + 1
C      CALL EZGETS ( 'ECEM+Z_READOUT_12_COPPER', 1,
C     &              IUSET_VOLUME_NAME(IUSET_NV), LEN, IER )
C      CALL EZGET ( 'IUSET_ECEM+Z_RDT_12_CU_IDTYPE', 
C     &              IUSET_IDTYPE(IUSET_NV), IER )
C----------------------------------------------------------------------
C  STAINLESS SKIN BEHIND EM4
C----------------------------------------------------------------------
C      IUSET_NV = IUSET_NV + 1
C      IUSET_VOLUME_NAME(IUSET_NV) = 'E42B'
C      CALL EZGET ( 'IUSET_ECEM+Z_19_ABSORBER_IDTYPE', 
C     &              IUSET_IDTYPE(IUSET_NV), IER )
C----------------------------------------------------------------------
C  Write sets
C----------------------------------------------------------------------
      CALL WRITE_IUSET
      RETURN
 1001 FORMAT('ECEM+Z_FLOOR',I1.1,'_VOLUME_NAME')
 1002 FORMAT('IUSET_ECEM+Z_FLOOR',I1.1,'_IDTYPE')
 1003 FORMAT('ECEM+Z_X_FLOOR',I1.1,'_VOLUME_NAME')
 1004 FORMAT('IUSET_ECEM+Z_X_FLOOR',I1.1,'_IDTYPE')
 1005 FORMAT('ECEM+Z_FLOOR3',A1,'_VOLUME_NAME')
 1006 FORMAT('IUSET_ECEM+Z_FLOOR3',A1,'_IDTYPE')
 1007 FORMAT('ECEM+Z_FLOOR4',A1,'_VOLUME_NAME')
 1008 FORMAT('IUSET_ECEM+Z_FLOOR4',A1,'_IDTYPE')
      END
