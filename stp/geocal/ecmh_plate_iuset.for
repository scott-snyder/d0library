      SUBROUTINE ECMH_PLATE_IUSET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  ECMH Detector Sets using Plate level
C-                          volumes.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  26-MAR-1990   Norman A. Amos
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:SRCP_VOLUME.INC'
      INCLUDE 'D0$INC:IUSET.INC'
      INTEGER IER,LEN,ISTEP,NSTEPS
      INTEGER ID,IDTYPE(11),ILEVEL
      CHARACTER VOLNAM*4,STEP*1
      EQUIVALENCE(VOLNAM,VOLUME_NAME)
      DATA ISTEP/1/
C----------------------------------------------------------------------
C-    ECMH Constants.
C----------------------------------------------------------------------
      CALL EZGETS('IUSET_ECMH_PLATE_LABEL',1,IUSET_LABEL,LEN,IER)
      CALL EZGETS('IUSET_ECMH_PLATE_NAME',1,IUSET_NAME,LEN,IER)
      CALL EZGET_iarr('IUSET_ECMH_IDTYPES',IDTYPE,IER)
      IUSET_NV=0
C----------------------------------------------------------------------
C-    ECMH Mother.
C----------------------------------------------------------------------
      CALL EZGET('ECMH_PLATE_MODULE_VOLUME_NAME',VOLUME_NAME,IER)
      CALL EZGET('IUSET_ECMH_PLATE_MODULE_IDTYPE',ID,IER)
      IUSET_NV=IUSET_NV+1
      VOLNAM(4:4)='T'
      IUSET_VOLUME_NAME(IUSET_NV)=VOLNAM
      IUSET_IDTYPE(IUSET_NV)=ID
      IUSET_NV=IUSET_NV+1
      VOLNAM(4:4)='B'
      IUSET_VOLUME_NAME(IUSET_NV)=VOLNAM
      IUSET_IDTYPE(IUSET_NV)=ID
C----------------------------------------------------------------------
C-    ECMH Frontplate.
C----------------------------------------------------------------------
      CALL EZGET('ECMH_FRONTPLATE_VOLUME_NAME',VOLUME_NAME,IER)
      CALL EZGET('IUSET_ECMH_FRONTPLATE_IDTYPE',ID,IER)
      IUSET_NV=IUSET_NV+1
      VOLNAM(4:4)='T'
      IUSET_VOLUME_NAME(IUSET_NV)=VOLNAM
      IUSET_IDTYPE(IUSET_NV)=ID
      IUSET_NV=IUSET_NV+1
      VOLNAM(4:4)='B'
      IUSET_VOLUME_NAME(IUSET_NV)=VOLNAM
      IUSET_IDTYPE(IUSET_NV)=ID
C----------------------------------------------------------------------
C-    ECMH Argon Gaps.
C----------------------------------------------------------------------
      CALL EZGET('ECMH_ARGONGAP_VOLUME_NAME',VOLUME_NAME,IER)
      CALL EZGET('ECMH_NUMBER_STEPS',NSTEPS,IER)
      DO ISTEP=1,NSTEPS
        WRITE (STEP,'(Z1)') ISTEP
        IUSET_NV=IUSET_NV+1
        VOLNAM(3:4)=STEP//'T'
        IUSET_VOLUME_NAME(IUSET_NV)=VOLNAM
        IUSET_IDTYPE(IUSET_NV)=IDTYPE(ISTEP)
        IUSET_NV=IUSET_NV+1
        VOLNAM(3:4)=STEP//'B'
        IUSET_VOLUME_NAME(IUSET_NV)=VOLNAM
        IUSET_IDTYPE(IUSET_NV)=IDTYPE(ISTEP)
      ENDDO
C----------------------------------------------------------------------
C-    ECMH Absorber Plate, Fine and Coarse.
C----------------------------------------------------------------------
      CALL EZGET('ECMH_ABSORBER_VOLUME_NAME',VOLUME_NAME,IER)
      CALL EZGET('IUSET_ECMH_ABSORBER_IDTYPE',ID,IER)
      IUSET_NV=IUSET_NV+1
      VOLNAM(4:4)='T'
      IUSET_VOLUME_NAME(IUSET_NV)=VOLNAM
      IUSET_IDTYPE(IUSET_NV)=ID
      IUSET_NV=IUSET_NV+1
      VOLNAM(4:4)='B'
      IUSET_VOLUME_NAME(IUSET_NV)=VOLNAM
      IUSET_IDTYPE(IUSET_NV)=ID
      IUSET_NV=IUSET_NV+1
      VOLNAM(4:4)='U'
      IUSET_VOLUME_NAME(IUSET_NV)=VOLNAM
      IUSET_IDTYPE(IUSET_NV)=ID
      IUSET_NV=IUSET_NV+1
      VOLNAM(4:4)='D'
      IUSET_VOLUME_NAME(IUSET_NV)=VOLNAM
      IUSET_IDTYPE(IUSET_NV)=ID
C----------------------------------------------------------------------
C-    ECMH Signal Board, Fine and Coarse.
C----------------------------------------------------------------------
      CALL EZGET('ECMH_SIGNALBOARD_VOLUME_NAME',VOLUME_NAME,IER)
      CALL EZGET('IUSET_ECMH_SIBNALBOARD_IDTYPE',ID,IER)
      IUSET_NV=IUSET_NV+1
      VOLNAM(4:4)='T'
      IUSET_VOLUME_NAME(IUSET_NV)=VOLNAM
      IUSET_IDTYPE(IUSET_NV)=ID
      IUSET_NV=IUSET_NV+1
      VOLNAM(4:4)='B'
      IUSET_VOLUME_NAME(IUSET_NV)=VOLNAM
      IUSET_IDTYPE(IUSET_NV)=ID
      IUSET_NV=IUSET_NV+1
      VOLNAM(4:4)='U'
      IUSET_VOLUME_NAME(IUSET_NV)=VOLNAM
      IUSET_IDTYPE(IUSET_NV)=ID
      IUSET_NV=IUSET_NV+1
      VOLNAM(4:4)='D'
      IUSET_VOLUME_NAME(IUSET_NV)=VOLNAM
      IUSET_IDTYPE(IUSET_NV)=ID
C----------------------------------------------------------------------
C-    ECMH Readout Board, Fine and Coarse.
C----------------------------------------------------------------------
      CALL EZGET('ECMH_READOUTBOARD_VOLUME_NAME',VOLUME_NAME,IER)
      CALL EZGET('IUSET_ECMH_READOUTBOARD_IDTYPE',ID,IER)
      IUSET_NV=IUSET_NV+1
      VOLNAM(4:4)='T'
      IUSET_VOLUME_NAME(IUSET_NV)=VOLNAM
      IUSET_IDTYPE(IUSET_NV)=ID
      IUSET_NV=IUSET_NV+1
      VOLNAM(4:4)='B'
      IUSET_VOLUME_NAME(IUSET_NV)=VOLNAM
      IUSET_IDTYPE(IUSET_NV)=ID
      IUSET_NV=IUSET_NV+1
      VOLNAM(4:4)='U'
      IUSET_VOLUME_NAME(IUSET_NV)=VOLNAM
      IUSET_IDTYPE(IUSET_NV)=ID
      IUSET_NV=IUSET_NV+1
      VOLNAM(4:4)='D'
      IUSET_VOLUME_NAME(IUSET_NV)=VOLNAM
      IUSET_IDTYPE(IUSET_NV)=ID
C----------------------------------------------------------------------
C-    ECMH Endplate.
C----------------------------------------------------------------------
      CALL EZGET('ECMH_ENDPLATE_VOLUME_NAME',VOLUME_NAME,IER)
      CALL EZGET('IUSET_ECMH_ENDPLATE_IDTYPE',ID,IER)
      IUSET_NV=IUSET_NV+1
      VOLNAM(4:4)='T'
      IUSET_VOLUME_NAME(IUSET_NV)=VOLNAM
      IUSET_IDTYPE(IUSET_NV)=ID
      IUSET_NV=IUSET_NV+1
      VOLNAM(4:4)='B'
      IUSET_VOLUME_NAME(IUSET_NV)=VOLNAM
      IUSET_IDTYPE(IUSET_NV)=ID
C----------------------------------------------------------------------
      CALL WRITE_IUSET
  999 RETURN
      END
