      SUBROUTINE ECMH_HOMO_IUSET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  ECMH Detector Sets using Homogenous level
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
      INTEGER ID,IDTYPE(11)
      CHARACTER VOLNAM*4,STEP*1
      EQUIVALENCE(VOLNAM,VOLUME_NAME)
C----------------------------------------------------------------------
C-    ECMH Constants.
C----------------------------------------------------------------------
      CALL EZGETS('IUSET_ECMH_HOMO_LABEL',1,IUSET_LABEL,LEN,IER)
      CALL EZGETS('IUSET_ECMH_HOMO_NAME',1,IUSET_NAME,LEN,IER)
      CALL EZGET('IUSET_ECMH_IDTYPES',IDTYPE,IER)
      IUSET_NV=0
C----------------------------------------------------------------------
C-    ECMH Module
C----------------------------------------------------------------------
      CALL EZGET('ECMH_HOMO_MODULE_VOLUME_NAME',VOLUME_NAME,IER)
      CALL EZGET('IUSET_ECMH_HOMO_MODULE_IDTYPE',ID,IER)
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
C-    ECMH STEPs.
C----------------------------------------------------------------------
      CALL EZGET('ECMH_HOMO_VOLUME_NAME',VOLUME_NAME,IER)
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
C-
      CALL WRITE_IUSET
  999 RETURN
      END
