      SUBROUTINE ECMH_HOMO_VOLUMES(NLINES,LINE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ECMH Homogenous level volumes
C-
C-   Inputs  : NLINES Current index in LINE array
C-             LINE   Character array
C-   Outputs : NLINES Updated index in LINE array
C-   Controls: 
C-
C-   Created  31-MAR-1990   Norman A. Amos
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NLINES
      CHARACTER*(*) LINE(*)
      INTEGER IER,LEN
      INTEGER ISTEP,NSTEPS
      CHARACTER STEP*1
C----------------------------------------------------------------------
C  Select the EC SRCP file
C----------------------------------------------------------------------
      CALL EZPICK ( 'ENDCAP' )
C----------------------------------------------------------------------
C-    Mother.
C----------------------------------------------------------------------
      NLINES=NLINES+1
      LINE(NLINES)='''ECMH_HOMO_MOTHER_VOLUME_TOP'''
      NLINES=NLINES+1
      LINE(NLINES)='''ECMH_HOMO_MOTHER_VOLUME_BOT'''
C----------------------------------------------------------------------
C-    FrontPlate.
C----------------------------------------------------------------------
      NLINES=NLINES+1
      LINE(NLINES)='''ECMH_FRONTPLATE_VOLUME_TOP'''
      NLINES=NLINES+1
      LINE(NLINES)='''ECMH_FRONTPLATE_VOLUME_BOT'''
C----------------------------------------------------------------------
C-    Homogenized STEPs.
C----------------------------------------------------------------------
      CALL EZGET('ECMH_NUMBER_STEPS',NSTEPS,IER)
      DO ISTEP=1,NSTEPS
        WRITE(STEP,'(Z1)') ISTEP
        NLINES=NLINES+1
        LINE(NLINES)='''ECMH_HOMO_STEP'//STEP//'_VOLUME_TOP'''
        NLINES=NLINES+1
        LINE(NLINES)='''ECMH_HOMO_STEP'//STEP//'_VOLUME_BOT'''
      ENDDO
C----------------------------------------------------------------------
C-    EndPlate.
C----------------------------------------------------------------------
      NLINES=NLINES+1
      LINE(NLINES)='''ECMH_ENDPLATE_VOLUME_TOP'''
      NLINES=NLINES+1
      LINE(NLINES)='''ECMH_ENDPLATE_VOLUME_BOT'''
C-
  999 RETURN
      END
