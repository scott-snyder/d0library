      SUBROUTINE ECMH_PLATE_VOLUMES(NLINES,LINE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ECMH Plate level volumes
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
      INTEGER IGAP,NGAPS,ISTEP
      INTEGER LAST_GAP_PER_STEP(11)
      INTEGER IREADOUT,READOUT_GAP(5)
      CHARACTER LABEL*32,GAP*2
      DATA IREADOUT/1/
      DATA ISTEP/1/
C----------------------------------------------------------------------
C  Select the EC SRCP file
C----------------------------------------------------------------------
      CALL EZPICK ( 'ENDCAP' )
C----------------------------------------------------------------------
C-    Mother.
C----------------------------------------------------------------------
      NLINES=NLINES+1
      LINE(NLINES)='''ECMH_PLATE_MOTHER_VOLUME_TOP'''
      NLINES=NLINES+1
      LINE(NLINES)='''ECMH_PLATE_MOTHER_VOLUME_BOT'''
C----------------------------------------------------------------------
C-    FrontPlate.
C----------------------------------------------------------------------
      NLINES=NLINES+1
      LINE(NLINES)='''ECMH_FRONTPLATE_VOLUME_TOP'''
      NLINES=NLINES+1
      LINE(NLINES)='''ECMH_FRONTPLATE_VOLUME_BOT'''
C----------------------------------------------------------------------
C-    Platelevel Gaps.
C----------------------------------------------------------------------
      CALL EZGET('ECMH_LAST_GAP_PER_STEP',LAST_GAP_PER_STEP,IER)
      CALL EZGET('ECMH_READOUT_GAPS',READOUT_GAP,IER)
      CALL EZGET('ECMH_NUMBER_GAPS',NGAPS,IER)
      DO IGAP=1,NGAPS
        WRITE(GAP,'(I2.2)') IGAP
        IF (IGAP.EQ.1) GOTO 100
        NLINES=NLINES+1
        LINE(NLINES)='''ECMH_ABSORBER'//GAP//'_VOLUME_TOP'''
        NLINES=NLINES+1
        LINE(NLINES)='''ECMH_ABSORBER'//GAP//'_VOLUME_BOT'''
100     NLINES=NLINES+1
        LINE(NLINES)='''ECMH_ARGONGAP'//GAP//'_VOLUME_FTOP'''
        NLINES=NLINES+1
        LINE(NLINES)='''ECMH_ARGONGAP'//GAP//'_VOLUME_FBOT'''
        NLINES=NLINES+1
        LINE(NLINES)='''ECMH_SIGNALBOARD'//GAP//'_VOLUME_TOP'''
        NLINES=NLINES+1
        LINE(NLINES)='''ECMH_SIGNALBOARD'//GAP//'_VOLUME_BOT'''
        NLINES=NLINES+1
        LINE(NLINES)='''ECMH_ARGONGAP'//GAP//'_VOLUME_BTOP'''
        NLINES=NLINES+1
        LINE(NLINES)='''ECMH_ARGONGAP'//GAP//'_VOLUME_BBOT'''
        IF (IGAP.EQ.READOUT_GAP(IREADOUT)) THEN
          IREADOUT=IREADOUT+1
          NLINES=NLINES+1
          LINE(NLINES)='''ECMH_READOUTBOARD'//GAP//'_VOLUME_TOP'''
          NLINES=NLINES+1
          LINE(NLINES)='''ECMH_READOUTBOARD'//GAP//'_VOLUME_BOT'''
        ENDIF
        IF (IGAP.EQ.LAST_GAP_PER_STEP(ISTEP)) ISTEP=ISTEP+1
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
