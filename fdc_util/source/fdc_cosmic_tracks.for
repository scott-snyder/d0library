C VAX/DEC CMS REPLACEMENT HISTORY, Element FDC_COSMIC_TRACKS.FOR
C *1     4-NOV-1993 10:52:30 AVERY "FDC changes for v12 RECO"
C VAX/DEC CMS REPLACEMENT HISTORY, Element FDC_COSMIC_TRACKS.FOR
      SUBROUTINE FDC_COSMIC_TRACKS(TZERO)
C------------------------------------------------------------------
C
C    Purpose and Methods : Find all FDC tracks in a COSMIC ray event.
C    Allow for a free t0 in all track fits. This is stored in track bank
C    for each FDC track. 
C
C-   Outputs : TZERO      first guess, just based on earliest hit.
C-             FTRH,FDCT  banks filled.
C
C    Note: If standard FDC tracking has already been done, we drop all 
C    those banks and start from scratch.
C
C    Note2: Structure of track bank is expanded, in order to 
C    allow space for t0. 
C    The words stored in FDCT for these tracks are:
C       +1 to +25  Same as standard FDCT (zee FDCT.ZEB)
C       +26        t0 from fit (ns)
C       +27        error on par(5), t0
C       +28 TO 32  Covariance of t0 w. parameters 1 to 5
C                  (i.e. C(1,5) ... C(5,5) )
C
C-   Created  28-MAY-1993   Robert E. Avery
C               This code is based on FTRAKS.FOR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:FLOCAL.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
C
C  Ouput:
C
      REAL    TZERO
C
C  Local:
C
      INTEGER HALF,LAYER
      INTEGER NTRSG
      INTEGER NLADD,NSEGML(0:1,0:2)
      INTEGER LADDRS(0:2,MXTRAK),ICALL
      INTEGER NTOT
      INTEGER IER
      INTEGER MAX_HITS
      INTEGER RUN,ID,RUNSAV,IDSAV
      INTEGER LKFTRH,LKFDCH
      INTEGER GZFTRH,GZFDCH
C
      REAL    ZVTX,THEMIN,THEMAX,PHIMIN,PHIMAX
      REAL    TZERO_SAVE
C
      LOGICAL EZERROR
      LOGICAL NEW_RUN 
      LOGICAL DO_T0HITS
      LOGICAL CLEAN_NOISE 
C
      SAVE TZERO_SAVE,DO_T0HITS,CLEAN_NOISE 
      SAVE ZVTX,THEMIN,THEMAX,PHIMIN,PHIMAX
C
      DATA ICALL/0/
      DATA MAX_HITS /1000/
      DATA DO_T0HITS/.TRUE./
      DATA CLEAN_NOISE /.TRUE./
C
C---------------------------------------------------------------------------
C
C  make sure the FDC full cosmic reconstruction is done once per event only
C
      CALL EVNTID(RUN,ID)
      NEW_RUN = RUN .NE. RUNSAV 
      IF (RUN .NE. RUNSAV .OR. ID .NE. IDSAV) THEN
        RUNSAV = RUN
        IDSAV = ID
      ELSE
        TZERO = TZERO_SAVE
        GOTO 999
      ENDIF
C
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('FTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('FTRAKS','FTRAKS','FTRAKS_RCP not found.','W')
        ELSE
          CALL EZGET('DO_T0HITS',DO_T0HITS,IER)
          CALL EZGET('CLEAN_NOISE',CLEAN_NOISE ,IER)
          CALL EZRSET
        ENDIF
        CALL VZERO(LADDRS,3*MXTRAK)
        ZVTX = 0.
        THEMIN = 0.
        THEMAX = PI
        PHIMIN = 0.
        PHIMAX = TWOPI
        ICALL = 1
      END IF
C
C  Drop old fdc hit banks first.
C
      LKFDCH = GZFDCH()
      IF (LKFDCH.GT.0) THEN
        CALL MZDROP(IXCOM,LKFDCH,' ')
        CALL MZGARB(IXMAIN,0)
      ENDIF
C
C  Perform full hitfinding.
C
      CALL FDC_HITS( .TRUE. ) 
C Mark all sectors as 'on road':
      CALL FRHITS(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX)     
C
C First guess at t0, based on earliest hit:
C
      IF ( DO_T0HITS ) THEN
        CALL FDC_T0HITS(TZERO)
        TZERO_SAVE = TZERO 
      ENDIF
C
C Initialize a temporary link area
      CALL MZLINT(IXCOM,'/FLOCAL/',FLOCAL,LFLOC,FLOCAL)
C Create stand alone bank for use in later routines
      CALL MZBOOK(IXMAIN,LFLOC,0,2,'FLOC',9,9,10,2,0)
C
C Loop over both halves of FDC to find tracks
C
      CALL VZERO(NSEGML(0,0),6)
      DO HALF = 0,MXHALF
C
C Find FDC segments
C
        CALL TSEGME(HALF,NSEGML)     ! Find THETA segments
        CALL PSEGME(HALF,NSEGML)     ! Find PHI segments
C
C Delete segments orginating from noise
C
        IF ( CLEAN_NOISE ) THEN
          CALL FCLEAN_NOISE(HALF)
        ENDIF
C
C Make FDC track candidates by matching segments
C
        DO LAYER =  0, 2
          IF (NSEGML(HALF,LAYER).EQ.0) GOTO 100
        ENDDO
C
C Form THREE layer ladders from unused segments using overlapping sectors
C
        NLADD = 0
        CALL FOVERLAD_3(HALF,NLADD,LADDRS)
C
        IF (NLADD.GT.0) THEN
          CALL FTR_TFLOAT(HALF,NLADD,LADDRS)
          CALL VZERO(LADDRS,3*NLADD)
        END IF
C
  100   CONTINUE                          
      ENDDO
C
C Drop User bank when done
C
      CALL MZDROP(IXCOM,LFLOC,'L')
      FLOCAL(1) = 0
C
C Calculate ionization for FDC tracks
      CALL FIONIZ

C Mark status bits for FDC tracks
C
      CALL FMARK_TRKHITS
C
C Flag FDC full tracking done.
C
      IQ(LKFTRH) = IBSET(IQ(LKFTRH),IDONE)
C
C Set status bits in FHIT bank
C
      CALL FHITST
C
C------------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
