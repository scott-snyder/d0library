      SUBROUTINE ZTRAKS(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX,PT,NZTR,ZLINKR)
C----------------------------------------------------------------------
C
C  Find central tracks in a road 
C
C  Inputs  : 
C     ZVTX   = vertex position in Z
C     PHIMIN   = minimum phi
C     PHIMAX   = maximum phi
C     THEMIN   = minimum theta
C     THEMAX   = maximum theta
C     PT       = transverse momentum of muon or electron candidate
C
C  Outputs:
C     NZTR:    number of Central Detector tracks in the road 
C     ZLINKR(I): link of Central Detector track in the road
C
C   1-FEB-1989   Daria Zieminska
C-   Updated  10-NOV-1989   Qizhong Li-Demarteau   changed for one road,
C-                             use reference links instead of track ID's
C-                             and regions for CDC+VTX, FDC+VTX and FDC-
C-                             only are calculated from vertex position
C-   Updated  06-JAN-1990   Qizhong Li-Demarteau  add ZTRKs containing 
C-                                  non-matched CDC or FDC or VTX tracks
C-                                 and added an input argument ZVTX 
C-   Updated  16-AUG-1990   Qizhong Li-Demarteau   added call to ZTCHCK
C-                                       and a choice to global fitting
C-   Updated   4-FEB-1991   Daria Zieminska  add CALL ZTRALL
C-   Updated  20-MAR-1991   Qizhong Li-Demarteau  add FCROAD for cosmics
C-                                         and switches for subdetectors
C-   Updated  26-APR-1991   Jeffrey Bantly   call FDROAD with z-vertex 
C-                                value to calculate FDC roads correctly
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated  18-JUL-1991   Qizhong Li-Demarteau  added a switch REDOCD 
C-   Updated  29-AUG-1991   Qizhong Li-Demarteau  update compressed hits banks
C-   Updated   2-SEP-1991   Susan K. Blessing  Calculation of NZTR was in 
C-    the wrong place.
C-   Updated  27-SEP-1991   Qizhong Li-Demarteau  added GAP_VTONLY switch 
C-   Updated  24-JUL-1992   Qizhong Li-Demarteau  added ZVTX in the calls
C-                                                to CDROAD and VTROAD
C-   Updated   4-SEP-1992   Qizhong Li-Demarteau  added switch ZFTVER and
C-                                             call ZTRKVT to fill FITVER
C-   Updated  29-DEC-1992   Qizhong Li-Demarteau  removed call ZTRALL and
C-                                             increased MAXTRK and ZMAX
C-   Updated  17-MAR-1993   Qizhong Li-Demarteau  moved MAXTRK and ZMAX
C-                                             to PARAMS and INC files 
C-   Updated  10-JUL-1995   Srini Rajagopalan  Add call to ZTRK_IN_ROAD at the
C-                          end; check if the global fit track is within road.
C
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZTRLNK.INC/LIST'
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'
      INTEGER NC,NF,NV,ERR, IZ
      INTEGER NZCV, NZFV, NZFC, NZF, NZC, NZV, NZ, NZTR, ZLINKR(ZMAX)
      INTEGER IDC(MAXTRK), IDF(MAXTRK), IDV(MAXTRK)
      INTEGER IER
      LOGICAL VTDONE, FDDONE, VTONLY, CDONLY, FDONLY, FIRST, MKZFIT
      LOGICAL VTXON, CDCON, FDCON, COSMIC, REDOCD
      LOGICAL EZERROR, GAP_VTONLY, ZFTVER
      REAL PHIMIN,PHIMAX,THEMIN,THEMAX,PT, ZVTX
      REAL P1,P2,T1,T2, THE1, THE2, THE3, THE4
      REAL    THETCV(2), THETFV(2), THETFF(2)
      REAL    ERRVER(3), FITVER(3)
      INTEGER RUN, ID, RUNSAV, IDSAV
      SAVE RUNSAV, IDSAV, FIRST
      DATA RUNSAV,IDSAV/-1,-1/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('ZTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','ZTRAKS',
     &    'Unable to find bank ZTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('GAP_VTONLY',GAP_VTONLY,ERR)
        CALL EZGET('VTONLY',VTONLY,ERR)
        CALL EZGET('CDONLY',CDONLY,ERR)
        CALL EZGET('FDONLY',FDONLY,ERR)
        CALL EZGET('COSMIC',COSMIC,ERR)
        CALL EZGET('MKZFIT',MKZFIT,ERR)
        CALL EZGET('ZFTVER',ZFTVER,ERR)
        CALL EZGET('VTXON',VTXON,ERR)
        CALL EZGET('CDCON',CDCON,ERR)
        CALL EZGET('FDCON',FDCON,ERR)
        CALL EZGET('REDOCD',REDOCD,ERR)
        CALL EZRSET
      ENDIF
C
C   Give user a choice to REDO the reconstruction for the Central Detector
C   from STA file
C     (default value for REDOCD is .false.)
C
      IF (REDOCD) CALL ZDDROP
C
      CALL ZFPARH(PHIMIN,PHIMAX,THEMIN,THEMAX,PT)
C
C ****  Initialize a temporary LINK area
C
      IF (ZTRLNK(1) .EQ. 0) 
     &  CALL MZLINT(IXCOM,'/ZTRLNK/',ZTRLNK,ZLINKS(ZMAX),ZTRLNK)
C
      NV = 0
      NC = 0
      NF = 0
      NZ = 0
      NZCV = 0
      NZFV = 0
      NZC  = 0
      NZV  = 0
      NZF  = 0
      P1=PHIMIN
      P2=PHIMAX
C
C  to make sure that T1 is always smaller than T2
C
      IF (THEMIN .LE. THEMAX) THEN
        T1 = THEMIN
        T2 = THEMAX
      ELSE
        T2 = THEMIN
        T1 = THEMAX
      ENDIF
C
C     check all ZTRKs, which built previoursly, to see if any ZTRK belong
C     to this road. 
C
      CALL ZTCHCK(P1,P2,T1,T2,NZ)
C
C     calculate the theta boundaries for CV road, FV road and FF road
C     from the vertex position
C
      CALL EVNTID(RUN,ID)
      IF(RUN.NE.RUNSAV.OR.ID.NE.IDSAV) THEN
        RUNSAV=RUN
        IDSAV=ID
        CALL ZSEPRT(ZVTX,THETCV,THETFV,THETFF)
      ENDIF
C
      VTDONE = .FALSE.
      FDDONE = .FALSE.
C 
C  Find CDC tracks and VTX tracks on this road. 
C  Match CDC tracks with VTX tracks to make central detector tracks
C
      IF (T1 .LE. THETCV(2) .AND. T2 .GE. THETCV(1)) THEN
        IF (CDCON) CALL CDROAD(ZVTX,P1,P2,T1,T2,NC,IDC) 
        IF (VTXON) THEN
          CALL VTROAD(ZVTX,P1,P2,T1,T2,NV,IDV) 
          VTDONE = .TRUE.
        ENDIF
        IF (CDCON .AND. VTXON) THEN
          CALL CVROAD(NC,IDC,NV,IDV,PT,NZ,NZCV)
          NZ = NZ + NZCV
        ENDIF
        IF (CDCON .AND. CDONLY) THEN
          CALL ZTRCDC(NC,IDC,NZ,NZC)
          NZ = NZ + NZC
        ENDIF
      ENDIF
C 
C  Find FDC tracks and VTX tracks on this road. 
C  Match FDC tracks with VTX tracks to make central detector tracks
C
      IF (T1 .LT. THETCV(1) .AND. T2 .GE. THETFV(1) 
     &   .OR. T1 .LE. THETFV(2) .AND. T2 .GE. THETCV(2)) THEN
        IF (FDCON) THEN
          CALL FDROAD(ZVTX,P1,P2,T1,T2,NF,IDF) 
          FDDONE = .TRUE.
        ENDIF
        IF (.NOT.VTDONE .AND. VTXON) THEN
          CALL VTROAD(ZVTX,P1,P2,T1,T2,NV,IDV) 
          VTDONE = .TRUE.
        ENDIF
        IF (FDCON .AND. VTXON) THEN 
          CALL FVROAD(NF,IDF,NV,IDV,PT,NZ,NZFV)
          NZ = NZ + NZFV
        ENDIF
        IF (FDONLY .AND. FDCON) THEN
          CALL FFROAD(NF,IDF,NZ,NZF)
          NZ = NZ + NZF
        ENDIF
      ENDIF
C 
C  Find FDC tracks on this road (outside VTX volume) 
C  Make central detector tracks containing only FDC tracks
C
      IF (T1 .LE. THETFV(1) .OR. T2 .GE. THETFV(2)) THEN
        IF (.NOT. FDDONE .AND. FDONLY .AND. FDCON) THEN
          CALL FDROAD(ZVTX,P1,P2,T1,T2,NF,IDF) 
          CALL FFROAD(NF,IDF,NZ,NZF)
          NZ = NZ + NZF
        ENDIF
      ENDIF
C
      IF (VTDONE .AND. VTXON) THEN
        IF (VTONLY) THEN
          CALL ZTRVTX(NV,IDV,NZ,NZV)
          NZ = NZ + NZV
        ELSE
C
C   If GAP_VTONLY is true, use non-matched VTX tracks when the road 
C   is not completely inside of CDC or FDC.
C
          IF (GAP_VTONLY) THEN
            CALL ZCDGAP(ZVTX,T1,T2,THE1,THE2,THE3,THE4)
            IF (THE1 .GT. 0.0) THEN
              CALL ZTRVTX_REGION(NV,IDV,THE1,THE2,NZ,NZV)
              NZ = NZ + NZV
            ENDIF
            IF (THE3 .GT. 0.0) THEN
              CALL ZTRVTX_REGION(NV,IDV,THE3,THE4,NZ,NZV)
              NZ = NZ + NZV
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
C    If it is a cosmic ray run, try to match tracks between FDC+CDC too
C
      IF (COSMIC .AND. CDCON .AND. FDCON) THEN
        CALL FCROAD(NF,IDF,NC,IDC,PT,NZ,NZFC)
        NZ = NZ + NZFC
      ENDIF
C
      NZTR = MIN(ZMAX,NZ)
C
C   make global fitting for each ZTRK 
C   (the fitting does not include vertex point)
C      
      IF (MKZFIT) THEN
        CALL ZTRKVT(ZVTX,FITVER,ERRVER)
        IF (.NOT.ZFTVER) ERRVER(1) = 9999.0
        DO 201 IZ = 1, NZTR
          CALL ZTRKFT(ZLINKS(IZ),FITVER,ERRVER)
  201   CONTINUE
      ENDIF
C
C   update compressed hits banks with ZTRK number
C      
      DO 202 IZ = 1, NZTR
        CALL ZSTCMP(ZLINKS(IZ))
  202 CONTINUE
C
C      CALL UCOPY(ZLINKS(1),ZLINKR(1),NZTR)
C
C Check all the built ZTRK tracks to see if they fall inside the road.
C Return links to only those tracks which fall inside...
C
      CALL ZTRK_IN_ROAD(ZVTX,P1,P2,T1,T2,NZTR,ZLINKR)
C
C  deactivate the temporary link area
C
      ZTRLNK(1) = 0
C
 999  RETURN
      END
