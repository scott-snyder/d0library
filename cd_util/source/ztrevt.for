      FUNCTION ZTREVT() 
C------------------------------------------------------------------
C 
C  Event processing routine for the ZTRAKS package 
C  Does full Central Detector track finding if FULL_TRACKING=.TRUE. 
C
C  Daria Zieminska OCT. 1989
C-   Updated  14-JUN-1990   Norman Graf  Added arguments to TTRAKS 
C-   Updated   4-FEB-1991   Daria Zieminska  Call VERTEX 
C-   Updated  20-MAR-1991   Qizhong Li-Demarteau  added a choise for
C-                                                cosmic ray runs
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated  18-JUL-1991   Qizhong Li-Demarteau  added a switch REDOCD 
C-   Updated   2-NOV-1991   Qizhong Li-Demarteau  added a choice to open
C-                                             road to full phi and theta
C-   Updated   1-DEC-1991   Qizhong Li-Demarteau  moved ZTRHIS to end of the
C-                                          end of event process hook ZCLEAN
C-   Updated   3-MAR-1992   Susan K. Blessing  Set full tracking done bits
C-    in ZTRH, DTRH, FTRH, VTRH if FULL_TRACKING option is chosen.
C-   Updated  26-MAR-1992   Qizhong Li-Demarteau  make COSMIC part independent
C-   Updated  28-DEC-1992   Qizhong Li-Demarteau  removed non-used all_tracks 
C-   Updated  08-JAN-1992   Jean-Francois Glicenstein Add TRD updates.
C-   Updated  20-APR-1993   Qizhong Li-Demarteau  use ZMAX parameter
C-   Updated  25-OCT-1993   Robert E. Avery  Call full hitfinding for 
C-      every event. 
C-      Add REDOROADS option for testing purposes.
C                            
C------------------------------------------------------------------
      IMPLICIT NONE  
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER IER
      INTEGER ILINKS(3), ZMAX    
      PARAMETER( ZMAX = 450 )
      INTEGER ZLINKS(ZMAX), NZ
      INTEGER LVERT, GZVERT
      INTEGER LZTRH,LDTRH,LFTRH,LVTRH
      INTEGER GZZTRH,GZDTRH,GZFTRH,GZVTRH
      INTEGER ROAD,NROADS
      INTEGER GZPARH,LPARH,LROAD
      INTEGER IZV,NZV
      INTEGER RUN,ID,RUNSAV,IDSAV
C
      REAL    NEWPI, NEW2PI
      REAL    ZVTX, PHI       
      REAL    ZV(5), DZV(5)
C
      INTEGER MAXR
      PARAMETER( MAXR = 20 )
      REAL    PHIMIN(MAXR),PHIMAX(MAXR),THEMIN(MAXR),THEMAX(MAXR)
      REAL    PT(MAXR)
C
      LOGICAL ZTREVT, FIRST, CALL_TRD, OK 
      LOGICAL CALL_VERTEX, VERTEX
      LOGICAL COSMIC, REDOCD, FULL_TRACKING
      LOGICAL FULL_HITFINDING
      LOGICAL TRDON
      LOGICAL EZERROR
      LOGICAL TRD_UPDATE_EVENT
      LOGICAL REDOROADS
C
      SAVE FIRST
      DATA FIRST/.TRUE./
      DATA ILINKS/3*0/     
      DATA FULL_HITFINDING /.TRUE./
C
C------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('ZTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','ZTREVT',
     &    'Unable to find bank ZTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('FULL_TRACKING',FULL_TRACKING,IER)
        CALL EZGET('FULL_HITFINDING',FULL_HITFINDING,IER)
        CALL EZGET('CALL_TRD',CALL_TRD,IER)
        CALL EZGET('CALL_VERTEX',CALL_VERTEX,IER)
        CALL EZGET('COSMIC',COSMIC,IER)
        CALL EZGET('TRDON',TRDON,IER)
        CALL EZGET('REDOCD',REDOCD,IER)
        CALL EZGET('REDOROADS',REDOROADS,IER)
        CALL EZRSET
        NEWPI = PI
        NEW2PI = TWOPI
      END IF
C
C Should only need to be called once per event:
C
      CALL EVNTID(RUN,ID)
      IF (RUN .NE. RUNSAV .OR. ID .NE. IDSAV) THEN
        RUNSAV = RUN
        IDSAV = ID
      ELSE
        GOTO 100
      ENDIF
C
C   Give user a choice to REDO the reconstruction for the Central Detector
C   from STA file
C     (default value for REDOCD is .false.)
C
      IF (REDOCD) CALL ZDDROP           
C
      CALL ZTR_HITS( FULL_HITFINDING ) 
C
      IF (CALL_VERTEX) THEN
        OK=VERTEX() 
      END IF
C     Updates TRD canary and HV information
      IF (TRDON) THEN
        OK=TRD_UPDATE_EVENT()
      END IF
C
      IF (FULL_TRACKING) THEN
        ZVTX = 0.0
        LVERT = GZVERT(1)
        IF (LVERT .GT. 0) ZVTX = Q(LVERT + 5)
        CALL ZTRAKS(ZVTX,0.0,NEW2PI,0.0,NEWPI,1.,NZ,ZLINKS)
        IF (CALL_TRD .AND. TRDON) CALL TTRAKS(ILINKS)
C
C Set bits in xTRH banks indicating that full tracking has been done
C ZTRH:bit 12, DTRH:bit 12, FTRH:bit 2, VTRH:bit 2
        LZTRH = GZZTRH()
        IF (LZTRH.GT.0) IQ(LZTRH) = IBSET(IQ(LZTRH),12)
C
        LDTRH = GZDTRH()
        IF (LDTRH.GT.0) IQ(LDTRH) = IBSET(IQ(LDTRH),12)
C          
        LFTRH = GZFTRH()
        IF (LFTRH.GT.0) IQ(LFTRH) = IBSET(IQ(LFTRH),2)
C          
        LVTRH = GZVTRH()
        IF (LVTRH.GT.0) IQ(LVTRH) = IBSET(IQ(LVTRH),2)
C          
      ELSEIF (COSMIC) THEN
        CALL ZCOSMC(0.0,NEW2PI,0.0,NEWPI,0.1,NZ,ZLINKS)
        IF (CALL_TRD .AND. TRDON) CALL TTRAKS(ILINKS)
C
      ELSEIF ( REDOROADS ) THEN
C
C This option is for test purposes.
C
C Save and zero number of roads:
        LPARH = GZPARH()
        IF (LPARH.LE.0) GOTO 100
        IF (IQ(LPARH+1).LT.1) GOTO 100
        NROADS = MIN(IQ(LPARH+10),MAXR)
        DO  ROAD =  1, NROADS
          LROAD = LPARH + 11 + 5*(ROAD-1)
          PHIMIN(ROAD)      = Q(LROAD)  
          PHIMAX(ROAD)      = Q(LROAD+1)
          THEMIN(ROAD)      = Q(LROAD+2)
          THEMAX(ROAD)      = Q(LROAD+3)
          PT(ROAD)          = Q(LROAD+4)
        ENDDO
        IQ(LPARH+10) = 0
C
C Now redo tracking in roads:       
        CALL ZVERTE(NZV,ZV,DZV)
        IF (NZV.EQ.0) THEN
          NZV = 1
          ZV(1)=0.
        ENDIF
        DO IZV = 1, NZV
          DO  ROAD =  1, NROADS
            CALL ZTRAKS(ZV(IZV),PHIMIN(ROAD),PHIMAX(ROAD),
     &        THEMIN(ROAD),THEMAX(ROAD),PT(ROAD),NZ,ZLINKS)
          ENDDO
        ENDDO
C
      ENDIF
C
  100 ZTREVT=.TRUE.
  999 RETURN 
      END       
