      FUNCTION FTRAKS()
C------------------------------------------------------------------
C
C    Purpose and Methods : Find all FDC tracks.
C
C-   Created  xx-JAN-1989   Daria Zieminska
C-   Updated  many time by many people.
C-   Updated  21-OCT-1993   Robert E. Avery  Now just calls FDROAD
C-      with wide open road. 
C-   Updated  18-NOV-1993   Robert E. Avery  Fetch pointer for FTRH! 
C-
C------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER ZMAX 
      PARAMETER( ZMAX = 450 )
      INTEGER NF,IDF(ZMAX)
      INTEGER IER
      INTEGER RUN,ID,RUNSAV,IDSAV
      INTEGER LKFTRH,GZFTRH
      INTEGER FDCRECO
C
      REAL ZVTX,THEMIN,THEMAX,PHIMIN,PHIMAX
C
      LOGICAL FTRAKS
      LOGICAL EZERROR
      LOGICAL FIRST
      LOGICAL FULL_HITFINDING
C
      SAVE FIRST
      SAVE FDCRECO,FULL_HITFINDING
      SAVE RUNSAV,IDSAV
      SAVE ZVTX,THEMIN,THEMAX,PHIMIN,PHIMAX
C
      DATA FIRST/.TRUE./
      DATA FDCRECO/3/
C
C---------------------------------------------------------------------------
C
      FTRAKS = .TRUE.
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('FTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('FTRAKS','FTRAKS','FTRAKS_RCP not found.','W')
        ELSE
          CALL EZGET('FDCRECO',FDCRECO,IER)
          CALL EZRSET
        ENDIF
        ZVTX = 0.
        THEMIN = 0.
        THEMAX = PI
        PHIMIN = 0.
        PHIMAX = TWOPI
        FULL_HITFINDING = .TRUE.
      END IF
C
C  make sure the FDC full reconstruction is done once per event only
C
      CALL EVNTID(RUN,ID)
      IF (RUN .NE. RUNSAV .OR. ID .NE. IDSAV) THEN
        RUNSAV = RUN
        IDSAV = ID
      ELSE
        GOTO 999
      ENDIF
C
C Do Full Hitfinding
C
      CALL FDC_HITS( FULL_HITFINDING ) 
C
C Do Trackfinding with wide open road.
C
      CALL FDROAD(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX,NF,IDF)
C
C Mark full tracking as done.
C
      IF (FDCRECO.EQ.3) THEN
        LKFTRH = GZFTRH()
        IF ( LKFTRH.GT.0 ) IQ(LKFTRH) = IBSET(IQ(LKFTRH),IDONE)
      ENDIF
C
  999 CONTINUE
      RETURN
      END
