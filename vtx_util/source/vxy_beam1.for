      SUBROUTINE VXY_BEAM1(ZVTX,XB,DXB,YB,DYB,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the transverse beam position in D0 
C-               coordinates for a given run number and z-coordinate of event
C-               vertex. 
C
C ****  BEAM CENTERS RETRIEVED FROM VERT BANK IF RUN DEPENDANT BEAM POSITION
C ****  IS PRESENT
C
C-
C-   Inputs  : ZVTX  z-coordinate in cm.
C-   Outputs : XB,YB   -- beam position in D0-frame -- cm
C-             DXB,DYB -- beam position errors in D0-frame -- cm
C-             STATUS  -- 0 This run has exact beam position (OR MC)
C-                         >0 : 
C-                         -1 : USE RUN DEPENDANT BEAM INFO IN VERH BANK
C-   Controls: 
C-
C-   Created  22-JUN-1994   Ed Oltman
C-   Updated   2-SEP-1994   Ed Oltman  fixed bug: INRCP error handling
C-   Updated   9-NOV-1994   Justin Bendich, (note by Liang-ping Chen)
C-                          add ENTRY VXY_AUX
C-   Updated  24-JAN-1995   Ed Oltman  increase size of arrays 2000 --> 2500
C-   Updated  26-JAN-1995   Ed Oltman  Use VERT bank if appropriate, put EZRSET
C-                          in, use same errors Justin has in XYVERT, update
C-                          VTX_AUX for XYRCP's benifit..
C-   Updated  30-JAN-1995   Ed Oltman  USE BEAM SLOPE IN VERH BANK 
C-   Updated  10-MAY-1995   Liang-ping Chen  For MC, get beam XY from VERT bank 
C-                          or VERTEX_RCP if VERT does not exist, use errors
C-                          from VERTEX_RCP if those from VERT are zero.
C-                          For real data, read in the ASCII file once
C-                          for the first data run which needs it.  
C-                          
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
c I/O:
      REAL ZVTX,XB,DXB,YB,DYB
      INTEGER STATUS,ARUN, ANT
c Locals:
      LOGICAL FIRST,OK,READ_ASCII
      INTEGER LRCP,ERR,LUN,LEN,IUSER,IDATE,ITIME,TZERO,TIME,RUN,TOTAL,IR
      INTEGER MAX_RUNS,STATUS_LAST,N,LAST
      REAL    BEAM_POS(3),BEAM_SLOP(2),BEAM_ERR(2)
      REAL    BMSL_ERR(2),IP_SIGMA(2)
      REAL    X,DX,Y,DY,SX,DSX,SY,DSY,HOUR,DAY
      REAL    XB_LAST,YB_LAST,SX_LAST,SY_LAST
      CHARACTER*80 FILENAME,TXT*24
      PARAMETER(MAX_RUNS = 3000)
      INTEGER NTracks(MAX_RUNS) 
      INTEGER RUNS(MAX_RUNS)
      REAL    XBEAM(MAX_RUNS),YBEAM(MAX_RUNS)
      REAL    DXBEAM(MAX_RUNS),DYBEAM(MAX_RUNS)
      REAL    SXBEAM(MAX_RUNS),SYBEAM(MAX_RUNS),HOURS(MAX_RUNS)
      REAL    ZVTX_AVE,DX_LAST,DY_LAST
      REAL    AERRX,AERRY,ASLOPX,ASLOPY
      INTEGER IER,LVERH,VERS
      INTEGER LVERT, GZVERT 
c Externals
      REAL    D3UXH
      INTEGER RUNNO,GZVERH
c Data:
      DATA FIRST/.TRUE./, READ_ASCII/.TRUE./
      DATA IUSER/777/
      DATA IDATE/920101/
      DATA ITIME/000000/
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZLOC('VERTEX_RCP',LRCP)
        IF (LRCP .EQ. 0) THEN
          CALL INRCP('VERTEX_RCP',ERR)
          IF (ERR .NE. 0) CALL ERRMSG('VERTEX_RCP not defined',
     &      'VXY_BEAM1','assign logical and try again!','F')
        ENDIF
        CALL EZPICK('VERTEX_RCP')
        CALL EZGET('BEAM_POS',BEAM_POS,IER)
        ERR = ABS(IER)
        CALL EZGET('BEAM_ERR',BEAM_ERR,IER)
        ERR = ERR + ABS(IER)
        CALL EZGET('BEAM_SLOP',BEAM_SLOP,IER)
        ERR = ERR + ABS(IER)

        IF (IQ(LHEAD + 1) .GT. 1000) THEN
          CALL EZRSET
          GO TO 3
        ENDIF

        CALL EZGET('BMSL_ERR',BMSL_ERR,IER)
        ERR = ERR + ABS(IER)
        CALL EZGET('IP_SIGMA',IP_SIGMA,IER)
        ERR = ERR + (IER)
        CALL EZGETS('XYBEAM_FILE',1,FILENAME,LEN,IER)
        ERR = ERR + ABS(IER)
        IF (ERR .NE. 0) CALL ERRMSG('BAD RCP','VXY_BEAM1',
     &    'VERTEX_RCP has missing or illegal DATA params','F')
        CALL EZRSET
        ZVTX_AVE = BEAM_POS(3)
        IP_SIGMA(1) = IP_SIGMA(1)**2
        IP_SIGMA(2) = IP_SIGMA(2)**2
        LAST = -1
      ENDIF
C
C ****  NORMAL ENTRY
C

3     IF (IQ(LHEAD + 1) .GT. 1000) THEN
        LVERT = GZVERT(1)
        IF (LVERT.GT.0) THEN
          XB = Q(LVERT+3)
          DXB= Q(LVERT+6)
          YB = Q(LVERT+4)
          DYB= Q(LVERT+7)
          IF (DXB.EQ.0.) DXB= BEAM_ERR(1) ! protect the global fitter 
          IF (DYB.EQ.0.) DYB= BEAM_ERR(2)
        ELSE  
          XB = BEAM_POS(1) + ZVTX*BEAM_SLOP(1)
          DXB= BEAM_ERR(1)
          YB = BEAM_POS(2) + ZVTX*BEAM_SLOP(2)
          DYB= BEAM_ERR(2)
        ENDIF 
        STATUS = 0
        GO TO 999
      ELSE
C
C ****  Check to see if there is already run dependant beam position..
C
        VERS = 0
        LVERH = GZVERH()
        IF (LVERH .GT. 0) VERS = IQ(LVERH+1)
        IF (VERS .GE. 2) THEN
          XB = Q(LVERH+4) + ZVTX*Q(LVERH+7)
          YB = Q(LVERH+5) + ZVTX*Q(LVERH+8)
          DX = SQRT( Q(LVERH+9)**2
     &             + ((ZVTX-Q(LVERH+6))*BMSL_ERR(1))**2 
     &             + IP_SIGMA(1))
          DY = SQRT( Q(LVERH+10)**2
     &             + ((ZVTX-Q(LVERH+6))*BMSL_ERR(2))**2 
     &             + IP_SIGMA(2))
          STATUS = -1
        ELSE
          IF (READ_ASCII) THEN 
            READ_ASCII=.FALSE.
            CALL DBPKTS(IDATE,ITIME,TZERO) ! DBL3 TIME FOR 1-JAN-1992 00:00
            CALL GTUNIT(IUSER,LUN,ERR)
            CALL D0OPEN(LUN,FILENAME(1:LEN),'IF',OK)
            IF (.NOT. OK) CALL ERRMSG('No beam position file found',
     &      'VXY_BEAM1','Cannot proceed...','F')
            TOTAL = 0
    1       READ(LUN,99,END=2) RUN,X,DX,Y,DY,SX,DSX,SY,DSY,N,DAY
   99       FORMAT(1X,I6,1X,2(F6.4,1X,F5.4,3X),
     &                2(F6.2,1X,F5.2,3X),I4,1X,F7.2)
            TOTAL = TOTAL + 1
            RUNS(TOTAL) = RUN
            XBEAM(TOTAL) = X
            YBEAM(TOTAL) = Y
            DXBEAM(TOTAL) = DX
            DYBEAM(TOTAL) = DY
            SXBEAM(TOTAL) = SX/10000.
            SYBEAM(TOTAL) = SY/10000.
            HOURS(TOTAL) = DAY*24.
            NTracks(TOTAL) = N 
            IF (TOTAL .EQ. MAX_RUNS) THEN
              WRITE(TXT,'(A,I6)') ' Last run read is ',RUN
              CALL ERRMSG('Too many runs',
     &          'VXY_BEAM1',TXT,'W')
              GO TO 2
            ENDIF
            GO TO 1
    2       CLOSE(LUN)
            CALL RLUNIT(IUSER,LUN,ERR)
          ENDIF
          RUN = RUNNO()
          IF (RUN .NE. LAST) THEN
            LAST = RUN
            IF (RUN .LT. RUNS(1)) THEN
              IR = 1
              CALL ERRMSG('Requested run is EARLY','VXY_BEAM1',
     &                    'Using first run in file','W')
            ELSEIF (RUN .GT. RUNS(TOTAL)) THEN
              IR = TOTAL
              CALL ERRMSG('Requested run is LATE','VXY_BEAM1',
     &                    'Using last run in file','W')
            ELSE
              DO IR = 1,TOTAL-1
                IF (RUN .GE.RUNS(IR) .AND. RUN .LT. RUNS(IR+1)) GO TO 20
              ENDDO
            ENDIF
   20       XB_LAST  = XBEAM(IR)
            YB_LAST  = YBEAM(IR)
            DX_LAST  = DXBEAM(IR)
            DY_LAST  = DYBEAM(IR)
            SX_LAST  = SXBEAM(IR)
            SY_LAST  = SYBEAM(IR)
            IF (RUN .EQ. RUNS(IR)) THEN
              STATUS_LAST = 0
            ELSE
              CALL D3UPT(IQ(LHEAD+4),TIME)      ! DBL3 TIME FOR FIRST EVENT
              HOUR = D3UXH(TIME,TZERO)          ! HOURS SINCE 1-JAN-1992 00:00
              STATUS_LAST = NINT(HOUR - HOURS(IR))
            ENDIF
          ENDIF
          XB = XB_LAST + ZVTX*SX_LAST
          DXB= SQRT(DX_LAST**2 + ((ZVTX-ZVTX_AVE)*BMSL_ERR(1))**2
     &              + IP_SIGMA(1))
          YB = YB_LAST + ZVTX*SY_LAST
          DYB= SQRT(DY_LAST**2 + ((ZVTX-ZVTX_AVE)*BMSL_ERR(2))**2
     &              + IP_SIGMA(2))
          STATUS = STATUS_LAST
        ENDIF
      ENDIF
  999 RETURN
      ENTRY VXY_AUX(ARUN, ANT, AERRX, AERRY, ASLOPX, ASLOPY)
C--
C Return run actually used and the number of tracks used therin
C--
      IF(IR .GT. 0) THEN
        ARUN = RUNS(IR)
        ANT = NTracks(IR)
        AERRX = DXBEAM(IR)
        AERRY = DYBEAM(IR)
        ASLOPX= SXBEAM(IR)
        ASLOPY= SYBEAM(IR)
      ELSE
        ARUN = -1
        ANT = -1
      ENDIF
      END
