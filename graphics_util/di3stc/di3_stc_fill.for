      SUBROUTINE STC_FILL(ID,YDATA,TNOW,ITBAR)
C----------------------------------------------------------------------
C-   Purpose and Methods :  Fills strip data, appending YDATA
C-           for chart ID to the current list.  If chart is being
C-           plotted currently, this routine triggers the update.
C-
C-   Inputs  :  ID       Stripchart identifier
C-              YDATA    Values for the various traces
C-              TNOW     Time:  If TNOW is negative, the time since 
C-                         first STC_FILL will be calculated and
C-                         automatically provided for the time base.
C-              ITBAR    0 - No time bar
C-                       1 - Put a time bar on this point
C-                       2 - Put a time bar and label it
C-
C-   Created  24-MAY-1990   Michael Shupe
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:DI3STC.INC'
      REAL YDATA(*)
C----------------------------------------------------------------------
C  INITIALIZE TIMER IN CASE AUTO TIME MODE IS USED
      IF(IFIRST.EQ.0) THEN
        IFIRST=1
        START=SECNDS(0.0)
      ENDIF
C
      IF(NCHARTS.EQ.0) THEN                ! NO CHARTS BOOKED YET
        CALL OUTMSG(' STC_FILL--NO CHARTS HAVE YET BEEN BOOKED.')
        RETURN
      ENDIF
C
C  PICK UP LOCATION OF THIS CHART IN STORAGE
      IF(ID.LT.0.OR.ID.GT.10) THEN
        CALL OUTMSG(' STC_FILL--ID OUT OF RANGE')
        RETURN
      ELSE
        NID=INDEXC(ID)
        IF(NID.EQ.0) THEN
          CALL OUTMSG(' STC_FILL--CHART HAS NOT BEEN BOOKED')
          RETURN
        ELSE
          IDNOW=ID
        ENDIF
      ENDIF
C
C  ID OK, FILL THE CHART BUFFER
   20 IBASE=ICNDX(NID)                   ! BEGINNING OF STORAGE FOR CHART
      NTRA=NTRAS(NID)                    ! NUMBER OF TRACES
      ITPTR=ITFILL(NID)+1                ! POINTER FOR CURRENT TIME
      IF(TNOW.LT.0.) TNOW=SECNDS(START)  ! PROVIDE VAX TIME (AUTO)
      TIMS(ITPTR,NID)=TNOW               ! STORE THE TIME
      JTBAR(ITPTR,NID)=ITBAR             ! STORE THE TIME BAR REQUEST
      ITBAR=0
      DT=TDELTA(NID)
      TMAX=TZERO(NID)+DT                 ! MAX TIME ON CURRENT CHART
      IF(TNOW.GT.TMAX) THEN
        CUPDAT(NID)=.TRUE.               ! FLAG CHART TIME OVERFLOW
        IF(OPTNS(4,NID)) THEN
          TZERO(NID)=TIMS(1,NID)         ! COMPRESS MODE
          TDELTA(NID)=2.*(TNOW-TZERO(NID))
        ELSE
          TZERO(NID)=TZERO(NID)+.5*DT
        ENDIF
      ENDIF
C
      DO 30 I=1,NTRA                     ! LOOP OVER TRACES
        YY=YDATA(I)
        INDX=IBASE+I-1                   ! POINTER FOR THIS TRACE
        CHDATA(ITPTR,INDX)=YY            ! STORE THE DATA
        IF(.NOT.OPTNS(5,NID)) GO TO 30   ! AUTOMATIC SCALING?
        IF(YY.GT.MAXVAL(INDX,NID)) THEN
          MAXVAL(INDX,NID)=YY            ! RUNNING MAX AND MIN
          CUPDAT(NID)=.TRUE.
        ENDIF
        IF(YY.LT.MINVAL(INDX,NID)) THEN
          MINVAL(INDX,NID)=YY
          CUPDAT(NID)=.TRUE.
        ENDIF
   30 CONTINUE
      ITFILL(NID)=ITPTR                     ! UPDATE THE TIME POINTER
C
C  SEE IF CHART ATTENTION IS NEEDED
      IF(PLOTTD(NID)) THEN
        IF(CUPDAT(NID)) THEN
          CALL STC_NEWFRAME(0)   ! SOMETHING CHANGED, GET NEW DISPLAY
          CUPDAT(NID)=.FALSE.
        ELSE
          CALL STC_TRACES(ID,SCROL(IC))   ! CURRENT PLOT OK. NEW DATA.
        ENDIF
      ENDIF
      RETURN
C
  999 RETURN
      END
