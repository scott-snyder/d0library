      SUBROUTINE STC_BOOK(ID,TTLS,WNDW,NTRACES,TRTTLS,TRSCLS,DELT,OPTS)
C----------------------------------------------------------------------
C-   Purpose and Methods : Book a strip chart with multiple traces.
C-
C-   Inputs  :  
C-              ID              Identifier for this new strip chart.
C-		TTLS            Titles.  Top, Bottom, L.L., L.R.
C-              WNDW            Corners of window on screen.
C-                              (X1,Y1,X2,Y2 in screen fractions 0. to 1.) 
C-              NTRACES         Number of traces in this chart.
C-              TRTTLS          Array of titles (one per trace).
C-              TRSCLS          Min and Max scale values for each trace.
C-                                If zero, then autoscale
C-              DELT            Chart width in same units as TIM.
C-              OPTS            Options array. (Logical)
C-			   	  1 - Put tick mark on each data point.
C-				  2 - Put all NTRACES on one axis with
C-			              differing linestyles or colors.
C-			              Scales taken from TRSCLS(2,1).)
C-				  3 - Use colors for 2, else linestyles.
C-			          4 - Display all data from beginning
C-				  5 - Use automatic scale factors
C-                                6 - Use real time clock for time base
C-                                7 - Suppress top title
C-                                8 - Suppress bottom title
C-
C-   Outputs :  Strip chart graphic displays.
C-   Created  23-MAY-1990   Michael Shupe, Harrison Prosper
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:DI3STC.INC'
      REAL WNDW(4),TRSCLS(2,*)
      CHARACTER*40 TTLS(4)
      CHARACTER*12 TRTTLS(*)
      LOGICAL OPTS(10)
C----------------------------------------------------------------------
      IF(NCHARTS.EQ.10) THEN
        CALL OUTMSG(' STC_BOOK--TOO MANY CHARTS')
        RETURN
      ENDIF
      IF(ICNOW+NTRACES.GT.100) THEN
        CALL OUTMSG(' STC_BOOK--TOO MANY TRACES')
        RETURN
      ENDIF
C
C  STORE IT ALL
      NCHARTS=NCHARTS+1                 ! NUMBER OF CHARTS
      IF(ID.LT.1.OR.ID.GT.10) THEN
	CALL OUTMSG('  ID OUT OF RANGE 1-10')
	RETURN
      ELSE
        INDEXC(ID)=NCHARTS              ! CHART NUMBER FOR GIVEN ID
      ENDIF
      ICNDX(NCHARTS)=ICNOW+1            ! BASE ADDRESS OF CHART IN 
                                        !    TRACE STORAGE
      IDCHRT(NCHARTS)=ID                ! CHART ID FOR GIVEN CHART NUMBER
      NTRAS(NCHARTS)=NTRACES            ! NUMBER OF TRACES FOR THIS CHART
      TDELTA(NCHARTS)=DELT              ! TIME RANGE FOR THIS CHART
      DO 10 I=1,10                      
        OPTNS(I,NCHARTS)=OPTS(I)        ! CHARTING OPTIONS
   10 CONTINUE
      PLOTTD(NCHARTS)=.FALSE.           ! STC_PLOT HASN'T BEEN CALLED
      DO 20 I=1,4
        TITLS(I,NCHARTS)=TTLS(I)        ! TOP, BOTTOM, LEFT, RIGHT TITLES
        WNDO(I,NCHARTS)=WNDW(I)         ! X1,Y1,X2,Y2 WINDOW CORNERS (0.-1.)
   20 CONTINUE
      DO 30 I=1,NTRACES
        INDX=ICNOW+I
        TRTITL(INDX)=TRTTLS(I)          ! TITLE FOR EACH TRACE
        TRSCAL(1,INDX)=TRSCLS(1,I)      ! VALMIN FOR EACH TRACE
        TRSCAL(2,INDX)=TRSCLS(2,I)      ! VALMAX FOR EACH TRACE
        MAXVAL(INDX,NID)=-99999.        ! SET UP MIN MAX
        MINVAL(INDX,NID)=99999. 
        CUPDAT(INDX)=.FALSE.            
   30 CONTINUE
C
      ICNOW=ICNOW+NTRACES               ! READY FOR NEXT STC_BOOK CALL
C        
  999 RETURN
      END
