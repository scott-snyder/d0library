      PROGRAM STCDEMO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Example to exercise and demonstrate the
C-        capabilities of the STC strip chart package.
C-
C-   Created  28-MAY-1990   Michael A. Shupe
C-   Updated  24-MAR-2004   sss - compile with g77.
C-
C----------------------------------------------------------------------
C  TITLE FOR TOP (1).  IF TITLES 2-4 ARE BLANK, TIME LABELLING IS
C  PROVIDED.  FOR EACH LABEL NOT BLANK, THE TIME LABELLING IS REPLACED
C  BY THIS LABEL.
      CHARACTER*40 GRANDTITLES(4)
      DATA GRANDTITLES/'THIS IS THE MAIN TITLE                  ',
     &                 'LABEL X AXIS                            ',
     &                 'LOWER LEFT                              ',
     &                 'LOWER RIGHT                             '/
C  TRACE LABELS (USUALLY WILL BE DIFFERENT FOR EACH PLOT BOOKED.)
      CHARACTER*12 TRACETITLES(10)
      DATA TRACETITLES/'HEWEY       ',
     &                 'DEWEY       ',
     &                 'LEWEY       ',
     &                 'FLOPSIE     ',
     &                 'MOPSIE      ',
     &                 'COTTENTAIL  ',
     &                 'EEYORE      ',
     &                 'YORICK      ',
     &                 'DULCINEA    ',
     &                 'CLYTEMNESTRA'/
C  OPTIONS ARRAY.  NO OPTIONS INITIALLY
      LOGICAL OPTIONS(10)
C  FULL SCREEN WINDOW
      REAL WINDOW(4)
      DATA WINDOW /0.,0.,1.,1./
C  QUAD WINDOWS
      REAL WINDOQ(4,4)
      DATA WINDOQ /0.,.5,.5,1.,    .5,.5,1.,1.,
     &             0.,0.,.5,.5,    .5,.0,1.,.5/
C  STACKED WINDOWS
      REAL WINDST(4,4)
      DATA WINDST /0.,.06,1.,.275,    0.,.276,1.,.50,
     &             0.,.501,1.,.735,   0.,.736,1.,.95/
C  ODD SIZES
      REAL WINDOO(4,2)
      DATA WINDOO /.1,.1,.6,.6,    .2,.7,.9,.9/
C  CHART WIDTH IN TIME UNITS
      DATA DELTATIME/100./
C  MINIMUM AND MAXIMUM SCALE VALUES FOR EACH TRACE
      REAL TRACESCALES(2,10)
      DATA TRACESCALES/0.,10.,-1.2,1.2,-1.2,1.2,-1.2,1.2,-1.2,1.2,
     &  -1.2,1.2,-1.2,1.2,-1.2,1.2,-1.2,1.2,-1.2,1.2/
C  DATA ARRAY
      REAL YDATA(10)
C  MISCELLANEOUS LOCAL STUFF
      INTEGER NT(4),JJ(4),NPTS(10),ITEM(4)
      DATA NT/3,2,3,2/
      DATA JJ/0,3,5,8/
      DATA NPTS/3*135,7*250/
C----------------------------------------------------------------------
      write (*,555)
  555 FORMAT(' TRACES FULL SPEED (0) OR SLOW MODE (1)?:',$)
C&IF LINUX
C&      read (*,*) islow
C&ELSE
      ACCEPT 556,ISLOW
  556 FORMAT(I)
C&ENDIF
C
C  BOOK THE CHARTS--UP TO TEN ALLOWED, WITH ID'S 1 THROUGH 10 IN ANY ORDER.
      NTRACES=10
      CALL STC_BOOK(8,GRANDTITLES,WINDOW,10,TRACETITLES,
     &    TRACESCALES,DELTATIME,OPTIONS)
      CALL STC_BOOK(10,GRANDTITLES,WINDOW,10,TRACETITLES,
     &    TRACESCALES,DELTATIME,OPTIONS)
      CALL STC_BOOK(9,GRANDTITLES,WINDOW,6,TRACETITLES(2),
     &    TRACESCALES(1,2),DELTATIME,OPTIONS)
      DO 5 I=1,4
        WRITE(GRANDTITLES(1),15) I
   15   FORMAT('PLOT ',I1,'                                  ')
        KK=I+JJ(I)
        CALL STC_BOOK(I+1,GRANDTITLES,WINDOQ(1,I),NT(I),
     &  TRACETITLES(JJ(I)+1),TRACESCALES(1,JJ(I)+1),DELTATIME,OPTIONS)
    5 CONTINUE
C
C  DO THE CHARTS IN VARIOUS PASSES
C  NOTE:  For convenience, the STC_OPTIONS and STC_REWINDOW routines
C         are used below to change plot formats and conditions.  All of
C         these options could have been specified at booking time
C         instead.
C  ALSO:  The fifth pass stacks four plots above one another, and
C         uses the title suppression options to use the title from
C         the top plot only and the time scales from the bottom plot
C         only.  This allows the flexibility to put single or multiple
C         traces along one axis with plots above each other as if
C         they were multiply traces of a single plot.  HOWEVER, this
C         works only if these plots all share the same time base!
C
      DO 8 IPASS=1,5
        IF(IPASS.EQ.1) THEN
          CALL STC_HEADER('ALL TRACES - ONE PLOT')
        ELSEIF(IPASS.EQ.2) THEN
          CALL STC_HEADER('FOUR PLOTS - SEPARATELY ADJUSTABLE WINDOWS')
        ELSEIF(IPASS.EQ.3) THEN
          CALL STC_HEADER('COMBINED TRACES - ONE PLOT')
        ELSEIF(IPASS.EQ.4) THEN
          CALL STC_HEADER('ALL TRACES - COMPRESS ON UPDATE, ADD TICS')
        ELSEIF(IPASS.EQ.5) THEN
          CALL STC_HEADER('FOUR PLOTS - COMBINED TRACES, TICS')
        ENDIF
        IF(IPASS.EQ.3) THEN
          CALL STC_OPTIONS(9,2,.TRUE.)          ! ALL TRACES ONE AXIS
          CALL STC_OPTIONS(9,3,.TRUE.)          ! COLORS FOR TRACES
        ENDIF
        IF(IPASS.EQ.4) THEN
          CALL STC_OPTIONS(10,1,.TRUE.)         ! TIC MARKS
          CALL STC_OPTIONS(10,4,.TRUE.)         ! ALL DATA FROM T0 ONWARD
        ENDIF
        IF(IPASS.EQ.5) THEN
          DO 11 I=1,4
            CALL STC_REWINDOW(I+1,WINDST(1,I))  ! REDO WINDOWS
            CALL STC_OPTIONS(I+1,1,.TRUE.)      ! ADD TIC MARKS
            CALL STC_OPTIONS(I+1,2,.TRUE.)      ! ALL TRACES ONE AXIS
            CALL STC_OPTIONS(I+1,3,.TRUE.)      ! COLORS FOR TRACES
            IF(I.LT.4) CALL STC_OPTIONS(I+1,7,.TRUE.) ! SUPP TOP TITLE
            IF(I.GT.1) CALL STC_OPTIONS(I+1,8,.TRUE.) ! SUPP BOT TITLE
   11     CONTINUE
        ENDIF
        IFIRST=0
C  FILL THE CHARTS
        DO 9 I=1,4
          ITEM(I)=0
    9   CONTINUE
        DO 10 II=1,NPTS(IPASS)
          IF(ISLOW.EQ.1) THEN
            DO 558 KLM=1,10000
              V=COS(RAN(III))
  558       CONTINUE
          ENDIF
          DO 20 J=1,10
            YDATA(J)=1.-2.*RAN(III)
   20     CONTINUE
          LL=II
C          IF(IPASS.NE.2) THEN
C            ITM=ITM+1
C            LL=ITM
C          ENDIF
          TNOW=FLOAT(LL)
          JJJ=MOD(LL,10)
          YDATA(1)=JJJ
          YDATA(3)=SIN(FLOAT(LL)/30.)
          YDATA(5)=0
          IF(JJJ.GT.3) YDATA(5)=1.
          IF(JJJ.GT.7) YDATA(5)=-1.
          IF(IPASS.EQ.1) THEN
            IF(JJJ.EQ.0) ITBAR=1
            CALL STC_FILL(8,YDATA,TNOW,ITBAR)
          ELSEIF(IPASS.EQ.3) THEN
            YDATA(2)=.5*YDATA(3)+.05*YDATA(2)
            YDATA(5)=.5*YDATA(5)
            YDATA(6)=.5*SQRT(1-YDATA(3)**2)+.1*YDATA(5)
            YDATA(7)=.1*YDATA(7)
            CALL STC_FILL(9,YDATA(2),TNOW,ITBAR)
          ELSEIF(IPASS.EQ.4) THEN
            IF(JJJ.EQ.0) ITBAR=1
            CALL STC_FILL(10,YDATA,TNOW,ITBAR)
          ELSEIF(IPASS.EQ.2.OR.IPASS.EQ.5) THEN
            DO 30 K=1,4
C  MAKE CHARTS RUN AT DIFFERENT RATES
              IF(RAN(III).GT.(SQRT(FLOAT(K-1)/4.))) THEN
                ITEM(K)=ITEM(K)+1
                TTEM=FLOAT(ITEM(K))
                JJJ=MOD(ITEM(K),10)
                IF(JJJ.EQ.0) ITBAR=1
                CALL STC_FILL(K+1,YDATA(JJ(K)+1),TTEM,ITBAR)
              ENDIF
   30       CONTINUE
          ENDIF
C  PLOT THE CHARTS (COULD HAVE DONE THIS BEFORE FILLING IF DESIRED.)
          IF(IFIRST.EQ.0) THEN
            IF(IPASS.EQ.1) THEN
              CALL STC_PLOT(8,0)
            ELSEIF(IPASS.EQ.3) THEN
              CALL STC_PLOT(9,0)
            ELSEIF(IPASS.EQ.4) THEN
              CALL STC_PLOT(10,0)
            ELSEIF(IPASS.EQ.2.OR.IPASS.EQ.5) THEN
              DO 35 K=1,4
                CALL STC_PLOT(K+1,0)
   35         CONTINUE
            ENDIF
            IFIRST=1
          ENDIF
   10   CONTINUE
C  DELETE PLOTS FROM SCREEN BETWEEN PASSES
        CALL STC_PLOT_END(0)                ! 0 = ALL PLOTS AT ONCE
    8 CONTINUE
C
      IF(NOTREADY.EQ.0) STOP
      write (*,777)
  777 FORMAT(' MAKING HARDCOPY OF 8,9,10')
      CALL STC_HARDCOPY(8)
      CALL STC_HARDCOPY(9)
      CALL STC_HARDCOPY(10)
      END
