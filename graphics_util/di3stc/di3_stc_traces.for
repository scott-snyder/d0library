      SUBROUTINE STC_TRACES(IDD,SCROLL)
C----------------------------------------------------------------------
C-   DI3000 VERSION
C-   Purpose and Methods :  Draw the strip chart traces from data stored
C-             in the CHDATA and TIMS arrays. 
C-   Inputs  : IDD      Chart identifier (If negative, force buff beginning)
C-             SCROLL   0 - Draw to current time point
C-                      1 - Scroll forward one time point (previously
C-                                  scrolled backward
C-                     -1 - Scroll backward one time point.
C-                      2 - After first plot has reached end,
C-                          compress to begin at first time point.
C-
C-   Created  8-NOV-1990   Michael A. Shupe
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:DI3STC.INC'
      INTEGER SCROLL
      REAL AMXMN(2)
      CHARACTER*40 GTITLE
      CHARACTER*20 LSTRNG
C----------------------------------------------------------------------
      ID=IABS(IDD)
      NID=INDEXC(ID)                     ! CHART NUMBER FOR THIS ID
      IBASE=ICNDX(NID)                   ! BEGINNING OF STORAGE FOR CHART
      NTRA=NTRAS(NID)                    ! NUMBER OF TRACES
      ITFIL=ITFILL(NID)                  ! POSITION OF FILL POINTER
      ITTRC=ITTRAC(NID)                  ! POSITION OF TRACE POINTER
      DT=TDELTA(NID)                     ! TMAX - TMIN FOR DISPLAY
      T0=TZERO(NID)                      ! CURRENT TIME ORIGIN (NOT RASP)
      R=RASP(NID)
      DY=1./FLOAT(NTRA)
C      TYPE 777,IDD,NID,IBASE,NTRA,ITFIL,ITTRC,DT,T0
C  777 FORMAT(' IDD,NID,IBASE,NTRA,ITFIL,ITTRC,DT,T0:',/,6I5,2F)
C
C----------PLOT RECENT LINE SEGMENT(S) OF EACH TRACE DO SCALES
   10 ITTRC=ITTRC-NREVS
      IF(ITTRC.EQ.0) GO TO 45
      IF(ITTRC.GE.ITFIL) GO TO 45
      IF(ITTRC.EQ.1) THEN
        T0=TIMS(1,NID)
        TZERO(NID)=T0
      ENDIF
      IF(IDD.LT.0) ITTRC=1                 ! STC_PLOT, FORCE BIN 1
      IF(NEWFRM(NID)) THEN                 ! FRAME ACTION CLEARED SCREEN
        NEWFRM(NID)=.FALSE.
        ITTRC=1                            ! FORCE BIN 1
      ENDIF
      IF(IDEV.EQ.1)
     &     CALL JVPORT(VPXLO(NID),VPXHI(NID),VPYLO(NID),VPYHI(NID))
      CALL JOPEN
      DO 20 J=ITTRC,ITFIL-1                ! LOOP OVER TIME BINS
        T1=(TIMS(J,NID)-T0)/DT             ! RETRIEVE THE TIME, SCALE
        IF(T1.LT.0.) GO TO 20
        T2=(TIMS(J+1,NID)-T0)/DT
C        TYPE 888,ID,J,T0,DT,T1,T2
C  888   FORMAT(' ID,J,T0,DT,T1,T2:',2I5,4F)
        ITBAR=JTBAR(J+1,NID)
        DO 30 I=1,NTRA                     ! LOOP OVER TRACES
          INDX=IBASE+I-1                   ! POINTER FOR THIS TRACE
          YBASE=DY*(FLOAT(I)-1.)           ! Y BASELINE FOR THIS TRACE
          AMXMN(2)=TRSCAL(2,INDX)          ! SCALE FACTORS IN Y
          AMXMN(1)=TRSCAL(1,INDX)
C  DRAW THE DATA
          CALL JCOLOR(7)                   ! WHITE
          DVAL=AMXMN(2)-AMXMN(1)
          Y1=CHDATA(J,INDX)                ! RETRIEVE THE DATA
          Y2=CHDATA(J+1,INDX)              ! NEW Y VALUE
          IF(.NOT.OPTNS(2,NID)) THEN
            Y1=(Y1-AMXMN(1))*DY/DVAL+YBASE ! SCALE IT TO DISPLAY
            Y2=(Y2-AMXMN(1))*DY/DVAL+YBASE   
          ELSE
            AMN=TRSCAL(1,INDX)
            Y1=(Y1-AMN)/DVAL
            Y2=(Y2-AMN)/DVAL
            IF(OPTNS(3,NID)) THEN          ! COLOR CODED
              IC=MOD(I,6)+1
              CALL JCOLOR(IC)
            ELSE                           ! SET LINE STYLE INSTEAD
              IC=MOD(I,6)
              CALL JLSTYL(IC)
            ENDIF
          ENDIF
          CALL JMOVE(T1,Y1)                ! DRAW THE DATA
          CALL JDRAW(T2,Y2)
          CALL JLSTYL(0)
          IF(OPTNS(1,NID)) THEN            ! DRAW THE TIC MARK IF REQ.
            CALL JMOVE(T2,Y2+.005)
            CALL JDRAW(T2,Y2-.005)
          ENDIF
          IF(ITBAR.GT.0.AND.I.EQ.1) THEN   ! DRAW TIME BAR IF REQUESTED
            CALL JCOLOR(4)                 ! BLUE
            CALL JMOVE(T2,0.)
            CALL JDRAW(T2,1.)
          ENDIF
   30   CONTINUE
   20 CONTINUE
      CALL JCLOSE
C
   45 ITTRAC(NID)=ITFIL                    ! POINTERS NOW IN SYNC
C
  999 RETURN
      END
