      SUBROUTINE STC_NEWFRAME(IDONLY)
C----------------------------------------------------------------------
C-   DI3000 VERSION
C-   Purpose and Methods :  Get fresh displays of all current plots.
C-      STC_PLOT will have provided the retained segments for frames
C-      and labels.  This routine provides scale factors and time
C-      labels (since they may be changing), and calls STC_TRACES
C-      to draw the traces
C-
C-   Created  8-NOV-1990   Michael A. Shupe
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:DI3STC.INC'
      INTEGER SCROLL
      REAL AMXMN(2)
      CHARACTER*40 GTITLE
      CHARACTER*20 LSTRNG
      INTEGER IHJ(4),IVJ(4)
      DATA IHJ,IVJ/2,2,1,3,2,1,1,1/
      DATA ASIZS/.015/
      DATA ASIZ,GSIZ,TSIZ/.02,.02,.02/
C----------------------------------------------------------------------
C
C  FRAME ACTION TO BRING UP ALL RETAINED SEGMENTS UNLESS NOT WANTED
C  (IN CALLS FROM STC_PLOT)
      IF(IDONLY.EQ.0) CALL JFRAME
C
C  LOOP OVER ALL CURRENTLY PLOTTED CHARTS
      DO 10 ID=1,IDMAX
        IF((IDONLY.GT.0).AND.(ID.NE.IDONLY)) GO TO 10
        NID=INDEXC(ID)                     ! CHART NUMBER FOR THIS ID
        IF(.NOT.PLOTTD(NID)) GO TO 10      ! NOT BEING PLOTTED AT PRESENT
        NEWFRM(NID)=.TRUE.                 ! TELL STC_TRACES OF FRAME ACT.
        IBASE=ICNDX(NID)                   ! BEGINNING OF STORAGE FOR CHART
        NTRA=NTRAS(NID)                    ! NUMBER OF TRACES
        DT=TDELTA(NID)                     ! TMAX - TMIN FOR DISPLAY
        T0=TZERO(NID)                      ! CURRENT TIME ORIGIN
        R=RASP(NID)                        ! ASPECT RATIO
        DY=1./FLOAT(NTRA)
        ITPTR=ITFILL(NID)
C      TYPE 777,IDD,NID,IBASE,NTRA,ITFIL,ITTRC,DT,T0
C  777 FORMAT(' IDD,NID,IBASE,NTRA,ITFIL,ITTRC,DT,T0:',/,6I5,2F)
C
C  CHECK FOR COMPRESSED MODE
        IF(OPTNS(4,NID).AND.(ITPTR.GT.1)) THEN
          T0=TIMS(1,NID)
          TZERO(NID)=T0
          DT=2.*(TIMS(ITPTR,NID)-T0)
          TDELTA(NID)=DT
        ENDIF
C
C  SET VIEWPORT
        IF(IDEV.EQ.1) 
     &      CALL JVPORT(VPXLO(NID),VPXHI(NID),VPYLO(NID),VPYHI(NID))
C
C  WRITE SCALE FACTORS
        CALL JOPEN
        X=-.03                             ! UPDATE THE SCALE FACTORS
        XSZ=ASIZS
        YSZ=ASIZS/R
        CALL JSIZE(XSZ,YSZ)
        CALL JJUST(3,1)                    ! JUSTIFY
        CALL JCOLOR(3)                     ! YELLOW
        DO 1 I=1,NTRA                      ! LOOP OVER TRACES
          INDX=IBASE+I-1                   ! POINTER FOR THIS TRACE
          YBASE=DY*(FLOAT(I)-1.)           ! Y BASELINE FOR THIS TRACE
          AMXMN(2)=TRSCAL(2,INDX)          ! SCALE FACTORS IN Y
          AMXMN(1)=TRSCAL(1,INDX)
          IF(.NOT.OPTNS(2,NID))THEN
            DO 2 JJ=1,2
              WRITE(LSTRNG,*)AMXMN(JJ)
              Y=(DY-2.0*YSZ)*FLOAT(JJ-1)+YBASE+.5*YSZ
              CALL JMOVE(X,Y)
              LENG=MYL(LSTRNG)
              CALL J3STRG(LSTRNG(1:LENG))  ! WRITE THE SCALE FACTORS
    2       CONTINUE
          ELSE                             ! ALL TRACES - ONE AXIS
            Y=YSZ
            CALL JMOVE(X,Y)
            CALL JJUST(3,3)
            DO 3 JJ=1,2
              WRITE(LSTRNG,*)AMXMN(JJ)     ! FIRST SET OF SCALES USED
              LENG=MYL(LSTRNG)
              CALL J3STRG(LSTRNG(1:LENG))
              Y=1.
              CALL JMOVE(X,Y)
    3       CONTINUE
            GO TO 4
          ENDIF
    1   CONTINUE
C  TIME LABELS
    4   TEND=DT+T0
        IF(OPTNS(8,NID)) GO TO 16           ! SUPPRESS BOTTOM LABELLING
        DO 15 NTITL=2,4
          IF(NTITL.EQ.3) THEN
            WRITE(GTITLE,*) T0
          ELSEIF(NTITL.EQ.4) THEN
            WRITE(GTITLE,*) TEND
          ELSE
            GTITLE='***TSTART  TSTOP***                     '
          ENDIF
          X=.5
          Y=1.05
          IF(NTITL.GT.1) Y=-2.*GSIZ/R
          IF(NTITL.EQ.3) X=0.
          IF(NTITL.EQ.4) X=1.
          XSZ=GSIZ
          XSV=XSZ
          YSZ=GSIZ/R
          YMARGD=1.5*YSZ
          IF(NTITL.EQ.1) THEN
            XSZ=GSIZ*2.
            YSZ=XSZ/R
            YMARGU=1.5*YSZ
          ENDIF
          CALL JJUST(IHJ(NTITL),IVJ(NTITL))       ! JUSTIFICATION
          CALL JSIZE(XSZ,YSZ)                     ! CHARACTER SIZE
          IF(NTITL.EQ.2) THEN
            CALL JCOLOR(2)                        ! GREEN
          ELSE
            CALL JCOLOR(3)                        ! YELLOW
          ENDIF
          CALL JMOVE(X,Y)
          LENG=MYL(GTITLE)
          CALL J3STRG(GTITLE(1:LENG))             ! DO TITLE STRING
   15   CONTINUE
   16   CALL JCLOSE
C
C  PLOT LINE SEGMENTS OF EACH TRACE (NEG PUTS POINTER TO START OF BUFFER)
        CALL STC_TRACES(-ID,SCROL(ID))
   10 CONTINUE
C
  999 RETURN
      END
