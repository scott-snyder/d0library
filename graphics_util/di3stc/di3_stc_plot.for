      SUBROUTINE STC_PLOT(ID,TSTART)
C----------------------------------------------------------------------
C-   DI3000 VERSION
C-   Purpose and Methods : Display a strip chart which has been
C-              previously booked by a call to STC_BOOK.
C-
C-   Inputs  : ID       Chart identifier used in booking call.
C-             TSTART   0 - Position chart at current time pointer.
C-                      M - Position chart at time bin M.
C-
C-   Created  9-NOV-1990   Michael A. Shupe
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:DI3STC.INC'
      INTEGER TSTART
      CHARACTER*40 GTITLE
      CHARACTER*12 GTRTTL
C
      CHARACTER*4 BLNK
      DATA BLNK/'    '/
      DATA EPS/0.02/
C
      DATA ASIZ,GSIZ,TSIZ/.02,.02,.02/
      INTEGER IHJ(4),IVJ(4)
      CHARACTER*40 BANTTL
      CHARACTER PAD*80
      DATA ASPECT,XLO,XHI,YLO,YHI/.8,-.3,1.1,-.1,1.1/
      DATA IHJ,IVJ/2,1,1,3,2,1,1,1/
      DATA IDEV/1/
C----------------------------------------------------------------------
      IF(IFIRST.EQ.1) GO TO 4000
C----------ONE-TIME SETUPS----------
      CALL JBEGIN
      CALL JVSPAC(-1.,1.,-ASPECT,ASPECT)
      IFIRST=1
C
C----------FIND THE CHART----------
 4000 IF(NCHARTS.EQ.0) THEN                ! NO CHARTS BOOKED YET
        CALL OUTMSG(' STC_PLOT--NO CHARTS HAVE YET BEEN BOOKED.')
        RETURN
      ENDIF
      IF(ID.LT.0.OR.ID.GT.10) THEN
        CALL OUTMSG(' STC_PLOT--ID OUT OF RANGE')
        RETURN
      ELSE
        NID=INDEXC(ID)
        PLOTTD(NID)=.TRUE.
        IF(NID.EQ.0) THEN
          CALL OUTMSG(' STC_PLOT--CHART HAS NOT BEEN BOOKED')
          RETURN
        ELSE
          IDNOW=ID
        ENDIF
        HEDR(NID)=HEADR                 ! SAVE HEADER FOR HARDCOPY
      ENDIF
C
C----------MAKE THE CHART WINDOW----------
C  ID OK. CONVERT FROM SCREEN FRACTIONS TO SCREEN COORDINATES
      IF(INPROG.EQ.0) THEN
        INPROG=1
        CALL JDINIT(IDEV)
        CALL JDEVON(IDEV)
        CALL JWINDO(XLO,XHI,YLO,YHI)
        IF(HEADR.NE.'    ') THEN
          CALL JVPORT(-1.,1.,-ASPECT,ASPECT)
          IF(IDEV.EQ.1) THEN
            CALL JROPEN(200)
          ELSE
            CALL JOPEN
          ENDIF
          CALL JSIZE(.02,.02)
          LENG=MYL(HEADR)
          CALL JJUST(1,3)
          CALL JMOVE(-.28,1.1)
          CALL J3STRG(HEADR(1:LENG))
          IF(IDEV.EQ.1) THEN
            CALL JRCLOS
          ELSE
            CALL JCLOSE
          ENDIF
        ENDIF
      ENDIF
      IF(IDEV.EQ.1) THEN
        VXLO=2.*WNDO(1,NID)-1.
        VYLO=2.*ASPECT*WNDO(2,NID)-ASPECT
        VXHI=2.*WNDO(3,NID)-1.
        VYHI=2.*ASPECT*WNDO(4,NID)-ASPECT
      ELSE                                     ! HARDCOPY WINDOW
        VXLO=-1.
        VXHI=1.
        VYLO=-ASPECT
        VYHI=ASPECT
      ENDIF
      CALL JVPORT(VXLO,VXHI,VYLO,VYHI)
      VPXLO(NID)=VXLO
      VPXHI(NID)=VXHI
      VPYLO(NID)=VYLO
      VPYHI(NID)=VYHI
      IF(IDEV.EQ.1) THEN
        CALL JROPEN(NID)                  ! SEGMENT NUMBER FOR THIS CHART
      ELSE
        CALL JOPEN
      ENDIF
      R=(VYHI-VYLO)/(VXHI-VXLO)           ! TRUE ASPECT RATIO
      RASP(NID)=R
C
C  FRAMES
      D=.003
C      CALL JRECT(XLO+D,YLO+D,XHI-D,YHI-D)
      CALL JRECT(-D,-D,1.+D,1.+D)
C
C  TITLES
      ITLAST=1
      IF(ITUSER.EQ.1) ITLAST=4
      DO 15 NTITL=1,ITLAST
        GTITLE=TITLS(NTITL,NID)
        IF(GTITLE.EQ.BLNK) GO TO 15
        IF((NTITL.EQ.1).AND.OPTNS(7,NID)) GO TO 15      ! SUPP TOP TITL
        IF((NTITL.EQ.2).AND.OPTNS(8,NID)) GO TO 15      ! SUPP BOT TITL
        X=.5
        Y=1.03
        IF(NTITL.GT.1) Y=-2.*GSIZ/R
        IF(NTITL.EQ.3) X=0.
        IF(NTITL.EQ.4) X=1.
        XSZ=GSIZ
        XSV=XSZ
        YSZ=GSIZ/R
        YMARGD=1.5*YSZ
        IF(NTITL.EQ.1) THEN
          XSZ=GSIZ*1.2
          YSZ=XSZ/R
          YMARGU=1.5*YSZ
        ENDIF
        CALL JJUST(IHJ(NTITL),IVJ(NTITL))       ! JUSTIFICATION
        CALL JSIZE(XSZ,YSZ)                     ! CHARACTER SIZE
        CALL JCOLOR(2)                          ! GREEN
        CALL JMOVE(X,Y)
        LENG=MYL(GTITLE)
        CALL J3STRG(GTITLE(1:LENG))             ! DO TITLE STRING
   15 CONTINUE
C  TRACE TITLES
   20 CONTINUE
      IBASE=ICNDX(NID)
      NTRA=NTRAS(NID)
      X=-.03
      YOFFS=0.
      DY=1./FLOAT(NTRA)
      IF(OPTNS(2,NID)) THEN
        DY=DY/2.
        YOFFS=.2
      ENDIF
      CALL JSIZE(TSIZ,TSIZ/R)
      CALL JCOLOR(2)
      DO 25 NTITL=1,NTRA                        ! LOOP OVER TRACES
        GTRTTL=TRTITL(IBASE+NTITL-1)
        Y=DY*(FLOAT(NTITL)-.5)+YOFFS
        CALL JJUST(3,2)                         ! JUSTIFY RIGHT,CENT.
        ILSTY=0
        IF(OPTNS(2,NID)) THEN                    ! ALL TRACES ONE AXIS
          IF(OPTNS(3,NID)) THEN                  ! COLOR CODED
            IC=MOD(NTITL,6)+1
            CALL JCOLOR(IC)
          ELSE                                  ! SET LINE STYLE INSTEAD
            IC=MOD(NTITL,6)
            ILSTY=1
          ENDIF
        ENDIF
        CALL JMOVE(X,Y)
        LENG=MYL(GTRTTL)
        CALL J3STRG(GTRTTL(1:LENG))             ! TRACE TITLES
        IF(ILSTY.EQ.1) THEN                     ! SAMPLE LINE BELOW TITLE
          ILSTY=0
          CALL JLSTYL(IC)
          YY=Y-DY/4.
          CALL JMOVE(X,YY)
          CALL JMOVE(-.1,YY)
          CALL JLSTYL(0)
        ENDIF
        IF(NTITL.EQ.1) GO TO 25
        YBASE=Y-DY*.5
        IF(OPTNS(2,NID)) GO TO 25               ! ONE AXIS, NO DIVIDER BAR
        CALL JMOVE(0.,YBASE)
        CALL JDRAW(1.,YBASE)                    ! HORIZ DIVIDER BAR
   25 CONTINUE
C
      IF(IDEV.EQ.1) THEN
        CALL JRCLOS
      ELSE
        CALL JCLOSE
      ENDIF
C
C----------POSITION THE CHART AND PLOT IT
      CALL STC_NEWFRAME(IDNOW)        ! NONZERO ID SUPPRESSES JFRAME
C
  999 RETURN
      END


