      SUBROUTINE VFITSG(LAYER,SECTOR,LISTPT)
C--------------------------------------------------------------------
C
C  Fit and store a VTX track segment given by LISTPT.
C  (used in the 'road' method)
C
C  INPUT:  LAYER,SECTOR
C          LISTPT
C
C  Daria Zieminska Feb.,1988
C   28-SEP-1992  P.Grudberg,M.Pang  Allow for mirror hits
C   30-SEP-1992  P. Grudberg translate segments back to D0 frame
C   AUG-SEP-1992 Liang-Ping Chen Pass errors to LDVSEG, convert r-z fit to D0
C                     frame (instead of S-Z frame)
C-   Updated  26-OCT-1992   Peter M. Grudberg   Clean up hit tagging
C-   Updated  12-NOV-1992   Peter M. Grudberg  Fix phi bug 
C-   Updated  18-MAR-1993   Ed Oltman  Throw away hits to bring chi2/dof down
C-   Updated  23-APR-1993   Al Clark  Correct the calculation of SXY 
C-   Updated  16-JUL-1993   Liang-ping Chen use VERTXMC, VERTYMC for MC
C-   Updated   4-MAR-1994   liang-ping Chen  store relative pointers in PTHIT,
C-                              refresh LPOIN after CALL LDVSEG
C-   Updated   6-FEB-1995   Ed Oltman   Use vxy_Beam1
C-   Updated  16-FEB-1995   Liang-ping Chen  protect ZVTX=9999 with VXY_BEAM1
C--------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LAYER,SECTOR,NHIT,LRWIR(8),IHIT(8),NWIRES
      INTEGER LOC,ID,IBSET
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZUSER.LINK/LIST'
      INTEGER LUSER,LPOIN
      PARAMETER (NWIRES=7)
      INTEGER NBSENS,TAGBIT
      PARAMETER (TAGBIT=30)
      PARAMETER (NBSENS=8)
      INTEGER INFHIT(8,NBSENS),NTHIT(NBSENS),PTHIT(NBSENS)
      REAL HITINF(8,NBSENS)
      EQUIVALENCE (INFHIT,HITINF)
      REAL AL,XG,YG,ALZ,SG,ZG,VZG,VAL,VALZ,VG,CHISQ,CHIMAX,CHISQZ
      REAL COSAL, SINAL
      REAL XHIT(NBSENS),YHIT(NBSENS),RHIT(NBSENS),ZHIT(NBSENS)
      REAL WT(NBSENS),WTZ(NBSENS),SXY(NBSENS)
      REAL VERTX, VERTY, FACTOR, VZGTHETA
      LOGICAL NEW
      REAL PI,PI2
      REAL PHIG, PHIDIFF
      INTEGER LISTPT(NBSENS)
      INTEGER IWIRE
      INTEGER NUM,LABL,FIRSPL,LASTPL
      INTEGER IER,ICALL,INEFF,NUSED
      INTEGER RUN,EVENT,RUNNO,EVONUM,RUN_LAST,EVENT_LAST,STATUS
      REAL    ZVTX,ZVTX_LAST,DX,DY,PHIRD,DZDRRD
      SAVE ICALL,CHIMAX,NEW
      DATA PI,PI2/3.141593,6.283185/
      DATA ICALL/0/
C-------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('CHIMAX',CHIMAX,IER)
        CALL EZGET('INEFF',INEFF,IER)
        CALL EZRSET
        NEW=.TRUE.
        ICALL=1
        EVENT_LAST = -1
        RUN_LAST = -1
        ZVTX_LAST = -999.
      END IF
      EVENT = EVONUM()
      RUN   = RUNNO()
      IF (EVENT .NE. EVENT_LAST .OR. RUN .NE. RUN_LAST) THEN
        RUN_LAST = RUN
        EVENT_LAST = EVENT
        CALL VRDGET(ZVTX,PHIRD,DZDRRD)
        ZVTX_LAST = ZVTX
        IF (ZVTX.GE.9999) ZVTX=0. 
        CALL VXY_BEAM1(ZVTX,VERTX,DX,VERTY,DY,STATUS)
      ELSE
        CALL VRDGET(ZVTX,PHIRD,DZDRRD)
        IF (ZVTX .NE. ZVTX_LAST) THEN
          ZVTX_LAST = ZVTX
          IF (ZVTX.GE.9999) ZVTX=0. 
          CALL VXY_BEAM1(ZVTX,VERTX,DX,VERTY,DY,STATUS)
        ENDIF
      ENDIF
C
      LUSER=LQ(LHEAD-IZUSER)
      LPOIN=LQ(LUSER-1)
      CALL UCOPY(IQ(LPOIN+1),NTHIT,NBSENS)
C
C *** store relative pointers in PTHIT
C  
      PTHIT(1)= 9
      DO 1 IWIRE=1,NBSENS
        IF (IWIRE.GT.1) PTHIT(IWIRE)=PTHIT(IWIRE-1)+NTHIT(IWIRE-1)*8
        IF (LISTPT(IWIRE).NE.0) THEN
          LOC=PTHIT(IWIRE)+(LISTPT(IWIRE)-1)*8 + LPOIN
          CALL UCOPY(Q(LOC),HITINF(1,IWIRE),8)
        END IF
    1 CONTINUE
      FIRSPL = 0
      LASTPL = 0
      NHIT=0
      DO 10 IWIRE = 1, NBSENS
        IF( LISTPT(IWIRE) .NE. 0 ) THEN
          LASTPL = IWIRE
          IF( FIRSPL .EQ. 0 ) FIRSPL = IWIRE
          NHIT=NHIT+1
          RHIT(NHIT) = HITINF( 1, IWIRE )
          XHIT(NHIT) = HITINF( 5, IWIRE )
          YHIT(NHIT) = HITINF( 6, IWIRE )
          ZHIT(NHIT) = HITINF( 7, IWIRE )
          WTZ(NHIT)  = HITINF( 8, IWIRE )
          WT(NHIT)   = HITINF( 3, IWIRE )
          LABL       = INFHIT( 4, IWIRE )
          LABL       = IBCLR(LABL,TAGBIT)
          IHIT(NHIT) = LABL/16
          LRWIR(NHIT)= LABL-IHIT(NHIT)*16
        ENDIF
   10 CONTINUE
C
C ****  If CHI/DOF is too big, throw away worst hits
C
      NUSED = NHIT
   11 CALL FITLIN(XHIT,YHIT,WT,NHIT,AL,XG,YG,VG,VAL,CHISQ)
      IF (CHISQ .LT. CHIMAX) GO TO 12
      IF (NUSED .LE. 8-INEFF) GO TO 1000
      NUSED = NUSED - 1
      CALL VTHROW(XHIT,YHIT,WT,NHIT,AL,XG,YG)
      GO TO 11
   12 CONTINUE
C
C ****  Segment is good -- now clean things up if necessary
C
      IF (NHIT .NE. NUSED) THEN
        FIRSPL = 0
        LASTPL = 0
        NHIT = 0
        NUSED= 0
        DO IWIRE = 1,NBSENS
          IF  (LISTPT(IWIRE) .NE. 0) THEN
            NHIT = NHIT + 1
            IF(WT(NHIT) .GT. 0.) THEN
              LASTPL = IWIRE
              IF (FIRSPL .EQ. 0) FIRSPL = IWIRE
              NUSED = NUSED + 1
              IF (NUSED .NE. NHIT) THEN
                RHIT(NUSED) = HITINF(1,IWIRE)
                XHIT(NUSED) = HITINF(5,IWIRE)
                YHIT(NUSED) = HITINF(6,IWIRE)
                ZHIT(NUSED) = HITINF(7,IWIRE)
                WTZ(NUSED)  = HITINF(8,IWIRE)
                WT(NUSED)   = HITINF(3,IWIRE )
                LABL        = INFHIT(4,IWIRE)
                LABL        = IBCLR(LABL,TAGBIT)
                IHIT(NUSED) = LABL/16
                LRWIR(NUSED)= LABL-IHIT(NUSED)*16
              ENDIF
            ELSE
              LISTPT(IWIRE) = 0
            ENDIF
          ENDIF
        ENDDO
        NHIT = NUSED
      ENDIF
           
C
C ****  Make sure the phi angle is not off by PI - it should be very close to
C ****  the phi of the center of gravity.  Then force it into the range from 0
C ****  to twopi.
C
      PHIG = ATAN2(YG,XG)
      PHIDIFF = PHIG - AL
      PHIDIFF = PHIDIFF - NINT(PHIDIFF/PI)*PI
      AL = PHIG - PHIDIFF
      IF (AL.LT.0.) AL=AL+PI2
      IF (AL.GT.PI2) AL=AL-PI2
      SINAL = SIN(AL)
      COSAL = COS(AL)
      DO 300 ID=1,NHIT
        SXY(ID)=(XHIT(ID)-XG)*COSAL + (YHIT(ID)-YG)*SINAL
  300 CONTINUE
      CALL FITLOC(SXY,ZHIT,WTZ,NHIT,ALZ,SG,ZG,VZG,VALZ,CHISQZ)
C
C ****  Convert to D0 frame and calculate errors accordingly
C
      ZG = ZG - SG*TAN(ALZ)
      ALZ=1.570796-ALZ
      FACTOR = SG/(SIN(ALZ)**2)
      VZG = VZG + VALZ*FACTOR**2
      VZGTHETA = VALZ*FACTOR
C
C ****  Now translate the segment back to D0 coords (segments are found in the
C ****  frame where Xbeam=Ybeam=0)
C
      XG = XG + VERTX
      YG = YG + VERTY
C
C ****  Store the segment in bank VSGn (n=layer)
C
      CALL LDVSEG(LAYER,SECTOR,NHIT,LRWIR,IHIT,AL,XG,YG,VG,VAL,
     1    CHISQ,ALZ,VZGTHETA,ZG,VZG,VALZ,CHISQZ,NEW)
C
C ****  Tag hit as being used ( no future seed )
C
      LUSER=LQ(LHEAD-IZUSER)
      LPOIN=LQ(LUSER-1)
      DO 145 IWIRE = FIRSPL+1, LASTPL-1
        IF( LISTPT(IWIRE) .NE. 0 ) THEN
          NUM = LISTPT(IWIRE)
          LABL = INFHIT( 4, IWIRE)
          IF( BTEST( LABL,TAGBIT ) ) GOTO 145
          INFHIT( 4, IWIRE ) = IBSET( LABL,TAGBIT)
          LOC=PTHIT(IWIRE)-1+(LISTPT(IWIRE)-1)*8+4 + LPOIN
          IQ(LOC)=INFHIT(4,IWIRE)
C ----> The following code tags the mirror hits
C          LABL = LABL + 1
C          IF( .NOT. BTEST( LABL, 0 ) ) LABL = LABL - 2
C          DO 146 IH = 1, NTHIT( IWIRE )
C            IF( INFHIT(4,IH,IWIRE) .EQ. LABL ) THEN
C              INFHIT(4,IH,IWIRE) = IBSET( LABL, TAGBIT )
C              LOC=PTHIT(IWIRE)-1+(LISTPT(IWIRE)-1)*8+4 +LPOIN
C              IQ(LOC)=INFHIT(4,IH,IWIRE)
C              GOTO 145
C            ENDIF
C  146     CONTINUE
        ENDIF
  145 CONTINUE
C
C  Increment number of used hits in this sector
C
      IQ(LUSER+2)=IQ(LUSER+2)+NUSED
 1000 RETURN
      END
