      SUBROUTINE PTZSTRIP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : plot TRD ZSTRIP info
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  10-JAN-1994   Sharon Hagopian
C-   Updated  24-FEB-1994   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,LAYER,LTPRL,LTRDT,GZTRDT,NZ(3),NMAX,NTRACK
      PARAMETER( NMAX =50  )
      REAL R(4),TRD_ZPOS,ENERG(NMAX,3),ZTRD(NMAX,3),PHITRD(NMAX,3)
      REAL ZMIN,ZMAX,HALFZ
      REAL ENRG,ENRMAX,R1,R2,SCALE,Z1,Z2,ZT,SCALZR,EPICOR(3),PHITR
      REAL URANIUM(3),COR_ENRG(3),SATURATION,ETH_INF(3),ETH_SUP(3)
      INTEGER  LVERH, GZVERH, NV, IV
      REAL ZVER(10),DZVER(10)
      REAL DELZ
      LOGICAL FIRST,EZERROR
      INTEGER IER,TYP
      REAL EMIN,DELTZ
      CHARACTER*4 CVAL,REM
      CHARACTER*3 PARCOL, ICOL1, ICOL2, KCOLOR
      LOGICAL TRONLY,FLGVAL,E_IN_MIPS
      INCLUDE 'D0$INC:WORKSP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      EQUIVALENCE(ZTRD(1,1),WS(1)),(ENERG(1,1),WS(500)),
     &  (PHITRD(1,1),WS(1000))
C   DATA Declaration:
C   =================
      DATA R/17.50,28.05,38.60,49.15/
      DATA HALFZ/83.5/
      DATA FIRST/.TRUE./
      DATA SATURATION/10./ ! Saturation at 10 MIPS
      DATA EPICOR/318.7,394.2,369.6/ ! From TRD.RCP
      DATA DELZ/.1/
C----------------------------------------------------------------------
c      write(73,*),'                        ### entering PZSTRIP'
C
C ****  Picking PIXIE RCP
C
      CALL EZPICK('PX_TRDDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PTRDVW','Cannot find PX_TRDDIS_RCP','W')
        GOTO 990
      ENDIF
C
C ****  Get some TRD constants
C
      CALL EZ_GET_ARRAY('PXPARAMS','TRD EMIN',1,EMIN,CVAL,
     &       TYP,REM,IER) ! READ THRESHOLD VALUE IN MIP
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('PIXIE','PTRDVW',
     &      'PXPARAMS NOT FOUND','W')
        GOTO 990
      ELSE
        CALL PUGETV('TRD ONLY',TRONLY)
      ENDIF
      IF(FIRST)THEN
        FIRST=.FALSE.
        ZMIN=-HALFZ
        ZMAX= HALFZ
        DELTZ=2*HALFZ/256. ! to be adjusted later
        CALL EZGET('URANIUM_LAYER1',URANIUM(1),IER)
        CALL EZGET('URANIUM_LAYER2',URANIUM(2),IER)
        CALL EZGET('URANIUM_LAYER3',URANIUM(3),IER)
        COR_ENRG(1)=1.
        COR_ENRG(2)=1.
        COR_ENRG(3)=1.
        CALL EZ_GET_ARRAY('PXPARAMS','MIPS',1,E_IN_MIPS,CVAL,
     &       TYP,REM,IER)
        DO LAYER=1,3
          IF(E_IN_MIPS)THEN
            ETH_INF(LAYER)=EMIN
            ETH_SUP(LAYER)=SATURATION
            IF(URANIUM(LAYER).NE.0.)COR_ENRG(LAYER)=URANIUM(LAYER)/2.
          ELSE
            ETH_INF(LAYER)=EMIN*URANIUM(LAYER)/2.
            ETH_SUP(LAYER)=SATURATION*URANIUM(LAYER)/2.
          END IF
        END DO
      END IF
      CALL EZ_GET_ARRAY('PXPARAMS','TRD ONLY',1,TRONLY,CVAL,
     &       TYP,REM,IER)
C    draw chamber in the R-Z view
C
C Check if TRD only or combined view      
      IF(TRONLY)THEN
        SCALZR=.45
      ELSE
        SCALZR=1.
      ENDIF
      CALL PUOPEN
      CALL JLSTYL(0)       !solid lines
      CALL PXCOLR('FOR')
      CALL JMOVE(-75.,0.)
      CALL JDRAW(75.,0.)  !z_axis
c      CALL JMOVE(0.,3.)
c      CALL JDRAW(0.,-3.)   ! center_cross
C
C draw vertex if TRD ONLY
      IF(TRONLY)THEN 
        LVERH = GZVERH()
        IF ( LVERH .LE. 0 ) GO TO 151
        CALL ZVERTE(NV, ZVER, DZVER)
        IF ( NV .EQ. 0 ) GO TO 151
        CALL PXCOLR('RED')
        DO 150 IV = 1, NV
          CALL JMOVE(ZVER(IV)-2.,-2.)
          CALL JDRAW(ZVER(IV)+2.,2.)
          CALL JMOVE(ZVER(IV)-2.,2.)
          CALL JDRAW(ZVER(IV)+2.,-2.)
          CALL JRECT(ZVER(IV)-DZVER(IV),0.08,ZVER(IV)+DZVER(IV),-0.08)
  150   CONTINUE
      ENDIF
  151 CALL PXCOLR('BLU')
      DO LAYER= 1,3
        Z1 = ZMIN*SCALZR
        Z2 = ZMAX*SCALZR
        R1 = R(LAYER)*SCALZR
        R2 = R(LAYER+1)*SCALZR
        CALL JRECT(Z1,R1,Z2,R2)
        CALL JRECT(Z1,-R1,Z2,-R2)
      END DO
C
C  fill an the array ZTRD with all the hits in Z and energies
C
      LTRDT=GZTRDT()
      IF(LTRDT.LE.0)GO TO 990
      NZ(1)=0
      NZ(2)=0
      NZ(3)=0
      NTRACK=0
      DO WHILE (LTRDT.NE.0)
        NTRACK=NTRACK+1
c        write(73,*)' in ptzstrip,track',iq(ltrdt-5)
        DO 40 LAYER=1,3
          LTPRL=LQ(LTRDT-LAYER)
          if(ltprl.le.0)go to 40
C We assume that in TPRL energies are in FADC/EPICOR
          ENRG=Q(LTPRL+13)*EPICOR(LAYER)/COR_ENRG(LAYER)
c          WRITE(73,*)' Q(LTPRL+13),COR_ENRG',Q(LTPRL+13),COR_ENRG(LAYER)
          CALL TRD_ZPOS(LTRDT,LAYER,PHITR,ZT)
c          WRITE(73,*)' track',iq(ltrdt-5),' layer',LAYER,' zt',ZT,
c     &      enrg
          IF(ABS(ZT).GT.HALFZ)GO TO 40
c          WRITE(73,*)' enrg',ENRG,' emin',EMIN
C          IF(ENRG.LT.EMIN) GO TO 40
          NZ(LAYER)=NZ(LAYER)+1
          ZTRD(NZ(LAYER),LAYER)=ZT
          ENERG(NZ(LAYER),LAYER)=ENRG
          PHITRD(NZ(LAYER),LAYER)=PHITR
          IF(NZ(LAYER).GE.NMAX)GO TO 48
   40   CONTINUE
   48   LTRDT=LQ(LTRDT)
      END DO

C
C   draw all Z hits
C
      DO 100 LAYER=1,3
c        WRITE(73,*)' layer',LAYER,' nb of Z hits',NZ(LAYER)
        IF(NZ(LAYER).LE.0)GO TO 100
        WRITE(73,'(a3,10g10.4)')'z  ',(ZTRD(I,LAYER),I=1,NZ(LAYER))
        WRITE(73,'(a3,10g10.4)')'phi',(phitrd(I,LAYER),I=1,NZ(LAYER))
        WRITE(73,'(a3,10g10.4)')'en ',(energ(I,LAYER),I=1,NZ(LAYER))
        DO 80 I=1,NZ(LAYER)
          R1=R(LAYER)
          ENRMAX=ETH_SUP(LAYER)
          ENRG=ENERG(i,LAYER)
          SCALE=AMIN1(ENRG/ENRMAX,1.)    ! Calc the scale for rectangle size
          CALL PTECUT(SCALE*10000.,KCOLOR)     ! Determines color
          CALL PXCOLFILL(KCOLOR)
c          write(73,*), '### i: ',i,'  kcolor: ',kcolor,' in ptzstrip '
c     +               ,' scale:',scale,'  phi: ',phitrd(i,layer)
          CALL PXCOLR(KCOLOR)
C  draw a thin rectangle centered on z with length=scale starting at R=R1
          ZT=ZTRD(I,LAYER)*SCALZR
          IF(PHITRD(I,LAYER).GT.3.14159)THEN
            R2=-(r1+.9*scale*(r(layer+1)-r1))*SCALZR
            CALL JRECT(ZT-DELZ,-R1*SCALZR,ZT+DELZ,R2)   
          ELSE
            R2=(r1+.9*scale*(r(layer+1)-r1))*SCALZR
            CALL JRECT(ZT-DELZ,R1*SCALZR,ZT+DELZ,R2)
          END IF
   80   CONTINUE
  100 CONTINUE
  990 CALL EZRSET
      CALL PUCLOSE
  999 RETURN
      END
