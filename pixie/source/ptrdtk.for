      SUBROUTINE PTRDTK
C====================================================================
C
C  Description:  Displays end view of TRD with the energy for each track.
C  ============
C
C
C  Author:
C  =======
C  Sharon Hagopian
C
C  Revision History:
C  =================
C  Original Creation - S. Hagopian Oct. 11, 1989
C-   Updated   9-FEB-1990   A. Zylberstejn  Use Zebra banks
C-   Updated   4-JUN-1990   Norman A. Graf  Insert call to PTPICK
C-   Updated  21-FEB-1991   A. Zylberstejn
C-   Updated  16-JAN-1992   J.F. DET×UF ESQ.
C-   Updated  17-JAN-1992   JFG  Remove call to PTPICK
C-   Updated  25-JAN-1992   JFG  Add variable BTOMIPS fot compatibility
C-                          with the TRD HIT display package.
C-   Updated 24-MAR-1992    S. Hagopian, changed call to PTLEMX
C-   Updated  26-NOV-1992   A. Zylberstejn  : change normalization
C-   Updated   7-JAN-1994   S. Hagopian, use UNPACK_TPRL
C=====================================================================

C  Local Declarations:
C  ====================
      IMPLICIT NONE
C=====================================================================
      INCLUDE 'D0$INC:GRAPHF77.INC'
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$INC:TRD_NWORD.INC'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.inc'
      INCLUDE 'd0$inc:trener.inc/LIST'
      INCLUDE 'D0$INC:TRINTR.INC/LIST'
      INCLUDE 'D0$INC:GEOMTR.INC/LIST'
      INCLUDE 'D0$INC:GCONST.INC/LIST'
      INCLUDE 'D0$INC:trhitw.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C=====================================================================
      REAL RANG,COR_ENRG(3),SATURATION,ETH_INF(3),ETH_SUP(3),EPICOR(3)
      REAL RAD(2)
      REAL ANG(2)
      REAL R(4) ! RADIOUS OF TRD LAYERS
      REAL DELANG ! ANGULAR 1/2 WIDTH OF CELL IN DEGREES
      REAL ENRG,PTGEGT
      REAL EMIN  ! Minimum energy/cell to appear in display in MIPS units
      REAL HENRG        ! FADC counts for Min. bias after normalization
      REAL SCALE        ! Scale to determine sector size
      REAL MXENRG(3)    ! Max energy of a sector of a track/layer
      REAL VNTPRL
      INTEGER NHITA
      INTEGER MXWIR(3)  ! Wire number with max. energy/layer
      INTEGER ICOL1,ICOL2
      INTEGER ILAY,ICELL,ANODE
      INTEGER WIRE      ! Wire with the highest enrgy per layer
      INTEGER TYP, IER
      REAL URANIUM(3)
      CHARACTER*24  MESS    ! Message to be displayed
      CHARACTER*8   CEMIN   ! Charc energy minimum
      CHARACTER*8 CX        ! Dummy var
      CHARACTER*3  KCOLOR, PARCOL
      CHARACTER*4 CVAL, REM
      INTEGER I,LTPRL,LTRDT,GZTRDT,NTRAK
      REAL VMIN,VMAX,BTOMIPS
      LOGICAL FLGVAL,TRONLY,FIRST,MCDATA,ELEC_ONLY,VLOGIC,E_IN_MIPS
      LOGICAL DRAW, EZERROR
      INTEGER ITK,IWIR
C
      INTEGER ENDVIEW     ! Retained segment number for TRD end view
C
C=====================================================================
C   Data Declaration:
C   =================
      DATA R/17.50,28.05,38.60,49.15/
      DATA DELANG/.7031/
      DATA ICOL1,ICOL2/0,8/
      DATA HENRG/950./
      DATA SATURATION/10./
      DATA PARCOL/'BLU'/
      DATA FIRST/.TRUE./
      DATA MCDATA/.FALSE./
      DATA EPICOR/318.7,394.2,369.6/ ! From TRD.RCP

C=====================================================================
C  HRMIN is the average value of a Min. Ionizing Particle
C  EMIN is the percentage of HRMIN under which the information in not
C   displayed (typically EMIN=.1)
C
C  Executable Code:
C  =================
C
      DRAW = .TRUE.
C ****  Picking PIXIE RCP files
C
      CALL EZPICK('PX_TRDDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PTRDTK','Cannot find PX_TRD_RCP','W')
        GO TO 999
      ENDIF
C
C ****  Get some TRD constants
C
      CALL EZGET('URANIUM_LAYER1',URANIUM(1),IER)
      CALL EZGET('URANIUM_LAYER2',URANIUM(2),IER)
      CALL EZGET('URANIUM_LAYER3',URANIUM(3),IER)
      COR_ENRG(1)=1.
      COR_ENRG(2)=1.
      COR_ENRG(3)=1.
      CALL EZ_GET_ARRAY('PXPARAMS','MIPS',1,E_IN_MIPS,CVAL,
     &       TYP,REM,IER)
      DO ILAY=1,3
        IF(E_IN_MIPS)THEN
          ETH_INF(ILAY)=EMIN
          ETH_SUP(ILAY)=SATURATION
          IF(URANIUM(ILAY).NE.0.)COR_ENRG(ILAY)= URANIUM(ILAY)/2.
        ELSE
          ETH_INF(ILAY)=EMIN*URANIUM(ILAY)/2.
          ETH_SUP(ILAY)=SATURATION*URANIUM(ILAY)/2.
        END IF
      END DO
      CALL EZ_GET_ARRAY('PXPARAMS','TRD EMIN',1,EMIN,CVAL,
     &       TYP,REM,IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('PIXIE','PTRDTK',
     &      'PXPARAMS NOT FOUND','W')
        GOTO 995
      ENDIF
      CALL EZ_GET_ARRAY('PXPARAMS','TRD ONLY',1,TRONLY,CVAL,
     &       TYP,REM,IER)
      CALL EZ_GET_ARRAY('PXPARAMS','TRD BIN_TOMIPS',1,BTOMIPS,CVAL,
     &       TYP,REM,IER)
C Fetch parameter to draw TRD for electron candidates
      CALL EZ_GET_ARRAY('PXPARAMS','TRD ELEC_ONLY',1,VLOGIC,CVAL,
     &       TYP,REM,IER)
      ELEC_ONLY=.FALSE.
      IF ( IER .EQ. 0 ) THEN
        ELEC_ONLY=VLOGIC
      END IF
      ANODE=1
  900 CONTINUE
      LTRDT=GZTRDT()
      IF(LTRDT.LE.0)GO TO 995
      IF(FIRST)THEN
        IF (IQ(LHEAD+1) .GT. 1000) MCDATA = .TRUE.
        HENRG=BTOMIPS  !Average value of energy for min bias
        PI=ACOS(-1.)
        RADDEG=180./PI
        FIRST=.FALSE.
      END IF
      nbthit(1,1)=0
      nbthit(2,1)=0
      nbthit(3,1)=0
      CALL PUOPEN
C DRAW CIRCLE BOUNDARIES
      CALL PXCOLR(PARCOL)
      DO 5 ILAY=1,4
    5 CALL JCIRCL(0.,0.,0.,R(ILAY),0)
      ITK=0
      CALL VZERO(MXENRG,3)
      CALL VZERO(MXWIR,3)
   10 IF(LTRDT.LE.0)GO TO 100
      IF(IQ(LTRDT+3).LE.0)GO TO 60 ! Check if there are hit anodes
C If ELEC_ONLY is requested keep tracks with total energy>3*MIPS
      IF(ELEC_ONLY)THEN ! Check if link to calo
        IF(lQ(LTRDT-5).Le.0)GO TO 60
      END IF
      ITK=ITK+1
      DO 40 ILAY=1,3
        LTPRL=LQ(LTRDT-ILAY)
        IF(LTPRL.LE.0)GO TO 40
        CALL VZERO(REAL_WORD,NWORD)
        CALL VZERO(INTEGER_WORD,NWORD)
C unpack wire energies
        CALL UNPACK_TPRL(LTPRL,VNTPRL,REAL_WORD,INTEGER_WORD,IER)
        IF(IER.NE.0)GO TO 40
C check if there are any anode hits in this layer
        NHITA=INTEGER_WORD(4)
        IF(NHITA.LE.0)GO TO 40
C        IF(IQ(LTPRL+14).LE.0)GO TO 40
C        IF(Q(LTPRL+12).LE.EMIN*HENRG) GO TO 40
        DO 16 IWIR=1,NHITA
          ICELL=INTEGER_WORD(50+IWIR)
C We assume that in TPRL energies are in FADC/EPICOR
          ENRG=REAL_WORD(50+IWIR) *EPICOR(ILAY)/COR_ENRG(ILAY)
          nbthit(ilay,1)=nbthit(ilay,1)+1
          NUMTWH(nbthit(ilay,1),ilay,1)=icell
          entwh(nbthit(ilay,1),ilay,1)=enrg
          IF(ENRG.GT.MXENRG(ILAY))THEN
            MXENRG(ILAY)=ENRG
            MXWIR(ILAY)=ICELL
          ENDIF
C  Calculate the center position of the first drawn sector, in this
C  case, sector 0 is drawn first.
C  =================================================================
          ANG(1)=(OFSDPH(ILAY)+(ICELL-1)*2*PI/NWIRE_PER_LAYER(ILAY))
     &      *RADDEG
          ANG(2)=(OFSDPH(ILAY)+    ICELL*2*PI/NWIRE_PER_LAYER(ILAY))
     &      *RADDEG
          RAD(1)=R(ILAY)
          RAD(2)=R(ILAY+1)
C  Draw sector in LAYER
C  =========================
          IF(ENRG.LE.ETH_INF(ILAY)) GO TO 16
C FULL SCALE = saturation *minimum ionizing
          SCALE=ENRG/ETH_SUP(ILAY)   ! Calc the scale for sector size
          SCALE=AMIN1(1.,SCALE)
          CALL PTECUT(SCALE*10000.,KCOLOR)     ! Detrmines color
          IF(KCOLOR.NE.'   ')THEN
            CALL PXCOLFILL(KCOLOR)
            CALL PXCOLR(KCOLOR)
            CALL PTSECT(RAD,ANG,SCALE)       ! Draws the sector
          ENDIF
   16   CONTINUE
   40 CONTINUE
   60 CONTINUE
      LTRDT=LQ(LTRDT)
      GO TO 10
  100 CONTINUE
  110 CONTINUE
C  Writing label messages
C  ======================
      IF(TRONLY)THEN
        CALL PTLCOL                     ! Drawing the color scale
        CALL PTLEMX(ANODE,MXWIR,MXENRG,PARCOL)  ! Drawing max eng wir/lay label
      ENDIF
      CALL JRCLOS
      ENDVIEW = MAXSEG
C  Drawing the EMIN only if TRONLY
      IF(TRONLY) THEN
        WRITE(CX,120)EMIN
  120   FORMAT(F8.0)
        READ(CX,122)CEMIN
  122   FORMAT(A8)
        MESS='      TRD EMIN= '//CEMIN
      ENDIF
C
C ****  Reset RCP file
C
  995 CALL EZRSET
  999 CONTINUE
      RETURN
      END
