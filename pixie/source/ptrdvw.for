      SUBROUTINE PTRDVW
C====================================================================
C
C  Description:  Displays end view of TRD.  This display will not write the
C  ============     energy realesed in each sector.
C
C
C  Author:
C  =======
C  Sharon Hagopian
C
C  Revision History:
C  =================
C  Original Creation -    June 6,1986
C  Revised by Lupe Rosas  Jan     1989
C  Revised by Lupe Howell Jan, 10 1990 Implementing Color Table
C  Updated   6-JUN-1990   Norman A. Graf
C  Updated  25-SEP-1990   Lupe Howell  Implementing PIXIE_RCP
C  Updated  15-APR-1991   Lupe Howell  The Hardcopy choice only if TRD ONLY.
C  Updated   3-OCT-1991   Lupe Howell  The TRD Emin displayed only if TRD ONLY
C  Updated  18-NOV-1991   JFG  Implementing the use of the cathodes toggle
C  Updated  19-FEB-1992   Lupe Howell   Remove seflt nested DO loop fo SGI
C
C=====================================================================

C  Local Declarations:
C  ====================
      IMPLICIT NONE
C=====================================================================
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:GEOMTR.INC/LIST'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.inc'
C=====================================================================
      REAL RANG,COR_ENRG(3),SATURATION,ETH_INF(3),ETH_SUP(3)
      REAL RADII(2)
      REAL ANG(2)
      REAL R(4) ! RADII OF TRD LAYERS
      REAL DELANG ! ANGULAR 1/2 WIDTH OF CELL IN DEGREES
      REAL ENRG,PTGEGT
      REAL EMIN  ! MINIMUM ENERGY/CELL TO APPEAR IN DISPLAY
      REAL HENRG        ! Highest energy released per layer
      REAL SCALE        ! Scale to determine sector size
      REAL ENRMAX       ! Highest energy released in all layers
      REAL MXENRG(3)    ! Max. energy/layer
      REAL URANIUM(3)
C
      INTEGER MXWIR(3)
      INTEGER ENDVIEW      ! Retained segment number for TRD end view
      INTEGER ILAY,ICELL,ANODE
      INTEGER WIRE      ! Wire with the highest enrgy per layer
      INTEGER TYP, IER
C
      CHARACTER*3 PARCOL, ICOL1, ICOL2, KCOLOR
      CHARACTER*24  MESS    ! Message to be displayed
      CHARACTER*8   CEMIN   ! Charc energy minimum
      CHARACTER*8 CX        ! Dummy var
      CHARACTER*8 CANODE,CCATHODE
      CHARACTER*4 CVAL, REM
C
      LOGICAL TRONLY,FLGVAL,E_IN_MIPS
      LOGICAL EZERROR,DRAW
      LOGICAL TRUTH

C=====================================================================
C   Data Declaration:
C   =================
      DATA R/17.50,28.05,38.60,49.15/
      DATA DELANG/.7031/
      DATA ICOL1,ICOL2/'FOR','BLA'/  ! Foreground, Black
      DATA PARCOL/'BLU'/
      DATA ANODE /1/
      DATA DRAW /.TRUE./
      DATA CANODE,CCATHODE/'  ANODE ',' CATHODE'/
      DATA SATURATION/10./ ! Saturation at 10 MIPS
C=====================================================================
C
C ****  Picking PIXIE RCP
C
      CALL EZPICK('PX_TRDDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PTRDVW','Cannot find PX_TRDDIS_RCP','W')
        GOTO 999
      ENDIF
C
C ****  Get some TRD constants
C
      CALL EZ_GET_ARRAY('PXPARAMS','TRD EMIN',1,EMIN,CVAL,
     &       TYP,REM,IER) ! READ THRESHOLD VALUE IN MIP
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('PIXIE','PTRDVW',
     &      'PXPARAMS NOT FOUND','W')
        GOTO 900
      ENDIF
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
          IF(URANIUM(ILAY).NE.0.)COR_ENRG(ILAY)=URANIUM(ILAY)/2.
        ELSE
          ETH_INF(ILAY)=EMIN*URANIUM(ILAY)/2.
          ETH_SUP(ILAY)=SATURATION*URANIUM(ILAY)/2.
        END IF
      END DO

      CALL EZ_GET_ARRAY('PXPARAMS','TRD ONLY',1,TRONLY,CVAL,
     &       TYP,REM,IER)
C
C ****  Check on the TRD_ANO_CATH flag to define
C ****  the ANODE variable to draw the correct display
C
      TRUTH = FLGVAL('TRD_ANO_CATH')
      IF (TRUTH.EQ..TRUE.) THEN
        ANODE = 1
      ELSE
        ANODE = 2
      ENDIF
C
C ****  Cell By Cell Loop Process
C
      CALL PUOPEN
C      ENRMAX = 0.
C      DO ILAY = 1,3
C        CALL PTHELA(ILAY,ANODE,WIRE)   ! Getting the wire with the highest enrg
C        HENRG=PTGEGT(ILAY,WIRE,ANODE)  ! Getting the enrg from wire
CC Store values for LABEL
C        MXENRG(ILAY)=HENRG
C        MXWIR(ILAY)=WIRE
C        IF(HENRG .GT. ENRMAX) ENRMAX = HENRG
C      ENDDO
      DO 100 ILAY=1,3
        DELANG=180./NWIRE_PER_LAYER(ILAY)
        MXENRG(ILAY)=0.
        MXWIR(ILAY)=0.
        DO 99 ICELL=1,NWIRE_PER_LAYER(ILAY)
          RANG=0.+(FLOAT(ICELL)-1.)*DELANG*2.
C
C ****  Get current viewport values
C
          CALL PXCOLFILL(PARCOL)
          CALL JPINTR(0)
          IF(ICELL.EQ.1)THEN
            CALL PXCOLR(PARCOL)
            CALL JCIRCL(0.,0.,0.,R(ILAY),0)
          ENDIF
C
C ****  Calculate the center position of the first drawn sector, in this
C ****  case, sector 0 is drawn first.
C
          ANG(1)=(OFSDPH(ILAY)+(ICELL-1)*2*PI/
     &                          FLOAT(NWIRE_PER_LAYER(ILAY)))/RADIAN
          ANG(2)=(OFSDPH(ILAY)+ ICELL*2*PI/
     &                          FLOAT(NWIRE_PER_LAYER(ILAY)))/RADIAN
          RADII(1)=R(ILAY)
          RADII(2)=R(ILAY+1)
C
C ****  Draw sector in LAYER
C
          ENRG=PTGEGT(ILAY,ICELL,ANODE)/COR_ENRG(ILAY)      ! Gets ENG value
C Store values for LABEL
          IF(ENRG.GT.MXENRG(ILAY))THEN
            MXENRG(ILAY)=ENRG
            MXWIR(ILAY)=icell
          END IF
          IF(ENRG.LE.EMIN) GO TO 99
          ENRMAX=ETH_SUP(ILAY)
          SCALE=AMIN1(ENRG/ENRMAX,1.)         ! Calc the scale for sector size
          CALL PTECUT(SCALE*10000.,KCOLOR)     ! Detrmines color
          CALL PXCOLFILL(KCOLOR)
          CALL PXCOLR(KCOLOR)
          CALL PTSECT(RADII,ANG,SCALE)       ! Draws the sector
   99   CONTINUE
  100 CONTINUE
      CALL JPINTR(0)
      CALL PXCOLR(PARCOL)
      CALL JCIRCL(0.,0.,0.,R(4),0)       ! Drawing the outer most circle
C
C ****  Writing label messages
C
      IF(TRONLY)THEN
        CALL PTLCOL                     ! Drawing the color scale
        CALL PTLEMX(ANODE,MXWIR,MXENRG,PARCOL)  ! Drawing max eng wir/lay label
      ENDIF
      CALL JRCLOS
C
CC      ENDVIEW = MAXSEG
C
C ****  Drawing the EMIN if TRD ONLY
C
      IF ( TRONLY ) THEN
        IF(ANODE.EQ.1)THEN
          CALL PUMESS(CANODE)
        ELSE
          CALL PUMESS(CCATHODE)
        ENDIF
        WRITE(CX,120)EMIN
  120   FORMAT(F8.0)
        READ(CX,122)CEMIN
  122   FORMAT(A8)
        MESS='      TRD EMIN= '//CEMIN
        CALL PUMESS(MESS)
      ENDIF
C
C ****  Reseting RCP file
C
  900 CALL EZRSET
  999 CONTINUE
      RETURN
      END
