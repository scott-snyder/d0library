      SUBROUTINE PT1TRK(LTRDT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : draw TRD information associated with PELC track
C-
C-   Inputs  : LTRDT: TRD bank address associated with PELC track
C-   Outputs : none
C-
C-   Created   3-AUG-1990   Qizhong Li-Demarteau   modified from PTRDTK
C-   Updated  21-FEB-1991   A. Zylberstejn  : Change energy calibration
C-                             constant according to TRDT bank version
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GRAPHF77.INC'
      INCLUDE 'D0$INC:GEOMTR.INC/LIST'
      INCLUDE 'D0$INC:GCONST.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL RANG
      REAL RAD(2)
      REAL ANG(2)
      REAL R(4) ! RADIOUS OF TRD LAYERS
      REAL DELANG ! ANGULAR 1/2 WIDTH OF CELL IN DEGREES
      REAL ENRG,PTGEGT
      REAL EMIN  ! MINIMUM ENERGY/CELL TO APPEAR IN DISPLAY
      REAL HENRG        ! COUNT FOR MIP
      REAL SCALE        ! Scale to determine sector size
      INTEGER ICOL1,ICOL2
      INTEGER ILAY,ICELL,ANODE
      INTEGER WIRE      ! Wire with the highest enrgy per layer
      CHARACTER*24  MESS    ! Message to be displayed
      CHARACTER*8   CEMIN   ! Charc energy minimum
      CHARACTER*8 CX        ! Dummy var
      CHARACTER*3  KCOLOR, PARCOL
      INTEGER I,LTPRL,LTRDT,GZTRDT,NTRAK
      REAL VMIN,VMAX
      LOGICAL FLGVAL,TRONLY,FIRST
      INTEGER ITK,IWIR
C
      DATA R/17.50,28.05,38.60,49.15/
      DATA DELANG/.7031/
      DATA ICOL1,ICOL2/0,8/
      DATA ANODE /1/
      DATA HENRG/950./
C----------------------------------------------------------------------
C
      IF (LTRDT .LE. 0) GOTO 999
      IF(IQ(LTRDT+1).GE.1.)HENRG=1.
      CALL PUGETV('TRD EMIN',EMIN) ! EMIN in min. ion.
      ITK=0
      IF(IQ(LTRDT+3).LE.0)GO TO 999
      ITK=ITK+1
      DO 40 ILAY=1,3
        LTPRL=LQ(LTRDT-ILAY)
        IF(Q(LTPRL+12).LE.0. .OR. IQ(LTPRL+14).LE.0)GO TO 40
        DO 16 IWIR=1,IQ(LTPRL+14)
          ICELL=Q(LTPRL+17+IWIR)
          ENRG=Q(LTPRL+17+IWIR+IQ(LTPRL+14))
C  Calculate the center position of the first drawn sector, in this
C  case, sector 0 is drawn first.
C  =================================================================
          ANG(1)=(OFSDPH(ILAY)+(ICELL-1)*2*PI/256)*RADDEG
          ANG(2)=(OFSDPH(ILAY)+    ICELL*2*PI/256)*RADDEG
          RAD(1)=R(ILAY)
          RAD(2)=R(ILAY+1)
C  Draw sector in LAYER
C  =========================
          IF(ENRG.LE.EMIN*HENRG) GO TO 16
C FULL SCALE = 4 *minimum ionizing
          SCALE=ENRG/(HENRG*4.)! Calc the scale for sector size
          SCALE=AMIN1(1.,SCALE)
          CALL PUOPEN
          CALL PTECUT(SCALE*10000.,KCOLOR)     ! Detrmines color
          IF(KCOLOR.NE.'   ')THEN
            CALL PXCOLFILL(KCOLOR)
            CALL PXCOLR(KCOLOR)
            CALL PTSECT(RAD,ANG,SCALE)       ! Draws the sector
          ENDIF
          CALL JRCLOS
   16   CONTINUE
   40 CONTINUE
C
  999 RETURN
      END
