      SUBROUTINE ZSTRIP(TIME, PHINT, PULWID, ZCOORD, IDTK)
C----------------------------------------------------------------------
C-   Purposes and Methods :
C-  Subroutine ZSTRIP converts a sense wire hit on a wire adjacent to
C-  a z-strip into a cluster of z-strip hits.
C-
C-  Description of z-strip numbering:
C-     Z-layers 0(unused), 2 and 4 are right-handed helixes while
C-     Z-layers 1, 3 and 5 are left-handed helixes.  Z-strips are numbered
C-     zero to NSTRPS(LAYER)-1 with strip zero crossing phi=0 at the -z end
C-     of the chamber.  More precisely, the centerline of the pads on strip
C-     zero is a helix which passes through phi=0 at the z of the centers of
C-     pads zero, the first row of pads.  The pads along a given sense wire
C-     are numbered from zero to NPADS(LAYER)-1 with pad number increasing as
C-     z increases.  Z-layer 2 is split at z=0 but the strips and pads in
C-     this layer are numbered as if the strips were not split.  The two ends
C-     are distinguished by including an end bit (0 for -z end, 1 for +z end).
C-     The end bit is also present on unsplit channels and indicates which end
C-     of the strip is read out.
C-
C-   Inputs  :
C-    LAYER, SECTOR, WIRE in VTLOCA
C-    TIME       time of pulse on sense wire.
C-    PHINT      pulse height (integrated).
C-    PULWID     pulse width.
C-    ZCOORD     z coordinate of hit on sense wire.
C-    IDTK       GEANT track ID
C-   Outputs :
C-     Loads COMMON /VZDATA/ with z-strip data.
C-   Controls: none
C-
C-        T. Trippe, Sep. 24, 1986
C-     Modified 15-DEC-1988   Peter Grudberg Use geometry banks; update
C-                                      to match hardware
C-     Updated 24-MAY-1989  Peter Grudberg - changed parameter PHFAC
C-     Modified 16-NOV-1989 P.G. - use common VZDATA insted of ZHITSV
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:VZDATA.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:VTLOCA.INC/LIST'
C
      INTEGER LVZST, GZVZST, NWZLYR, I, IDTK
      REAL TIME, PHINT, PULWID, ZCOORD
      INTEGER IPAD0, IPAD1, IPAD2, IPAD, IZLAY, ISTRIP, IEND, ITIME
      REAL HALFZL, ZEDGE, SIGMA, FREQ, FREQL, FREQH, PFRAC, PULSE, DEND
C
C **** Z-strip constants:
C
      INTEGER NPADS(0:5),NSTRPS(0:5),NSECS(0:5)! for z-layer 0 to 5
      REAL ZL(0:5), WITOZS(0:5), ZLHELX(0:5), PHFAC, VEL, DZ(0:5)
      DATA PHFAC /0.75/  ! ratio of induced z-strip to sense-wire pulse height.
      DATA VEL /20.0/    ! velocity of light in z-strips (cm/ns).
                         ! Guessing=2/3c.
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
C **** Get constants from VZST
C
        LVZST = GZVZST()
        NWZLYR = IC( LVZST + 2 )
        DO 20 I = 0 , 5
          NSTRPS(I) = IC( LVZST + 3 + I*NWZLYR )         ! number of strips
          NPADS(I)  = IC( LVZST + 5 + I*NWZLYR )         ! number of pads
          NSECS(I)  = IC( LVZST + 6 + I*NWZLYR )         ! number of phi-sectors
          ZL(I) = C( LVZST + 7 + I*NWZLYR )              ! z-length (cm)
          DZ(I) = C( LVZST + 8 + I*NWZLYR )              ! pad spacing
          ZLHELX(I) = C( LVZST + 9 + I*NWZLYR )          ! heliz length
          WITOZS(I) = C( LVZST + 11 + I*NWZLYR )         ! wire to pad separation
   20   CONTINUE
      ENDIF
C
C **** Which z-strip layer (2-5) gets signal?  Layers 0,1 not installed.
C **** Wires 0-3 can induce pulses on the inner z layer, and wires 4-7 
C **** can induce pulses on the outer z layer.
C
      IF ( LAYER .LE. 0 ) GO TO 999
      IF( WIRE .LE. 3 ) THEN
        IZLAY = 2 * LAYER               ! inner layer
      ELSE
        IZLAY = 2 * LAYER + 1           ! outer layer
      END IF
C
C **** Which pads are affected? (5 pads maximum, pad numbers must be
C **** between 0 and NPAD(IZLAY)-1)
C
      HALFZL = ZL(IZLAY) / 2.
      IPAD0 = INT((HALFZL+ZCOORD) / DZ(IZLAY))    ! pad with biggest signal
      IPAD1 = MIN0(MAX0(IPAD0-2,0),NPADS(IZLAY)-1) ! lowest pad number
      IPAD2 = MIN0(MAX0(IPAD0+2,0),NPADS(IZLAY)-1) ! highest pad number
C
C **** Integral of error function over each pad (FREQ, ERF - CERNLIB).
C
      ZEDGE = IPAD1 * DZ(IZLAY) - HALFZL
      SIGMA = WITOZS(IZLAY) / 1.18
      FREQL = FREQ( (ZEDGE-ZCOORD) / SIGMA )
C
C **** Loop over pads.
C
      DO IPAD = IPAD1, IPAD2
        IF ( NZDATA .GE. NZDTMX ) THEN
          WRITE (LOUT,*) ' **** ZSTRIP: Too many pulses; excess lost'
          GO TO 999
        END IF
        NZDATA = NZDATA + 1            ! increment number of z pulses
        ZEDGE  = ZEDGE + DZ(IZLAY)
        FREQH  = FREQ( (ZEDGE-ZCOORD) / SIGMA )
        PFRAC  = FREQH - FREQL         ! fraction of pulse on this strip.
        PULSE  = PHFAC * PFRAC * PHINT ! pulse height on strip.
        FREQL  = FREQH                 ! prepare for next loop.
C
C **** Determine z-strip address.
C **** Which z-strip is read out?
C
        ISTRIP = MOD( SECTOR * NSTRPS(IZLAY) / NSECS(IZLAY)
     &                - (-1)**IZLAY * IPAD
     &                + NSTRPS(IZLAY),  NSTRPS(IZLAY))
C
C **** Which end is read out?
C
        IF ( IZLAY .EQ. 2 ) THEN           ! layer 2 is split at z=0.
          IF ( (2*IPAD) .GE. NPADS(IZLAY) ) THEN
            IEND = 1                                  ! +z end (south).
          ELSE
            IEND = 0                                  ! -z end (north).
          END IF
        ELSE IF ( IZLAY .EQ. 3 ) THEN
          IEND = MOD( (ISTRIP+185)/96, 2 )
        ELSE IF ( IZLAY .EQ. 4 ) THEN
          IEND = MOD( (ISTRIP+5)/96, 2 )
        ELSE
          IEND = MOD( (ISTRIP+48)/32, 2 )     ! layer 5
        END IF
C
C **** Store address, time, pulse height and pulse width.
C
        ZDAT_ADDR(NZDATA) = 2**12 + IZLAY*2**9 + ISTRIP*2 + IEND
C      DEND=(IABS(IPAD-IEND*NPADS(IZLAY))+.5)*DZ(IZLAY)
C      *ZLHELX(IZLAY)*ZL(IZLAY)
        ZDAT_TIME(NZDATA) = TIME    ! +DEND/VEL
        ZDAT_PHGT(NZDATA) = PULSE
        ZDAT_PWID(NZDATA) = PULWID
        ZDAT_TRAK(NZDATA) = IDTK
C
C **** Store sorting integer for later ordering z-strip hits.
C
        ITIME = 8. * ZDAT_TIME(NZDATA)
        IF ( ITIME .GT. 65535) THEN
          WRITE(LOUT,*) ' **** ZSTRIP: Time out of range'
          ITIME = 65535
        ELSE IF (ITIME .LT. 0) THEN
          WRITE(LOUT,*) ' **** ZSTRIP: Time out of range'
          ITIME = 0
        END IF
        ZDAT_SORT(NZDATA) = ZDAT_ADDR(NZDATA) * 65536 +ITIME ! sorting integer 
                                                     ! (16 bit shift)
      ENDDO
C
  999 RETURN
      END
