      SUBROUTINE ZSTRHT(TIME, PHINT, PULWID, ZCOORD, IDTK)
C----------------------------------------------------------------------
C-   Purposes and Methods :
C-  Subroutine ZSTRHT converts a sense wire hit into an induced z strip
C-  hit and saves it for later creation of z strip coordinate "hits"
C-  to simulate the result of cluster finding on the z strips.
C-
C-   Inputs  :
C-    LAYER, SECTOR, WIRE in VTLOCA
C-    TIME       time of pulse on sense wire.
C-    PHINT      pulse height (integrated).
C-    PULWID     pulse width.
C-    ZCOORD     z coordinate of hit on sense wire.
C-    IDTK       GEANT track ID = secondary track #*2**11 + primary track #
C-   Outputs :
C-     Loads COMMON/VZHITS/ with z-strip hits.
C-   Controls: none
C-
C-        T. Trippe, 3 Jan. 1986
C-        P. Grudberg 14-DEC-1988 update for hardware changes
C-  Modified 16-NOV-1989  P.G. - change arguments, routine structure
C-                        use new common VZHITS
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:VZHITS.INC/LIST'
      INCLUDE 'D0$INC:VTLOCA.INC/LIST'
C
      INTEGER IDTK
      REAL TIME, PHINT, PULWID, ZCOORD
      INTEGER INOUT, IZLAY, NZSTRP, IEND, ITIME
      REAL ZINDUC, ZSTRP, PULSE
C----------------------------------------------------------------------
C
      IF ( LAYER .EQ. 0 ) GO TO 999  ! no strips in wire layer 0
C
      IF (NZHITS .GE. NZHTMX) GO TO 999  ! don't compute if no room to store
      NZHITS = NZHITS + 1                ! increment number of z hits.
C
C ****  Inner wires (0-3) can induce signal on inner z layer
C ****  Outer wires (4-7) can induce signal on outer z layer
C
      IF ( WIRE .LE. 3 ) THEN
        IZLAY = 2 * LAYER               ! inner layer
      ELSE
        IZLAY = 2 * LAYER + 1           ! outer layer
      ENDIF
C
C **** Compute pulse height (integrated)
C
      PULSE = ZINDUC(IZLAY,WIRE) * PHINT
C
C **** Compute floating z strip number corresponding to z value
C
      CALL ZSTRNO(ZCOORD,IZLAY,SECTOR,NZSTRP,IEND,ZSTRP)
C
C **** Compute and store time, pulse height, pulse width and address.
C$    Omit timing corrections for now but probably want to correct everything
C$    to the end, or perhaps to z=0.  Save related junk in following comments.
C$     REAL ZLHELX(0:5),VEL,DEND
C$     DATA ZLHELX/94.97, 98.69, 109.42, 109.98, 120.75, 139.35/ ! ~helix length
C$     DATA VEL/20.0/    ! velocity of light in z-strips (cm/ns). Guessing=2/3c.
C$     DEND=(IABS(IPAD-IEND*NPADS(IZLAY))+.5)*DZ*ZLHELX(IZLAY)*ZL(IZLAY)
C$     TIZCOR=DEND/VEL   ! time correction
      ZHIT_STRP(NZHITS) = ZSTRP
      ZHIT_TIME(NZHITS) = TIME     ! +TIZCOR
      ZHIT_PHGT(NZHITS) = PULSE
      ZHIT_PWID(NZHITS) = PULWID
      ZHIT_ADDR(NZHITS) = 2**12 + IZLAY*2**9 + NZSTRP*2 +IEND
      ZHIT_TRAK(NZHITS) = IDTK
C
C **** Store sorting integer for later ordering z-strip hits.
C
      ITIME = 8. * TIME
      IF ( ITIME .GT. 65535 ) THEN
        WRITE(LOUT,*) ' **** ZSTRHT: Time out of range'
        TIME = 65535
      ELSE IF ( ITIME .LT. 0 ) THEN
        WRITE(LOUT,*) ' **** ZSTRHT: Time out of range'
        ITIME = 0
      ENDIF
      ZHIT_SORT(NZHITS) = IZLAY*2**28 + IFIX(ZSTRP*2.**16) + ITIME
      IF(IZLAY.EQ.2) ZHIT_SORT(NZHITS) = ZHIT_SORT(NZHITS) + IEND*2**27
C
  999 RETURN
      END
