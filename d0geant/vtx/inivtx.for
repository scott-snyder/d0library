      LOGICAL FUNCTION INIVTX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Various initializations for VTX.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: D0LOG.INC
C-
C-   Created  dd-mmm-198y   ????????????
C-   Updated  24-MAY-1988   Ghita Rahal-Callot
C    Modified  6-OCT-1988   Tom Trippe
C    Updated  20-JUN-1989   Peter Grudberg - add pulse shape parameters,
C                           zero suppression include file changed
C    Updated   7-JUL-1989   P. G. - SCALE(DTTYPE) added to ZPULPR.INC
C-   Updated  17-JUL-1989   Harrison B. Prosper
C-   Made into pbd interface function
C-   Updated  17-NOV-1989   P. G. - SCALE : 3 -> 6 to increase pulse size
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:ZPULPR.INC/LIST'
      INCLUDE 'D0$INC:ZZEROP.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C
      INTEGER  IERR
C----------------------------------------------------------------------
      INIVTX = .TRUE.
      IF ( DVTX .LE. 0 ) GOTO 999
C
C ****  Initialize the geometry of the VTX in the store /ZEBSTP/
C
      IERR=0
      CALL VTISTP ( 'VTX_STPFILE', IERR )
      IF (IERR .NE. 0 ) THEN
        WRITE(LOUT,*)' ****INIVTX: Error initializing geometry'
        CALL EXIT(1)
      ENDIF
C
C Parameters for pulse shape simulation:
C                 (x=0: VTX; x=1: CDC; x=2: FDC)
C
C PARTR and PARTW are fitted parameters from real data
C
C   Rise time = PARTR(1,x) * SQRT(Tdrift) + PARTR(2,x)
C               (for uniform drift region: Tdrift > TRLIMT)
C             = PARTR(3,x) * Tdrift ** 2 + PARTR(4,x) * Tdrift + PARTR(5,x)
C               (for the non-uniform drift region: 0 < Tdrift < TRLIMT)
C
C Total width = PARTW(1,x) * SQRT(Tdrift) + PARTW(2,x)
C               (for uniform drift region: Tdrift > TWLIMT)
C             = PARTW(3,x) * Tdrift ** 2 + PARTW(4,x) * Tdrift + PARTW(5,x)
C               (for the non-uniform drift region: 0 < Tdrift < TWLIMT)
C
C For now, all numbers except PULWEI and DLSCAL taken from CDC
C (For the VTX, delay line parameters apply to the z-strips)
C
      PARTR(1,0) = 0.476
      PARTR(2,0) = 31.9
      PARTR(3,0) = -0.00097
      PARTR(4,0) = 0.45
      PARTR(5,0) = -7.39
C
      PARTW(1,0) = 0.029
      PARTW(2,0) = 11.3
      PARTW(3,0) = -0.000097
      PARTW(4,0) = 0.083
      PARTW(5,0) = -2.2
C
      TRLIMT(0) = 300.0                 ! ns
      TWLIMT(0) = 220.0                 ! ns
      MINRS(0) = 20.0                   ! minimum rise time (ns)
      MINWID(0) = 10.0                  ! minimun width (FADC channels)
      PULWEI(0) = 0.5                   ! weight for center of gravity
      DLSCAL(0) = 1.2                   ! Delay line / Sense wire
      ! (width)
      SCALE(0)  = 6.0                   ! overall scale factor for fadc data
C
C ****
C ****  Fill the common ZZEROP
C ****  contains the thresholds for the Zero-suppression in the FADC for
C ****  the 3 chambers VTX, CDC, FDC.  DIFTHn(x) and BINTHn(x)
C                    (x=0: VTX; x=1: CDC; x=2: FDC)
C **** If SVTX(5) equals 0., do zero suppression, else set thresholds
C **** to zero to turn off suppression.
C
      IF ( SVTX(5) .EQ. 0 ) THEN
        DIFTH1(0) = 5
        DIFTH2(0) = 5
        DIFTH3(0) = 5
        BINTH1(0) = 27
        BINTH2(0) = 27
        BINTH3(0) = 27
        BINTH4(0) = 27
      ELSE
        DIFTH1(0) = 0
        DIFTH2(0) = 0
        DIFTH3(0) = 0
        BINTH1(0) = 0
        BINTH2(0) = 0
        BINTH3(0) = 0
        BINTH4(0) = 0
      ENDIF
C
C
C
  999 CONTINUE
      RETURN
      END
