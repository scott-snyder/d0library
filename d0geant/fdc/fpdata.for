      SUBROUTINE FPDATA(HITSV,NHITSV)
C-----------------------------------------------------------------------
C   Subroutine FPDATA makes "data" for a given wire in the phi unit in
C   forward drift chamber from information saved by
C   subroutine STPFDC as each track was stepped through the chamber.
C
C   Inputs:
C     HITSV(IWORD,IHIT) contains ten data words for each input hit IHIT.
C            HITSV(1)   X-GLOBAL
C            HITSV(2)   Y-GLOBAL
C            HITSV(3)   Z-GLOBAL
C            HITSV(4)   X-LOCAL      (corresponds to the drift direction)
C            HITSV(5)   PULSE HEIGHT ( Integrated charge)
C            HITSV(6)   Distance to the positive end of the DELAY LINE
C            HISTV(7)   Distance to the negative end of the DELAY LINE
C            HITSV(8)   Track length in the cell
C            HITSV(9)   Track id.=2**11*Secondary track #+Primary track#
C            HITSV(10)  DX/DZ
C     NHITSV is number of hits on wire:  IHIT=1,NHITSV.
C
C   Output to LDFPDA:
C     VHITS(IWORD,IHIT) contains nine data words for each input hit IHIT.
C            VHITS(1)   Hit Id = Logical address (see FCODER.FOR for details)
C            VHITS(2)   Drift Time (ns)
C            VHITS(3)   Pulse Area ()
C            VHITS(4)   Pulse Width (ns)
C            VHITS(5)   PULSE HEIGHT ( Integrated charge)
C            VHITS(6)   Drift_Time_Error (ns)
C            VHITS(7)   Pulse_Height_Error ()
C            VHITS(8)   Status BYTE
C(GEANT only VHITS(9)   GEANT Track id.=2**11*Secondary track #+Primary track#
C     NVHITS is number of hits on wire:  IHIT=1,NVHITS.
C     HALF,SECTOR,WIRE are (forward/backward) half, sector and wire number,
C     respectively.
C     ICALL  = counter of calls to FPDATA in MKFPDA
C
C-   Created   2-JAN-1987   Tom Trippe 
C-   Updated   x-MAR-1987   Daria Zieminska 
C-   Updated   3-OCT-1988   Jeffrey Bantly  modified for new hit format
C-   Updated   4-OCT-1989   Jeffrey Bantly  added T0 to DATA bank
C-   Updated  18-MAR-1990   Jeffrey Bantly  use logical format 
C-   Updated  13-AUG-1992   Robert E. Avery   Get stagger using FSTAGR.
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:FDLOCA.INC'
      INTEGER NWPHV,NHMXV,NWDSHT
      PARAMETER (NWPHV=10)
      PARAMETER (NHMXV=50)
      PARAMETER (NWDSHT=9)
      REAL TIME(NHMXV),HITSV(NWPHV,NHMXV),VHITS(NWDSHT,NHMXV)
      INTEGER IORDER(NHMXV),IVHITS(NWDSHT,NHMXV)
      EQUIVALENCE (VHITS,IVHITS)
      INTEGER IHIT,NHITSV,JHIT,NVHITS,INEXT,IORD
      REAL THRESH,STAG,DFTVEL,PULPH,PULWID,T0
      REAL FSTAGR
      PARAMETER (THRESH=0.)      ! set threshold at zero
      PARAMETER (DFTVEL=0.0033)  ! drift velocity (cm/nsec)
      INTEGER LCFTSE,LCFTMG,GZFTSE
C-------------------------------------------------------------------------
C
      IF(NHITSV.LE.0) RETURN     ! no hits to process
C
C  Get T0 from STP banks
C
      T0 = 0.
      LCFTSE = GZFTSE(HALF,UNIT,QUAD,SECTOR)
      IF ( LCFTSE .LE. 0 ) THEN
        CALL ERRMSG('Bank not defined','FTDATA',
     &                'Time bank FTSE not defined,T0=0. used','I')
      ELSE
        LCFTMG = LCFTSE + 6 + WIRE * IC(LCFTSE+4)
        T0 = C(LCFTMG+1)
      ENDIF
C
C  Add T0 for channel to TIME
C
      DO 5 IHIT=1,NHITSV
        TIME(IHIT)=TIME(IHIT) + T0
    5 CONTINUE
C
C  Sort hit times
C
      DO 10 IHIT=1,NHITSV
        STAG=FSTAGR(HALF,UNIT,QUAD,SECTOR,WIRE)       ! stagger in cm
        TIME(IHIT)=ABS((HITSV(4,IHIT)-STAG)/DFTVEL)
   10 CONTINUE
      CALL SORTZV(TIME,IORDER,NHITSV,1,0,0)
C
C  Loop through in time order creating wire "data"
C
      JHIT=0
      DO 100 INEXT=1,NHITSV
        IORD=IORDER(INEXT)
C
C  If pulse is below threshold, ignore it.
C
        IF(HITSV(5,IORD).LT.THRESH) GO TO 100
C
C  Form "data" in array VHITS.
C
        IF(JHIT.GE.NHMXV) THEN
          CALL ERRMSG('Too many hits','FPDATA',
     &                'Over 50 hits in this sector','I')
          GO TO 101
        ELSE                          ! form "data" output array
          JHIT=JHIT+1
          CALL FCODER(LOGCHA,HALF,UNIT,QUAD,SECTOR,WIRE,UB,2)
          IVHITS(1,JHIT)=LOGCHA               ! logical channel address
          VHITS(2,JHIT)=TIME(IORD)            ! drift time
          PULPH=HITSV(5,IORD)
          VHITS(3,JHIT)=PULPH                 ! pulse area (curr.=peak ht)
          PULWID=(HITSV(8,IORD)/DFTVEL)/1.2   ! 1.2 correction from real data
          VHITS(4,JHIT)=PULWID                ! pulse width
          VHITS(5,JHIT)=PULPH                 ! peak height
          VHITS(6,JHIT)=0.02/DFTVEL           ! drift time error
          VHITS(7,JHIT)=SQRT(PULPH)           ! peak height error
          IVHITS(8,JHIT)=0
          IVHITS(9,JHIT)=0
        ENDIF
C
  100 CONTINUE  ! Bottom of loop over NHITS.
  101 CONTINUE  ! Overflow exit, too many hits.
C
C  Load data into ZEBRA structure
C
      NVHITS=JHIT
      IF(NVHITS.LE.0) RETURN       ! return if no hits to output
      CALL LDFPDA(VHITS,NVHITS)
C
C-----------------------------------------------------------------------------
      RETURN
      END
