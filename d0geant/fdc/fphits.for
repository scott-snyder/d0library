      SUBROUTINE FPHITS(HITSV,NHITSV)
C-----------------------------------------------------------------------
C   Subroutine FPHITS makes coordinate "HITS" for a given wire in
C   the phi unit in the forward drift chamber from information saved by
C   subroutine STPFDC as each track was stepped through the chamber.
C   The coordinate "HITS" are time ordered and have pulse height above
C   a fixed (small) threshold. The "HITS" are loaded into ZEBRA bank 'FPHI'.
C
C   Inputs:
C     HITSV(IWORD,IHIT) contains ten data words for each input hit IHIT.
C            HITSV(1)   X-GLOBAL
C            HITSV(2)   Y-GLOBAL
C            HITSV(3)   Z-GLOBAL
C            HITSV(4)   X-LOCAL      (corresponds to the drift direction)
C            HITSV(5)   PULSE HEIGHT ( Integrated charge)
C            HITSV(6)   No delay line in Phi chamber
C            HITSV(7)   No delay line in Phi chamber
C            HITSV(8)   Track length in the cell
C            HITSV(9)   Track id.=2**11*Secondary track #+Primary track#
C            HITSV(10)  DX/DZ
C     NHITSV is number of hits on wire:  IHIT=1,NHITSV.
C     HALF,SECTOR,WIRE are (forward/backward) half, sector and wire number,
C     respectively.
C
C   Sent to LDFPSC:
C     VHITS(IWORD,IHIT) contains twelve data words for each input hit IHIT.
C            VHITS(1)   I.D. = Logical Address (see FCODER.FOR for details)
C            VHITS(2)   Drift distance for the phi+ solution
C            VHITS(3)   Drift distance for the phi- solution
C            VHITS(4)   No delay line in Phi chamber
C            VHITS(5)   Error on the drift distance (cm)
C            VHITS(6)   =9999., No delay line in Phi chamber
C            VHITS(7)   Ionization of the hit in M.I.P. units
C            VHITS(8)   Error in the previous value
C            VHITS(9)   STATUS word
C            VHITS(10)  Pointer on first PULSE hit     ( sense wire )
C            VHITS(11)  Unused
C            VHITS(12)  Unused
C        For GEANT direct HIT banks,
C            VHITS(10)=Track id.= 2**11*Secondary track# + Primary track#
C            and there are no PULSE pointers (so only 10 words/hit)
C     NVHITS is number of hits on wire:  IHIT=1,NVHITS.
C
C-   Created   2-JAN-1987   Tom Trippe 
C-   Updated   x-MAR-1987   Daria Zieminska  
C-   Updated   6-OCT-1988   Jeffrey Bantly  modified for new hit format
C-   Updated  18-MAR-1990   Jeffrey Bantly  use logical format 
C-   Updated  13-AUG-1992   Robert E. Avery   Get stagger using FSTAGR.
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:FDLOCA.INC'
      INTEGER NWPHV,NHMXV,NWDSHT
      PARAMETER (NWPHV=10)
      PARAMETER (NHMXV=50)
      PARAMETER (NWDSHT=12)
      REAL TIME(NHMXV),HITSV(NWPHV,NHMXV),VHITS(NWDSHT,NHMXV)
      INTEGER IORDER(NHMXV),IVHITS(NWDSHT,NHMXV)
      EQUIVALENCE (VHITS,IVHITS)
      INTEGER IHIT,NHITSV,JHIT,NVHITS,INEXT,IORD
      REAL THRESH,STAG,FSTAGR,DFTVEL,PULPH
      PARAMETER (THRESH=0.)      ! set threshold at zero
      PARAMETER (DFTVEL=0.0033)  ! drift velocity (cm/nsec)
C-------------------------------------------------------------------------
C
      IF(NHITSV.LE.0) RETURN     ! no hits to process
C
      DO 10 IHIT=1,NHITSV
        STAG=FSTAGR(HALF,UNIT,QUAD,SECTOR,WIRE)       ! stagger in cm
        TIME(IHIT)=ABS((HITSV(4,IHIT)-STAG)/DFTVEL)   ! drift time
   10 CONTINUE
C
C  Sort hit times
C
      CALL SORTZV(TIME,IORDER,NHITSV,1,0,0)
C
C  Loop through in time order creating wire "hits"
C
      JHIT=0
      DO 100 INEXT=1,NHITSV
        IORD=IORDER(INEXT)
C
C  If pulse is below threshold, ignore it.
C
        IF(HITSV(5,IORD).LT.THRESH) GO TO 100
C
C  Form coordinate "hit" in array VHITS.
C
        IF(JHIT.GE.NHMXV) THEN
          CALL ERRMSG('Too many hits','FPHITS',
     &                'Over 50 hits in this sector','I')
          GO TO 101
        ELSE                          ! form "hit" output array
          JHIT=JHIT+1
          CALL FCODER(LOGCHA,HALF,UNIT,QUAD,SECTOR,WIRE,UB,2)
          IVHITS(1,JHIT)=LOGCHA                 ! packed hit address
          STAG=FSTAGR(HALF,UNIT,QUAD,SECTOR,WIRE)       ! stagger in cm
          VHITS(2,JHIT)=TIME(IORD)*DFTVEL+STAG  ! drift distance (phi+)
          VHITS(3,JHIT)=-TIME(IORD)*DFTVEL+STAG ! drift distance (phi-)
          VHITS(4,JHIT)=0.                     ! no delay line
          VHITS(5,JHIT)=0.020                  ! err. in drift dist.
          VHITS(6,JHIT)=9999.                  ! no delay line
          PULPH=HITSV(5,IORD)
          VHITS(7,JHIT)= PULPH                ! Ionization of hit in M.I.P.
          VHITS(8,JHIT)= SQRT(PULPH)          ! Error on previous value
          VHITS(9,JHIT)= 0.                   ! Status word
          IVHITS(10,JHIT)=INT(HITSV(9,IORD))  ! track ID
          VHITS(11,JHIT)= 0.                  ! Not used
          VHITS(12,JHIT)= 0.                  ! Not used
        ENDIF
C
  100 CONTINUE  ! Bottom of loop over NHITS.
  101 CONTINUE  ! Overflow exit, too many hits.
C
C  Load hits into ZEBRA structure
C
      NVHITS=JHIT
      IF(NVHITS.LE.0) RETURN       ! return if no hits to output
      CALL LDFPSC(VHITS,NVHITS)
C
C-------------------------------------------------------------------------
      RETURN
      END
