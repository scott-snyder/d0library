      SUBROUTINE FTDLDA(HDLAY,NDELAY,END)
C-----------------------------------------------------------------------
C   Subroutine FTDLDA makes delay line "data" for a given delay line end
C   in a given sector in a theta unit in the forward drift chamber from
C   information saved by subroutine FTWRDA
C   The "data" are loaded into ZEBRA bank 'FTDA'.
C
C   Inputs: HDLAY
C     HDLAY(IWORD,IHIT) contains three data words for each input hit IHIT.
C       HDLAY(1,IHIT) = drift time (ns)
C       HDLAY(2,IHIT) = pulse height (counts)
C       HDLAY(3,IHIT) = pulse width (ns)
C     NDELAY is number of hits on wire:  IHIT=1,NDELAY.
C     HALF,UNIT,QUAD,SECTOR,WIRE
C
C-   Created   2-JAN-1987   Tom Trippe 
C-   Updated   x-MAR-1987   Daria Zieminska  
C-   Updated  18-MAR-1990   Jeffrey Bantly   use logical format
C-
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:FDLOCA.INC'
      INTEGER NWPHV,NHMXV,NWDSHT
      PARAMETER (NWPHV=3)
      PARAMETER (NHMXV=50)
      PARAMETER (NWDSHT=9)
      REAL TIME(NHMXV),HDLAY(NWPHV,NHMXV),HITOUT(NWPHV,NHMXV),
     +     VHITS(NWDSHT,NHMXV),PULPH
      INTEGER IORDER(NHMXV),IVHITS(NWDSHT,NHMXV)
      EQUIVALENCE (VHITS,IVHITS)
      INTEGER IHIT,NDELAY,NHOUT,JHIT,NVHITS,INEXT,IORD
      INTEGER END,CHAN,NWIRE
      REAL THRESH,DELVEL
      PARAMETER (THRESH=0.)      ! set threshold at 0
      PARAMETER (DELVEL=0.22)           ! delay line velocity cm/nsec
      DATA NWIRE/8/
C-------------------------------------------------------------------------
C
      IF(NDELAY.LE.0) RETURN     ! no hits to process
C
C  Sort hit times
C
      DO 10 IHIT=1,NDELAY
   10 TIME(IHIT)=HDLAY(1,IHIT)
      CALL SORTZV(TIME,IORDER,NDELAY,1,0,0)
      CALL DELDIG(HDLAY,IORDER,NDELAY,HITOUT,NHOUT)
C
C  Loop through in time order creating delay line "data"
C
      JHIT=0
      WIRE=NWIRE+END
      CALL FCODER(LOGCHA,HALF,UNIT,QUAD,SECTOR,WIRE,0,2)
      DO 100 JHIT=1,NHOUT
        IVHITS(1,JHIT)=LOGCHA            ! logical channel address
        VHITS(2,JHIT)=HITOUT(1,JHIT)     ! drift time
        PULPH=HITOUT(2,JHIT)
        VHITS(3,JHIT)=PULPH              ! pulse area (curr.=peak ht)
        VHITS(4,JHIT)=HITOUT(3,JHIT)     ! pulse width
        VHITS(5,JHIT)=PULPH              ! peak height
        VHITS(6,JHIT)=0.02/DELVEL        ! drift time error
        VHITS(7,JHIT)=SQRT(PULPH)        ! peak height error
        IVHITS(8,JHIT)=0
        IVHITS(9,JHIT)=0
        NVHITS=JHIT
C
  100 CONTINUE  ! Bottom of loop over NHOUT.
  101 CONTINUE  ! Overflow exit, too many hits.
C
C  Load hits into ZEBRA structure
C
      IF(NVHITS.LE.0) RETURN       ! return if no hits to output
      CALL LDFTDA(VHITS,NVHITS)
C
C-------------------------------------------------------------------------
      RETURN
      END
