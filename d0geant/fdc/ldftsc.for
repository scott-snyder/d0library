      SUBROUTINE LDFTSC(VHITS,NVHITS)
C-----------------------------------------------------------------------
C-  Subroutine LDFTSC loads hits from one sense wire into ZEBRA bank 'FTSC'.
C-  The bank 'FTSC' is booked if it was not already booked earlier.  The number
C-  of hits in the sector bank 'FTSC', quadrant bank 'FTQD', theta unit bank
C-  'FTHE', forward/backward half bank 'FHLF' and forward detector bank 'FDCH'
C-  are incremented.
C-  Inputs:
C-    VHITS(JWORD,JHIT) contains 18 data words for each output hit JHIT.
C       IVHITS(1,JHIT) =  I.D. = Logical address (see FCODER.FOR for details)
C       VHITS(2,JHIT)  = X/YLOC(cm) [assumed X/YLOC>0] Sector<3: Drift Distance
C       VHITS(3,JHIT)  = X/YLOC(cm) [assumed X/YLOC>0] Sector<3: 0.
C       VHITS(4,JHIT)  = Y/X position from the delay line hit (cm)
C       VHITS(5,JHIT)  = Error on the drift distance (cm)
C       VHITS(6,JHIT)  = Error on Y/X position, =9999. if no delay line hit
C       VHITS(7,JHIT)  = Ionization of the hit in M.I.P. units
C       VHITS(8,JHIT)  = Error in the previous value
C       VHITS(9,JHIT)  = STATUS word
C       VHITS(10,JHIT) = Pointer on first PULSE hit  ( sense wire )
C       VHITS(11,JHIT) = Pointer on second PULSE hit ( delay line, -Y/X end )
C       VHITS(12,JHIT) = Pointer on third PULSE hit  ( delay line, +Y/X end )
C    For GEANT direct HIT banks,
C       VHITS(10)=Track id.= 2**11*Secondary track# + Primary track#
C       and there are no PULSE pointers (so only 10 words/hit)
C     NVHITS is number of output hits on wire:  JHIT=1,NVHITS
C     HALF,UNIT,QUAD,SECTOR,WIRE are (forward/backward) half, theta unit,
C                              quadrant,sector and wire number respectively.
C
C-   Created   4-JAN-1987   Tom Trippe 
C-   Updated   x-MAR-1987   Daria Zieminska  
C-   Updated   6-OCT-1988   Jeffrey Bantly   modified to new hit format
C-   Updated  18-MAR-1990   Jeffrey Bantly  use logical format 
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC' 
      INCLUDE 'D0$INC:GCUNIT.INC' 
      INCLUDE 'D0$INC:FDLOCA.INC'
      INTEGER NVHITS,NWDSHT,NPTWHT,MXHPCH
      INTEGER IPTR,NHTS,IPTRHT,IHIT,LBASE,IWDHT,I,NHITS
      INTEGER LKFDCH,LKFHLF,LKFTHE,LKFTQD,LKFTSC,GZFTSC
      PARAMETER (NWDSHT=12)
      PARAMETER (NPTWHT=8)
      PARAMETER (MXHPCH=50)
      REAL VHITS(NWDSHT,*)
C------------------------------------------------------------------------
C
C  Get link LKFTSC for sector
C
      LKFTSC=GZFTSC(HALF,QUAD,SECTOR)
      IF(LKFTSC.EQ.0) THEN
        NHITS=NPTWHT*MXHPCH
        CALL BKFTSC(HALF,QUAD,SECTOR,NHITS,LKFTSC)
      ENDIF
      IPTR=4+WIRE
      NHTS=IQ(LKFTSC+IPTR)
      IF(NHTS.NE.0) THEN         ! data already present for this wire
        CALL ERRMSG('Data already present','LDFTSC',
     &              'Data already present for this wire','I')
        RETURN
      ENDIF
      IQ(LKFTSC+IPTR)=NVHITS                  ! load number of hits
      IPTRHT=4+NPTWHT*2+NWDSHT*IQ(LKFTSC+1)   ! pointer to 1st hit for this wire
      IQ(LKFTSC+IPTR+NPTWHT)=IPTRHT           ! load pointer to first hit
      IQ(LKFTSC+1)=IQ(LKFTSC+1)+NVHITS        ! increment # hits in sector
      LKFTQD=LQ(LKFTSC+1)
      IQ(LKFTQD+1)=IQ(LKFTQD+1)+NVHITS        ! increment # hits in quadrant
      LKFTHE=LQ(LKFTQD+1)
      IQ(LKFTHE+1)=IQ(LKFTHE+1)+NVHITS        ! increment # hits in unit
      LKFHLF=LQ(LKFTHE+1)
      IQ(LKFHLF+1)=IQ(LKFHLF+1)+NVHITS        ! increment # hits in half
      LKFDCH=LQ(LKFHLF+1)
      IQ(LKFDCH+1)=IQ(LKFDCH+1)+NVHITS        ! increment # hits in forward ch.
C
C------------------------ TEST PRINTOUT -------------------------------
D         WRITE(LOUT,20) HALF,UNIT,QUAD,SECTOR,WIRE,
D    &    IQ(LKFHLF+1),IQ(LKFTHE+1),IQ(LKFTQD+1),IQ(LKFTSC+1),NVHITS
   20 FORMAT(' LDFTSC: HALF/UNIT/QUAD/SECT/WIRE ',5I3,5X,5I3)
C----------------------------------------------------------------------
C
C  Transfer hits to ZEBRA bank "FTSC".
C
      DO 200 IHIT=1,NVHITS
        LBASE=LKFTSC+IPTRHT-1+NWDSHT*(IHIT-1)
        DO 201 IWDHT=1,NWDSHT
          Q(LBASE+IWDHT)=VHITS(IWDHT,IHIT)
  201   CONTINUE
  200 CONTINUE
C
C---------------------------------------------------------------------
      RETURN
      END
