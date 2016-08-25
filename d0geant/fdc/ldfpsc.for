      SUBROUTINE LDFPSC(VHITS,NVHITS)
C-----------------------------------------------------------------------
C-  Subroutine LDFPSC loads hits from one sense wire into ZEBRA bank 'FPSC'.
C-  The bank 'FPSC' is booked if it was not already booked earlier.  The number
C-  of hits in the sector bank 'FPSC', phi unit bank 'FPHI', forward/backward
C-  half bank 'FHLF' and forward detector bank 'FDCH' are incremented.
C-  Inputs:
C-    VHITS(JWORD,JHIT) contains 18 data words for each output hit JHIT.
C       IVHITS(1,JHIT) =  I.D. = Logical Address (see FCODER.FOR for details)
C-      VHITS(2,JHIT)  = drift distance of phi+ soln, (cm) assuming ylocal > 0
C-      VHITS(3,JHIT)  = drift distance of phi- soln, (cm) assuming ylocal < 0
C-      VHITS(4,JHIT)  = unused
C-      VHITS(5,JHIT)  = error in drift distance (cm)
C-      VHITS(6,JHIT)  = 9999., no delay line
C-      VHITS(7,JHIT)  = Ionization of the hit in M.I.P. units
C-      VHITS(8,JHIT)  = Error in the previous value
C-      VHITS(9,JHIT)  = STATUS word
C-      VHITS(10,JHIT) = Pointer on first PULSE hit  ( sense wire )
C-      VHITS(11,JHIT) = Unused
C-      VHITS(12,JHIT) = Unused
C-     For GEANT direct HIT banks,
C-      VHITS(10)=Track id.= 2**11*Secondary track# + Primary track#
C-      and there are no PULSE pointers (so only 10 words/hit)
C-    NVHITS is number of output hits on wire:  JHIT=1,NVHITS
C-    HALF,SECTOR,WIRE are (forward/backward) half, sector and wire number,
C-    respectively.
C
C-   Created   4-JAN-1987   Tom Trippe 
C-   Updated   x-MAR-1987   Daria Zieminska  
C-   Updated   6-OCT-1988   Jeffrey Bantly   modified for new hit banks
C-   Updated  18-MAR-1990   Jeffrey Bantly   general cleanup
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC' 
      INCLUDE 'D0$INC:GCUNIT.INC' 
      INCLUDE 'D0$INC:FDLOCA.INC'
      INTEGER NVHITS,NWDSHT,NPTWHT,MXHPCH
      INTEGER IPTR,NHTS,IPTRHT,IHIT,LBASE,IWDHT,I,NHITS
      INTEGER LKFDCH,LKFHLF,LKFPHI,LKFTQD,LKFPSC,GZFPSC
      PARAMETER (NWDSHT=12)
      PARAMETER (NPTWHT=16)
      PARAMETER (MXHPCH=50)
      REAL VHITS(NWDSHT,*)
C------------------------------------------------------------------------
C
C  Get link LKFPSC for sector
C
      LKFPSC=GZFPSC(HALF,SECTOR)
      IF(LKFPSC.EQ.0) THEN
        NHITS=NPTWHT*MXHPCH
        CALL BKFPSC(HALF,SECTOR,NHITS,LKFPSC)
      ENDIF
      IPTR=4+WIRE
      NHTS=IQ(LKFPSC+IPTR)
      IF(NHTS.NE.0) THEN         ! data already present for this wire
        CALL ERRMSG('Bank overwrite','LDFPSC',
     &              'Data already present for current wire','I')
        GOTO 999 
      ENDIF
      IQ(LKFPSC+IPTR)=NVHITS                  ! load number of hits
      IPTRHT=4+NPTWHT*2+NWDSHT*IQ(LKFPSC+1)   ! pointer to 1st hit for this wire
      IQ(LKFPSC+IPTR+NPTWHT)=IPTRHT           ! load pointer to first hit
      IQ(LKFPSC+1)=IQ(LKFPSC+1)+NVHITS        ! increment # hits in sector
      LKFPHI=LQ(LKFPSC+1)
      IQ(LKFPHI+1)=IQ(LKFPHI+1)+NVHITS        ! increment # hits in unit
      LKFHLF=LQ(LKFPHI+1)
      IQ(LKFHLF+1)=IQ(LKFHLF+1)+NVHITS        ! increment # hits in half
      LKFDCH=LQ(LKFHLF+1)
      IQ(LKFDCH+1)=IQ(LKFDCH+1)+NVHITS        ! increment # hits in forward ch.
C
C-------------------- TEST PRINTOUT ---------------------------------------
cD     WRITE (LOUT,20) HALF,SECTOR,WIRE,
cD    &       IQ(LKFHLF+1),IQ(LKFPHI+1),IQ(LKFPSC+1),NVHITS
   20 FORMAT(' LDFPSC: HALF/UNIT/SECT/WIRE ',3I3,5X,4I3)
C--------------------------------------------------------------------------
C  Transfer hits to ZEBRA bank "FPSC".
C
      DO 200 IHIT=1,NVHITS
        LBASE=LKFPSC+IPTRHT-1+NWDSHT*(IHIT-1)
        DO 201 IWDHT=1,NWDSHT
          Q(LBASE+IWDHT)=VHITS(IWDHT,IHIT)
  201   CONTINUE
  200 CONTINUE
C
C---------------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
