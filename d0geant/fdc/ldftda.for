      SUBROUTINE LDFTDA(VHITS,NVHITS)
C-----------------------------------------------------------------------
C-  Subroutine LDFTDA loads data from one channel into ZEBRA bank 'FTDA'.
C-  Inputs:
C-    VHITS(JWORD,JHIT) contains 9 data words for each output data JHIT.
C-      IVHITS(1,JHIT) = Logical address (see FCODER.FOR for details)
C-      VHITS(2,JHIT) = drift time (ns)
C-      VHITS(3,JHIT) = pulse area
C-      VHITS(4,JHIT) = pulse width (ns)
C-      VHITS(5,JHIT) = peak hight
C-      VHITS(6,JHIT) = time error (ns)
C-      VHITS(7,JHIT) = pulse height error
C-      VHITS(8,JHIT) = status word
C-      VHITS(9,JHIT) = track id in GEANT
C-    NVHITS is number of output data in channel:  JHIT=1,NVHITS
C     HALF,UNIT,QUAD,SECTOR,CHAN are (forward/backward) half, theta unit,
C                       quadrant,sector and channel number respectively.
C
C-   Created   4-JAN-1987   Tom Trippe 
C-   Updated   x-MAR-1987   Daria Zieminska  
C-   Updated   3-OCT-1988   Jeffrey Bantly  modified for new hit format
C-   Updated  18-MAR-1990   Jeffrey Bantly  use logical format 
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC' 
      INCLUDE 'D0$INC:GCUNIT.INC' 
      INCLUDE 'D0$INC:FDLOCA.INC'
      INTEGER NVHITS,NWDSHT,NPTWHT,MXHPCH
      INTEGER IPTR,NHTS,IPTRHT,IHIT,LBASE,IWDHT,I,NHITS
      INTEGER LKFHLF,LKFTHE,LKFTQD,LKFTDA,GZFTDA
      PARAMETER (NWDSHT=9)
      PARAMETER (NPTWHT=10)
      PARAMETER (MXHPCH=50)
      REAL VHITS(NWDSHT,*)
C-----------------------------------------------------------------------
C
C  Get link LKFTDA for sector
C
      LKFTDA=GZFTDA(HALF,QUAD,SECTOR)
      IF(LKFTDA.EQ.0) THEN
        NHITS=NPTWHT*MXHPCH
        CALL BKFTDA(HALF,QUAD,SECTOR,NHITS,LKFTDA)
      ENDIF
      IPTR=4+WIRE
      NHTS=IQ(LKFTDA+IPTR)
      IF(NHTS.NE.0) THEN         ! data already present for this channel
        CALL ERRMSG('Data already present','LDFTDA',
     &              'Data already present for this channel','I')
        RETURN
      ENDIF
      IQ(LKFTDA+IPTR)=NVHITS                  ! load number of hits
      IPTRHT=4+NPTWHT*2+NWDSHT*IQ(LKFTDA+1)   ! pointer to 1st hit for this chan
      IQ(LKFTDA+IPTR+NPTWHT)=IPTRHT           ! load pointer to first hit
      IQ(LKFTDA+1)=IQ(LKFTDA+1)+NVHITS        ! increment # data in sector
C
C  Transfer data to ZEBRA bank "FTDA".
C
      DO 200 IHIT=1,NVHITS
        LBASE=LKFTDA+IPTRHT-1+NWDSHT*(IHIT-1)
        DO 201 IWDHT=1,NWDSHT
          Q(LBASE+IWDHT)=VHITS(IWDHT,IHIT)
  201   CONTINUE
  200 CONTINUE
C
C------------------------------------------------------------------------
      RETURN
      END
