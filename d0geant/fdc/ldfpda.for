      SUBROUTINE LDFPDA(VHITS,NVHITS)
C-----------------------------------------------------------------------
C-  Subroutine LDFPDA loads data from one channel into ZEBRA bank 'FPDA'.
C-  Inputs:
C-    VHITS(JWORD,JHIT) contains 9 data words for each output data JHIT.
C-      IVHITS(1,JHIT) = logical channel address
C-      VHITS(2,JHIT) = time (ns)
C-      VHITS(3,JHIT) = pulse area
C-      VHITS(4,JHIT) = pulse width
C-      VHITS(5,JHIT) = peak hight
C-      VHITS(6,JHIT) = time error
C-      VHITS(7,JHIT) = pulse hight error
C-      VHITS(8,JHIT) = status word
C-      VHITS(9,JHIT) = spare
C-    NVHITS is number of output data in channel:  JHIT=1,NVHITS
C     HALF,SECTOR,CHAN are (forward/backward) half, 
C                       sector and channel number respectively.
C
C-   Created   4-JAN-1987   Tom Trippe 
C-   Updated   x-MAR-1987   Daria Zieminska  
C-   Updated   6-OCT-1988   Jeffrey Bantly  modified for new hit format
C-   Updated  18-MAR-1990   Jeffrey Bantly  general cleanup 
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC' 
      INCLUDE 'D0$INC:GCUNIT.INC' 
      INCLUDE 'D0$INC:FDLOCA.INC'
      INTEGER NVHITS,NWDSHT,NPTWHT,NHITS,MXHPCH
      INTEGER IPTR,NHTS,IPTRHT,IHIT,LBASE,IWDHT,I
      INTEGER LKFHLF,LKFTHE,LKFTQD,LKFPDA,GZFPDA
      PARAMETER (NWDSHT=9)
      PARAMETER (NPTWHT=16)
      PARAMETER (MXHPCH=50)
      REAL VHITS(NWDSHT,*)
C-----------------------------------------------------------------------
C
C  Get link LKFPDA for sector
C
      LKFPDA=GZFPDA(HALF,SECTOR)
      IF(LKFPDA.EQ.0) THEN
        NHITS=NPTWHT*MXHPCH
        CALL BKFPDA(HALF,SECTOR,NHITS,LKFPDA)
      ENDIF
      IPTR=4+WIRE
      NHTS=IQ(LKFPDA+IPTR)
      IF(NHTS.NE.0) THEN         ! data already present for this channel
        CALL ERRMSG('Data already there','LDFPDA',
     &              'Data already present for this channel','I')
        RETURN
      ENDIF
      IQ(LKFPDA+IPTR)=NVHITS                ! load number of hits
      IPTRHT=4+NPTWHT*2+NWDSHT*IQ(LKFPDA+1) ! pointer to 1st hit for this chan
      IQ(LKFPDA+IPTR+NPTWHT)=IPTRHT         ! load pointer to first hit
      IQ(LKFPDA+1)=IQ(LKFPDA+1)+NVHITS      ! increment # data in sector
C
C  Transfer data to ZEBRA bank "FPDA".
C
      DO 200 IHIT=1,NVHITS
        LBASE=LKFPDA+IPTRHT-1+NWDSHT*(IHIT-1)
        DO 201 IWDHT=1,NWDSHT
          Q(LBASE+IWDHT)=VHITS(IWDHT,IHIT)
  201   CONTINUE
  200 CONTINUE
C
C-----------------------------------------------------------------------
      RETURN
      END
