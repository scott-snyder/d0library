      SUBROUTINE LDVSEC( VHITS, NVHITS )
C-----------------------------------------------------------------------
C-   Purposes and Methods :
C-  Subroutine LDVSEC loads hits from one sense wire into ZEBRA bank "VSEC".
C-  The bank "VSEC" is booked if it was not already booked earlier.  The number
C-  of hits in the sector bank "VSEC", layer bank "VLAY" and vertex chamber
C-  bank "VTXH" are incremented.
C-   Inputs  :
C-    VHITS(JWORD,JHIT) contains 11 data words for each output hit JHIT.
C-      IVHITS(1,JHIT)  = layer*2**9 + sector*2**4 + wire*2 (integer)
C-      VHITS(2,JHIT)   = drift coordinate for phi(hit) > phi(wire) (cm)
C-      VHITS(3,JHIT)   = drift coordinate for phi(hit) < phi(wire) (cm)
C-      VHITS(4,JHIT)   = z from charge division (cm)
C-      VHITS(5,JHIT)   = error in drift distance (cm)
C-      VHITS(6,JHIT)   = error in z from charge division (cm)
C-      VHITS(7,JHIT)   = ionization of the hit (M.I.P)
C-      VHITS(8,JHIT)   = error on ionization
C-      VHITS(9,JHIT)   = drift time (ns)
C-      IVHITS(10,JHIT) = status word
C-      IVHITS(11,JHIT) = MC track #
C-    NVHITS is number of output hits on wire:  JHIT=1,NVHITS
C-    LAYER, SECTOR, WIRE in VTLOCA
C-   Outputs : data to VSEC
C-   Controls: none
C-
C-  T. Trippe, 4 Jan. 1987
C   D.Zieminska May 1988   modified hit format
C-  P. Grudberg May 1989  added dummy argument in call to BKVSEC for # hits
C-  Modified 12-NOV-1989  P. Grudberg - change arguments, add VTLOCA
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:VTXLNK.INC/LIST'
      INCLUDE 'D0$INC:VTLOCA.INC/LIST'
C
      INTEGER NVHITS, NWDSHT, NPTWHT, MAXHIT, NUMWIR, NHMAX
      PARAMETER ( NWDSHT = 11 )
      PARAMETER ( NHMAX = 50 )          ! max hits / wire
      PARAMETER ( NUMWIR = 8 )          ! 8 wires / sector
      PARAMETER ( MAXHIT = NUMWIR * NHMAX )     ! max hits / sector
      INTEGER IPTR, NHTS, IPTRHT, IHIT, LBASE, IWDHT
      INTEGER LKVSEC, LKVLAY, LKVTXH
      REAL VHITS(NWDSHT,*)
C-----------------------------------------------------------------------
C
C **** Get link LKVSEC for sector
C
      LKVSEC = LVSEC( SECTOR, LAYER )
C
C ****  Book the bank if needed
C
      IF (LKVSEC .LE. 0) CALL BKVSEC( LAYER, SECTOR, MAXHIT, LKVSEC)
      NPTWHT = IQ(LKVSEC+2)
      IPTR = 4 + WIRE
C
C ****  Check if data is already present
C
      NHTS = IQ(LKVSEC+IPTR)
      IF (NHTS .NE. 0) THEN
        WRITE (LOUT,*) ' **** LDVSEC: error; attempting overwrite'
        GO TO 999
      ENDIF
C
C ****  Load # of hits and pointer to first hit on wire
C
      IQ(LKVSEC+IPTR) = NVHITS
      IPTRHT = 4 + NPTWHT*2 + NWDSHT*IQ(LKVSEC+1)
      IQ(LKVSEC+IPTR+NPTWHT) = IPTRHT
C
C ****  Increment # hits in sector
C
      IQ(LKVSEC+1) = IQ(LKVSEC+1) + NVHITS
C
C ****  Increment # hits in layer, VTX chamber
C
      LKVLAY = LQ(LKVSEC+1)
      IQ(LKVLAY+1) = IQ(LKVLAY+1) + NVHITS   ! total # hits in layer
      IQ(LKVLAY+2) = IQ(LKVLAY+2) + NVHITS   ! # hits with +z and -z data
      LKVTXH = LQ(LKVLAY+1)
      IQ(LKVTXH+1) = IQ(LKVTXH+1) + NVHITS   ! total # hits in VTX
      IQ(LKVTXH+2) = IQ(LKVTXH+2) + NVHITS   ! # wire hits in VTX
C
C *** Transfer hits to ZEBRA bank VSEC.
C
      DO IHIT=1, NVHITS
        LBASE = LKVSEC + IPTRHT - 1 + NWDSHT*(IHIT-1)
        DO IWDHT = 1, NWDSHT
          Q(LBASE+IWDHT) = VHITS(IWDHT,IHIT)
        ENDDO
      ENDDO
C
  999 RETURN
      END
