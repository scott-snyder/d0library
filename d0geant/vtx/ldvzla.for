      SUBROUTINE LDVZLA(ZHIT,LAYERZ)
C-----------------------------------------------------------------------
C-   Purposes and Methods :
C-  Subroutine LDVZLA loads a single hit on the z strips into ZEBRA bank
C-  "VZLA".  The bank "VZLA" is booked if it was not already booked 
C-  earlier.  The number of hits in the z layer bank "VZLA" and in the
C-  vertex chamber bank "VTXH" are incremented.
C-   Inputs  :
C-      IZHIT(1) = 2**12+z_layer*2**9+z_strip*2+end for central strip 
C-      ZHIT(2) = floating z strip number from cluster center
C-      ZHIT(3) = error in floating z strip number
C-      ZHIT(4) = drift time (ns)
C-      ZHIT(5) = drift time error (ns)
C-      ZHIT(6) = pulse area (counts - currently = peak height)
C-      ZHIT(7) = pulse area error (counts)
C-      ZHIT(8) = peak height (counts)
C-      ZHIT(9) = pulse width (ns)
C-      IZHIT(10) = status word (not yet used)
C-      IZHIT(11) = MC track #
C-
C-   Outputs : data to VZLA
C-   Controls: none
C-
C-  T. Trippe, 13 Mar. 1987
C   D.Zieminska May 1988   modifed hit format
C   P. Grudberg May 1989   added dummy argument to BKVZLA (# pulses)
C-  P. Grudberg 16-NOV-1989 clean up
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:VTXLNK.INC/LIST'
C
      INTEGER NWDSHT 
      INTEGER LAYERZ, LKVZLA, LKVTXH, LBASE, IWDHT, MAXHIT
      PARAMETER ( MAXHIT = 1600 )       ! 50 hits / sector * 32 sectors
      REAL ZHIT(*)
C-----------------------------------------------------------------------
C
C **** Get link LKVZLA for z layer
C
      LKVZLA = LVZLA( LAYERZ )
      IF (LKVZLA .EQ. 0) CALL BKVZLA(LAYERZ, MAXHIT, LKVZLA)
      NWDSHT = IQ(LKVZLA+3) 
C
C **** Transfer hit to ZEBRA bank "VZLA"
C
      LBASE = LKVZLA + 3 + NWDSHT*IQ(LKVZLA+1)! pointer to last word filled
      DO IWDHT=1, NWDSHT
        Q(LBASE+IWDHT)=ZHIT(IWDHT)
      ENDDO
C
C ****  Increment # hits in z layer
C
      IQ(LKVZLA+1) = IQ(LKVZLA+1) + 1
C
C ****  Increment # hits in VTX chamber
C
      LKVTXH = LQ(LKVZLA+1)
      IQ(LKVTXH+1) = IQ(LKVTXH+1) + 1 ! total # hits in VTX 
      IQ(LKVTXH+3) = IQ(LKVTXH+3) + 1 ! # strip hits in VTX 
C
  999 RETURN
      END
