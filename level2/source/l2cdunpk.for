      SUBROUTINE L2CDUNPK(end, depth, data, ier)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpack one channel of CD zero-suppressed 
C-                         FADC data.
C-
C-   Inputs  : integer end : pointer to end of data for this
C-                           channel
C-             integer depth : length of output array
C-   Outputs : integer data(depth) : unpacked data
C-             integer ier : 0 if unpack ok.
C-   Controls: none.
C-
C-   Created  10-DEC-1992   Chris Klopfenstein
C-   Modified 19 Sept 93 - eliminate call to VZERO - CK
C-   Modified November 93 - use equivalence between word array and
C-   byte array to unpack, rather than CERNLIB routine. - ck
C----------------------------------------------------------------------
      implicit none
      include 'd0$inc:zebcom.inc'
      include 'd0$params:byte_order.params'
      integer WordData
      byte ByteData(4)
      equivalence (WordData, ByteData(1))
      integer end, pout, depth, ier
      integer data(depth)
      integer channel_length, cluster_length
      integer point, start, cluster_end, bin, ind
C----------------------------------------------------------------------
C  note: here channel length in bytes
      channel_length = IBITS(IQ(end), 0, 16)
      if (channel_length .le. 4) then
        data(1) = 0
        ier = 0
        goto 999
      endif
      start = end - channel_length / 4
      pout = 1
      point = end - 1
C
C  Unpack each cluster separately. Format of unpacked data is
C  data(pout) = cluster_length - 4 (length includes itself)
C  data(pout + 1) = address of first bin
C  data(pout + 2 ...pout + cluster_length) = FADC data
C
      do while (point .gt. start)
        cluster_length = IBITS(IQ(point), 0, 16)
        cluster_end = IBITS(IQ(point), 16, 16)
        if ((cluster_length .gt. channel_length) .or.
     &      (cluster_end .gt. depth)) then
          call ErrMsg('L2CDHT', 'L2CDUNPK',
     &      'Error unpacking FADC cluster', 'W')
          ier = 1
          goto 999
        endif
        point = point - cluster_length / 4
        data(pout) = cluster_length - 4
        data(pout + 1) = cluster_end - (cluster_length - 4) + 1
C
C  unpack one cluster, using KERNLIB routine
C
C        call BLOW(IQ(point+1), data(pout + 2),
C     &            cluster_length - 4, 8)
C  use equivalence between byte and word data to 
C  unpack the bytes
        do ind = 0, (cluster_length/4 - 2)
          WordData = IQ(point + ind + 1)
          data(pout + 1 + 4*ind + 1) = ByteData(byte4)
          data(pout + 1 + 4*ind + 2) = ByteData(byte3)
          data(pout + 1 + 4*ind + 3) = ByteData(byte2)
          data(pout + 1 + 4*ind + 4) = ByteData(byte1)
        enddo
        pout = pout + cluster_length - 2
C
      enddo
C  need to mark end of data
      data(pout) = 0
C
      ier = 0
  999 RETURN
      END
