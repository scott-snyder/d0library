      SUBROUTINE FILL_DHITLIST(lay, sec, maxhits, nhits,
     &                         hitlist)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill an array with hit information
C-                         from DHIT bank, for one layer/sector.
C-
C-   Inputs  : integer lay : layer number
C-             integer sec : sector number
C-             integer maxhits : maximum allowed hits in sector
C-   Outputs : integer nhits : number of hits in this sector
C-             real hitlist : hit information
C-                  hitlist(1, nhit) = wire number
C-                  hitlist(2, nhit) = drift time (ns)
C-                  hitlist(3, nhit) = pulse MIP
C-                  hitlist(4, nhit) = z position (cm) 
C-                  hitlist(5, nhit) = error in z (cm)
C-                  hitlist(6, nhit) = status
C-   Controls: none.
C-
C-   Created  14-NOV-1993   Chris Klopfenstein
C-   Updated   8-APR-1994   Qizhong Li-Demarteau  fix the bug for negative
C-                                           drift time and bad Z position
C-   Updated  11-APR-1994   Srini Rajagopalan  Fix bug for getting "status"
C-                          word from DHIT bank, was 6 bits -> 2 bits.                      
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      include 'd0$inc:zebcom.inc'
      integer lay, sec, maxhits, nhits
      real hitlist(6, maxhits)
      integer hit, wire, status
      real fwire, fstatus
      equivalence (wire, fwire)
      equivalence (status, fstatus)
      integer sec_point, hit_point
      integer ldhit, gzdhit, dhit_len
      real time, zed, errz
      integer MASK16
      data MASK16 / z'FFFF' /
      REAL    MAX_VALUE
      PARAMETER( MAX_VALUE = 32768.0 )
      REAL    MIP
C----------------------------------------------------------------------
      nhits = 0
      ldhit = gzdhit()
      if (ldhit .le. 0) goto 999
      dhit_len = IQ(ldhit + 3)
C
C  get pointer to beginning of this sector in DHIT
C
      call DHITPT(lay, sec, sec_point, nhits)
      nhits = min(nhits, maxhits)
C
      hit_point = sec_point
      do hit = 1, nhits
        wire = IBITS(IQ(ldhit + hit_point + 1), 8, 3)
        hitlist(1, hit) = fwire
        status = IBITS(IQ(ldhit + hit_point + 1), 20, 2)
        hitlist(6, hit) = fstatus
        time = IAND(IQ(ldhit + hit_point + 2), MASK16)
        IF (TIME .GT. MAX_VALUE) THEN
          TIME = TIME - MAX_VALUE * 2
        ENDIF
        hitlist(2, hit) = time / 10.
        if ((wire .eq. 0) .or. (wire .eq. 6)) then  ! outer sense wire
          zed = IQ(ldhit + hit_point + 2) / MASK16
          MIP = IAND(IQ(ldhit + hit_point + 3), MASK16)
          errz = IQ(ldhit + hit_point + 3) / MASK16
        else
          zed = 0.
          errz = 1000.
          MIP = IQ(ldhit + hit_point + 2) / MASK16
        endif
        hitlist(3, hit) = MIP / 100.
        IF (ZED .LT. 9999) THEN
          HITLIST(4, HIT) = ZED / 100.
          HITLIST(5, HIT) = ERRZ / 1000.
        ELSE
          HITLIST(4, HIT) = 0.0
          HITLIST(5, HIT) = 9999.
        ENDIF
        hit_point = hit_point + dhit_len
      enddo
C
  999 RETURN
      END
