      INTEGER FUNCTION GZCDH2()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find pointer to CDH2 bank (result bank for
C-   level-2 CDC hit-finding).
C-
C-   Returned value  : integer GZCDH2 = pointer to bank
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  26-APR-1993   Chris Klopfenstein
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      include 'd0$inc:zebcom.inc'
      include 'd0$links:izcdh2.link'
      integer gzl2ch, ll2ch
C----------------------------------------------------------------------
      gzcdh2 = 0
      ll2ch = gzl2ch()
      if (ll2ch .gt. 0) gzcdh2 = LQ(ll2ch - izcdh2)
  999 RETURN
      END
