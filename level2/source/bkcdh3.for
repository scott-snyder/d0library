      SUBROUTINE BKCDH3(lcdh3)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book CDH3 bank, result bank for level-2
C-   CDC hi-finding.
C-
C-   Inputs  : none.
C-   Outputs : integer lcdh3 = pointer to CDH3 bank
C-   Controls: 
C-
C-   Created  26-APR-1993   Chris Klopfenstein
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      include 'd0$inc:zebcom.inc'
      include 'd0$links:izcdh3.link'
      integer lcdh3
      integer gzl2ch, ll2ch
      INTEGER MPCDH3(5), ISETVN
      LOGICAL FIRST
      character*4 string
      data string / 'CDH3' /
      DATA FIRST / .TRUE. /
      DATA MPCDH3 / 0, 0, 0, 40000, 1 /
C
C======================================================================
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL UCTOH(string, MPCDH3, 4, 4)
      ENDIF
C        
      ll2ch = GZL2CH()
      IF ( ll2ch .le. 0 ) call bkl2ch(ll2ch)
C
C ****  Book CDH3
C
      LCDH3 = LQ(LL2CH - IZCDH3)
      IF ( LCDH3 .EQ. 0 ) THEN
        CALL MZLIFT ( IXMAIN, LCDH3, LL2CH, -IZCDH3, MPCDH3, 3 )
      ENDIF
C
      IQ(LCDH3) = ISETVN(IQ(LCDH3),0)
  999 RETURN
      END
