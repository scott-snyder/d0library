      SUBROUTINE BKCDH2(lcdh2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book CDH2 bank, result bank for level-2
C-   CDC hi-finding.
C-
C-   Inputs  : none.
C-   Outputs : integer lcdh2 = pointer to CDH2 bank
C-   Controls: 
C-
C-   Created  26-APR-1993   Chris Klopfenstein
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      include 'd0$inc:zebcom.inc'
      include 'd0$links:izcdh2.link'
      integer lcdh2
      integer gzl2ch, ll2ch
      INTEGER MPCDH2(5), ISETVN
      LOGICAL FIRST
      character*4 string
      data string / 'CDH2' /
      DATA FIRST / .TRUE. /
      DATA MPCDH2 / 0, 0, 0, 40000, 1 /
C
C======================================================================
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL UCTOH(string, MPCDH2, 4, 4)
      ENDIF
C        
      ll2ch = GZL2CH()
      IF ( ll2ch .le. 0 ) call bkl2ch()
C
C ****  Book CDH2
C
      LCDH2 = LQ(LL2CH - IZCDH2)
      IF ( LCDH2 .EQ. 0 ) THEN
        CALL MZLIFT ( IXMAIN, LCDH2, LL2CH, -IZCDH2, MPCDH2, 3 )
      ENDIF
C
      IQ(LCDH2) = ISETVN(IQ(LCDH2),0)
  999 RETURN
      END
