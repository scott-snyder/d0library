      FUNCTION MZ_BANK_SAME_MSG(L1,L2,IN_ZEBCOM,STATUS,MSG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : compare two zebra banks (both in ZEBCOM or ZEBSTP)
C-
C-   Returned value  : TRUE means banks are identical
C-   Inputs  : L1,L2 are the links for the two banks
C-             IN_ZEBCOM = .TRUE. if the two banks are in /ZEBCOM/
C-                 = .FALSE. if the two banks are in /ZEBSTP/  
C-   Outputs : STATUS = 0 banks are the same   
C                       1 neither bank exists 
C                      -1 can't find bank 1
C                      -2 can't find bank 2
C                      -3 banks have different data length
C                      -4 same length, different data contents
C                      -5 same data contents, different numbers of links
C                      -6 same data contents and # links; bank name differs
C              MSG  CHAR*80 ascii text describing status code
C              MZ_BANK_SAME = .TRUE. if banks are the same (status 0 or 1)
C-   Controls: 
C-
C-   Created  5-Dec-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER L1,L2,STATUS,LEN,I
      LOGICAL IN_ZEBCOM,MZ_BANK_SAME,MZ_BANK_SAME_MSG
      INTEGER STATUS_MIN,STATUS_MAX
      PARAMETER( STATUS_MIN = -6 )
      PARAMETER( STATUS_MAX =  1 )
      CHARACTER*80 MESSAG(STATUS_MIN:STATUS_MAX),MSG
      SAVE MESSAG
      DATA MESSAG(0)/' banks are the same'/
      DATA MESSAG(1)/' neither bank exists '/
      DATA MESSAG(-1)/'  can''t find bank 1'/
      DATA MESSAG(-2)/'  can''t find bank 2'/
      DATA MESSAG(-3)/'  banks have different data length'/
      DATA MESSAG(-4)/'  same length, different data contents'/
      DATA MESSAG(-5)/
     &  '  same data contents, different numbers of links'/
      DATA MESSAG(-6)/
     &  '  same data contents and # links; bank name differs'/
C----------------------------------------------------------------------
      MZ_BANK_SAME_MSG = MZ_BANK_SAME(L1,L2,IN_ZEBCOM,STATUS)
      WRITE(MSG,100) STATUS
  100 FORMAT(' Unknown Status Code',I10) 
      IF ((STATUS.GE.STATUS_MIN).AND.(STATUS.LE.STATUS_MAX)) 
     &  MSG = MESSAG(STATUS)
      RETURN
      END
