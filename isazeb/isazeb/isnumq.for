      SUBROUTINE ISNUMQ(NJT,NQS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Find number of primary (ISAJ) and secondary (ISAQ) parton banks
C-   Outputs : 
C-      NJT= number of ISAJ banks
C-      NQS= number of ISAQ banks
C-
C-   Created  13-DEC-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NJT,NQS
      INTEGER LISAJ,LISAQ,GZISAJ,GZISAQ
C----------------------------------------------------------------------
      NJT=0
      NQS=0
C
C  find number of primary parton banks
      LISAJ=GZISAJ()
  11  IF(LISAJ.NE.0) THEN
        NJT=NJT+1
        LISAJ=LQ(LISAJ)
        GOTO 11
      ENDIF
C  find number of stable parton banks
      LISAQ=GZISAQ()
  12  IF(LISAQ.NE.0) THEN
        NQS=NQS+1
        LISAQ=LQ(LISAQ)
        GOTO 12
      ENDIF
  999 RETURN
      END
