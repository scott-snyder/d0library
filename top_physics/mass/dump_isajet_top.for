      SUBROUTINE DUMP_ISAJET_TOP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dump top related quantities from Isajet
C-   info.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   9-AUG-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INTEGER LISAE,LISAQ,LISAL,LISAJ
      INTEGER GZISAL,GZISAQ,GZISAJ
      EQUIVALENCE (LISAE,CSTLNK(LNKMX)),(LISAQ,CSTLNK(LNKMX-1))
      EQUIVALENCE (LISAL,CSTLNK(LNKMX-2)),(LISAJ,CSTLNK(LNKMX-3))
      INTEGER PRUNIT,SSUNIT
      INTEGER RUN,EVENT
      LOGICAL MONTE_CARLO_DATA
C----------------------------------------------------------------------
      IF ( .NOT.MONTE_CARLO_DATA() ) RETURN
C
      PRUNIT=SSUNIT()
      RUN = IQ(LHEAD+6)
      EVENT=IQ(LHEAD+9)
C
      WRITE(PRUNIT,1)RUN,EVENT
    1 FORMAT(//' ISAJET DUMP FOR RUN/EVENT ',2I)
      CALL PRISAJ(PRUNIT,0,0,'ALL',0)
      CALL PRISAQ(PRUNIT,0,0,'ALL',0)
      CALL PRISAL(PRUNIT,0,0,'ALL',0)
C
  999 RETURN
      END
