      SUBROUTINE ISLBNK(QPART,QCAL,QLEP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Return flags indicating which banks to write
C-
C-   Outputs : 
C-   QPART= value set by call to ISBKST
C-   QCAL =     "
C-   QLEP =     "
C-
C-   ENTRY ISBKST(QPART,QCAL,QLEP)
C-
C-   Created   7-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL QPART,QCAL,QLEP,QPS,QCS,QLS
      SAVE QPS,QCS,QLS
C----------------------------------------------------------------------
C
C
      QPART=QPS
      QCAL=QCS
      QLEP=QLS
      RETURN
C
      ENTRY ISBKST(QPART,QCAL,QLEP)
      QPS=QPART
      QCS=QCAL
      QLS=QLEP
  999 RETURN
C
      END
