      SUBROUTINE D0DBL3_RCLIENT_INIT(NITEM,ITEM,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Called by the client process to activate
C-                         initialization of a database if necessary and/or
C-                         to set logicals and flags.
C-
C-   Inputs  : NITEM        Dimension of ITEM
C-             ITEM(1)      Database name
C-             ITEM(2-10)   Reserved
C-   Outputs : 
C-             IER          0=everything OK, otherwise problem
C-   Controls: 
C-
C-   Created  11-JAN-1994   S. Abachi
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NITEM,ITEM(NITEM),IER
      INCLUDE 'D0$INC:DBSTP.INC'
C
      CALL_DBEND = .TRUE.
      IER = 0
C
C----------------------------------------------------------------------
  999 RETURN
      END
