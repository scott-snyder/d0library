      SUBROUTINE ZABEND
C----------------------------------------------------------------
C-
C-      do not allow for graceful exit from a Zebra fatal error
C-
C------------------------------------------------------------------
      PRINT *,' ZABEND CALLED IS IN ISAZEB.OLB'
      CALL ABORT
      END
