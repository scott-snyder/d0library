      FUNCTION ISEDIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-        dummy hook for editing events written to output stream
C-
C-   Created   7-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL ISEDIT
      INCLUDE 'D0$INC:ZEBCOM.INC'
C-------------------------------------------------------
      ISEDIT=.TRUE.
      RETURN
      END

