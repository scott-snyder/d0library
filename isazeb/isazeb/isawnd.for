      SUBROUTINE ISAWND
C-----------------------------------------------------------------
C-
C-  Replaces ISAJET ISAWND. Write Zebra end record (1002).
C-
C-    SDP Jan.,1986
C-
C-----------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ISAUNT.INC'
C----------------------------------------------------------------------
C
      CALL ISAFFL     ! fill ISAF bank
C
C  write out end record
      CALL FZOUT(ISUNIT,IXMAIN,LHEAD,1,' ',1,0,0)
      CALL MZWIPE(0)
      RETURN
      END
