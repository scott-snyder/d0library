       SUBROUTINE ISAWBG
C------------------------------------------------------------------
C-
C-     subroutine ISAWBG writes beginning record (1001)
C-
C-     WRITTEN BY SDP 12/85, rev. Sept.86
C-
C------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ISAUNT.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
C
      CALL ISABFL          ! fill ISAB bank
C
C  write out beginning record
      CALL FZOUT(ISUNIT,IXMAIN,LHEAD,1,' ',1,0,0)
      CALL MZWIPE(0)
       RETURN
       END
