      SUBROUTINE ISAWEV
C------------------------------------------------------------------
C-
C- ISAWEV replaces ISAJET ISAWEV
C-
C-  write ZEBRA bank ISAE, ISAJ and ISAQ (main, primary partons,
C-  initial and final partons)
C-
C-  Default is to write ISV1 and ISP1, if QPART false they are dropped
C-  If QLEP true write ISAL banks
C-  If QCAL true write ISAC banks (toy calorimeter)
C-
C-      WRITTEN BY SDP 12/85, Modified Dec. 19,1988
C-
C------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ISAUNT.INC'
      LOGICAL QPART,QCAL,QLEP,ISEDIT
      INTEGER LISV1,GZISV1
C
      CALL ISAEFL             ! fill all ISAJET event banks
C
C  write out event record
      IF(ISEDIT()) THEN
        CALL FZOUT(ISUNIT,IXMAIN,LHEAD,1,' ',1,0,0)
      ENDIF
  999 RETURN
      END
