      SUBROUTINE ISAZEB(CHIO)
C---------------------------------------------------------------
C-
C-    Initialize Zebra for Isajet
C-    Uses /ZEBCOM/ as the common block for Zebra banks
C-
C-    INPUT:
C-    CHIO = character IO control: 'I' for input, 'O' for output
C-
C-     SDP  Dec.,1985
C-   Updated  22-MAR-2004   sss - compile with g77
C-
C---------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ISAUNT.INC'
      INCLUDE 'D0$INC:ISABNK.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ISALNK.INC'
      CHARACTER*1 CHIO
      LOGICAL FIRST,OK
      DATA FIRST/.TRUE./
C----------------------------------------------------------------
C
C   Do basic initalization
C
      IF(FIRST) THEN
        CALL INZCOM(2)
        FIRST=.FALSE.
C
C        setup the link area for ISAZEB
        CALL MZLINK(IXCOM,'/ISALNK/',LVD,PQREF(MPQREF),LVD)
      ELSE
        IF(CHIO.EQ.'I') CALL FZENDI(ISUNIT,'UT')
        IF(CHIO.EQ.'O') CALL FZENDO(ISUNIT,'UT')
        CLOSE(ISUNIT)
      ENDIF
C
C    Initialize for IO
C
      IF(CHIO.EQ.'O') THEN
        CALL D0OPEN(ISUNIT,FILISA,'OU',OK)
        CALL FZFILE (ISUNIT,0,'O')
      ELSE IF(CHIO.EQ.'I') THEN
        CALL D0OPEN(ISUNIT,FILISA,'IU',OK)
        CALL FZFILE (ISUNIT,0,'I')
      ELSE
        PRINT 101,CHIO
        STOP
      ENDIF
      RETURN
 101  FORMAT(//'  CHIO=',A4,
     &       ' IS NOT A RECOGNIZABLE COMMAND FOR ISAZEB,'
     &       ,/' JOB STOPPED')
      END
