      SUBROUTINE BKBERD(LBERD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank BERD
C-
C-   Inputs  : LHEAD = Link of parent bank.
C-                      = 0, will find it for you.
C-   Outputs : Link of Booked BERD Bank
C-   Controls: None
C-
C-   Created   2-JUL-1990 17:13:19.16  Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LBERD
      INTEGER IXIO
      INTEGER GZHEAD
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZBERD.LINK/LIST'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LBERD = 0
      IF(FIRST)THEN
C
        CALL MZFORM('BERD','-I',IXIO)        ! Describe Bank format
C
      ENDIF
C
      CALL MZBOOK
     &  (IXMAIN,LBERD,LHEAD,-IZBERD,'BERD',20,20,2,IXIO,0)
C
  999 RETURN
      END
