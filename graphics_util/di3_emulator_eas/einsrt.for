      SUBROUTINE EINSRT(NINTEG,ILIST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To insert (include) a previously removed segment
C-                         in the display.
C-
C-   Inputs  : 
C-              NINTEG  :  Number of segments
C-              ILIST(NINTEG) : List of segment names
C-
C-
C-   Created  22-NOV-1989   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NINTEG, ILIST(1024)
      INCLUDE 'D0$INC:NEWDI3.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      EXTERNAL ERRHND
      INTEGER I,NAMES
      CHARACTER*4 TSEG
C
      DO I=1,NINTEG
        NAMES = ILIST(I)
        CALL KBLDN(NAMES, TSEG)
        TSEG = 'D'//TSEG
        CALL PINCL(TSEG//'"', EMDISP, ERRHND)
        CALL PPURGE(ERRHND)
      ENDDO
C
  999 RETURN
      END
