      LOGICAL FUNCTION L2_HEXIST(ID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Checks the existence of a histogram.
C-                         True if histogram exists.
C-
C-   Inputs  : ID = Histogram ID number
C-   Outputs : none
C-   Controls: none
C-
C-   Created  22-JAN-1990   Jan Guida, Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBHST.INC'
      INCLUDE 'D0$INC:L2CLBH.INC'
C
      INTEGER ID,NLINK
C----------------------------------------------------------------------
C
C  *** Check for bugs
C
      L2_HEXIST = .FALSE.
      NLINK = IH(LCLBH+3)
      IF (ID.LE.0.OR.ID.GT.NLINK) THEN
        CALL L2_HBERR('L2HEXIST', 'Invalid ID specification ',ID)
        GO TO 999
      ENDIF
C
      IF (LCHST(ID).NE.0) THEN
        L2_HEXIST = .TRUE.
      ENDIF
C
  999 RETURN
      END
