      FUNCTION GZLV0H()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to zebra bank LV0H
C-
C-   Returned value  : GZLV0H
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created  13-JUL-1992   Freedy Nang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:LV0LNK.INC'
      INCLUDE 'D0$LINKS:IZLV0H.LINK/LIST'
      INCLUDE 'D0$PARAMS:ISTAT_DROP.PARAMS'
C

      INTEGER GZLV0H
      INTEGER DUMMY, LKHITS, GZHITS
C
      CHARACTER*4 PATH_LINK   !   path for which link has been set
      CHARACTER*4 PATH        !   path for which link is wanted
C
      LOGICAL FIRST, LGEANT_CHK
C
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
C        IF (.NOT.LGEANT_CHK()) THEN
C
C ****  COMMENT OUT THIS CALL FOR NOW
C
          CALL LV0PLNK
C        ENDIF
        FIRST = .FALSE.
      ENDIF
C
      IF (LLV0H.EQ.0) THEN        ! link not set
        LKHITS=GZHITS()
        IF (LKHITS.NE.0) LLV0H=LQ(LKHITS-IZLV0H)
        GZLV0H=LLV0H
        CALL PATHGT(PATH)
        PATH_LINK=PATH
      ELSE                        ! link set
        CALL PATHGT(PATH)
        IF (PATH.NE.PATH_LINK.OR.IAND(IQ(LLV0H),ISTAT_DROP).NE.0) THEN
C                                 ! link set for wrong path
C                                 ! or bank has been dropped
          GZLV0H=0
          LKHITS=GZHITS()
          IF (LKHITS.NE.0) LLV0H=LQ(LKHITS-IZLV0H)
          GZLV0H=LLV0H
          PATH_LINK=PATH
        ELSE
          GZLV0H=LLV0H
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
