      FUNCTION GZL0AD()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank L0AD
C-
C-   Returned value  : GZL0AD
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created  14-JUL-1992   Freedy Nang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:LV0LNK.INC'
      INCLUDE 'D0$LINKS:IZL0AD.LINK/LIST'
      INCLUDE 'D0$PARAMS:ISTAT_DROP.PARAMS'
C
      INTEGER GZL0AD
      INTEGER DUMMY, LKLV0H, GZLV0H
C
      CHARACTER*4 PATH_LINK   !   path for which link has been set
      CHARACTER*4 PATH        !   path for link is wanted
C
      LOGICAL FIRST, LGEANT_CHK
C
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
C
C        IF ( .NOT.LGEANT_CHK() ) THEN
          CALL LV0PLNK
C        ENDIF
        FIRST=.FALSE.
      ENDIF
C
      IF ( LL0AD.EQ.0 ) THEN      ! link not set
        LKLV0H=GZLV0H()
        IF (LKLV0H.NE.0) LL0AD=LQ(LKLV0H-IZL0AD)
        GZL0AD=LL0AD
        CALL PATHGT(PATH)
        PATH_LINK=PATH
      ELSE                        ! link set
        CALL PATHGT(PATH)
        IF (PATH.NE.PATH_LINK.OR.IAND(IQ(LL0AD),ISTAT_DROP).NE.0) THEN
C                                 ! link set for wrong path
C                                 ! or bank has been dropped
          GZL0AD=0
          LKLV0H=GZLV0H()
          IF (LKLV0H.NE.0) LL0AD=LQ(LKLV0H-IZL0AD)
          GZL0AD=LL0AD
          PATH_LINK=PATH
        ELSE
          GZL0AD=LL0AD
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
