      FUNCTION GZPLV0()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to zebra bank PLV0
C-
C-   Returned value  : GZPLV0
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  13-JUL-1992   Freedy Nang
C-   Updated   8-DEC-1995   Tracy L. Taylor  look for MDST bank if QCD MDST 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:LV0LNK.INC'
      INCLUDE 'D0$LINKS:IZPLV0.LINK/LIST'
      INCLUDE 'D0$PARAMS:ISTAT_DROP.PARAMS'
C
      INTEGER GZPLV0
      INTEGER LKPROC, GZPROC
      INTEGER GZMDST,LMDST
C
      CHARACTER*4 PATH_LINK   !   path for which link has been set
      CHARACTER*4 PATH        !   path for which link is wanted
C
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
      SAVE PATH_LINK
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
          CALL LV0PLNK
        FIRST = .FALSE.
      ENDIF
C
      CALL PATHGT(PATH)
      IF ( PATH.EQ.'MDST' ) THEN
        LMDST = GZMDST()
        IF ( LMDST .GT. 0 ) GZPLV0 = LQ(LMDST-2)
        IF ( GZPLV0.EQ.0 ) CALL ERRMSG('GZPLV0','GZPLV0',
     &                     'PLV0 BANK NOT PRESENT','W')
        GOTO 999
      ENDIF
      IF (LPLV0.EQ.0) THEN        ! link not set
        LKPROC=GZPROC()
        IF (LKPROC.NE.0) LPLV0=LQ(LKPROC-IZPLV0)
        GZPLV0=LPLV0
        CALL PATHGT(PATH)
        PATH_LINK=PATH
      ELSE                        ! link set
        CALL PATHGT(PATH)
        IF (PATH.NE.PATH_LINK.OR.IAND(IQ(LPLV0),ISTAT_DROP).NE.0) THEN
C                                 ! link set for wrong path
C                                 ! or bank has been dropped
          GZPLV0=0
          LKPROC=GZPROC()
          IF (LKPROC.NE.0) LPLV0=LQ(LKPROC-IZPLV0)
          GZPLV0=LPLV0
          PATH_LINK=PATH
        ELSE
          GZPLV0=LPLV0
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
