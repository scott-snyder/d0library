      SUBROUTINE BKFSEG(HALF,LAYER,LSEGM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book FSG0, FSG1, FSG2, FSG3, FSG4, FSG5 banks
C-
C-   Inputs  : HALF
C-             LAYER
C-   Outputs : LSEGM = Link to new bank
C-   Controls: 
C-
C-   Created   9-MAY-1991   Susan K. Blessing
C-   Updated  14-JUN-1991   Susan K. Blessing  Change MZLIFT calls to 
C-    MZBOOK calls.  Add 8 floating point words to the segment banks
C-    for segment fitting information.
C-   Updated  31-OCT-1991   Susan K. Blessing  Add a reference link to 
C-    a second track.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      INTEGER HALF,LAYER
      INTEGER MODULE
      INTEGER LSEGM,ID,NIO(0:5),NSEGM
      INTEGER NL,NS,ND
      INTEGER LSEG(0:5),GZFSEG
      INTEGER LKFTRH,GZFTRH
      INTEGER LZFIND,NZBANK
C
      LOGICAL FIRST
C
      CHARACTER*4 CHIDH(0:5)
C
      SAVE FIRST,CHIDH,NL,NS,NIO
      DATA CHIDH/'FSG0','FSG1','FSG2','FSG3','FSG4','FSG5'/
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL MZFORM('FSG0','19I 18F',NIO(0))
        CALL MZFORM('FSG1','19I 18F',NIO(1))
        CALL MZFORM('FSG2','35I 27F',NIO(2))
        CALL MZFORM('FSG3','19I 18F',NIO(3))
        CALL MZFORM('FSG4','19I 18F',NIO(4))
        CALL MZFORM('FSG5','35I 27F',NIO(5))
        NL = 2
        NS = 0
        FIRST = .FALSE.
      END IF
C
      MODULE = LAYER+3*HALF
C
      IF (LAYER.EQ.2) THEN
        ND = 62
      ELSE
        ND = 37
      END IF
C
      LKFTRH = GZFTRH(0)
      IF (LKFTRH.LE.0) THEN
        CALL ERRMSG('FTRAKS-Bank not booked','BKFSEG',
     &                    'FTRH bank not yet booked','I')
        CALL BKFTRH(LKFTRH)
      ENDIF
      LSEG(MODULE) = GZFSEG(HALF,LAYER)
      IF (LSEG(MODULE).EQ.0) THEN
        NSEGM = 0
      ELSE 
        NSEGM = NZBANK(IXCOM,LSEG(MODULE))
      END IF
C
C  Book bank for new segment
C
      IF (NSEGM.EQ.0 .OR. LSEG(MODULE).EQ.0) THEN
        CALL MZBOOK(IXCOM,LSEGM,LKFTRH,-3-MODULE,
     &    CHIDH(MODULE),NL,NS,ND,NIO(MODULE),0)
      ELSE
        LSEGM = LZFIND(IXCOM,LQ(LKFTRH-3-MODULE),NSEGM,-5)
        CALL MZBOOK(IXCOM,LSEGM,LSEGM,0,
     &    CHIDH(MODULE),NL,NS,ND,NIO(MODULE),0)
      END IF
C
  999 RETURN
      END
