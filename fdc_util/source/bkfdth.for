      SUBROUTINE BKFDTH(TRKNUM,LKFDTH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book an FDTH FDC Track hit info bank
C-
C-   Inputs  : TRKNUM = track number
C-   Outputs : LKFDTH = link of booked FDTH bank
C-
C-   Created  20-JAN-1991   Jeffrey Bantly
C-   Updated   9-MAY-1991   Susan K. Blessing  Add three reference links
C-    to point to the segments used to build the track
C-   Updated  20-SEP-1991   Susan K. Blessing  Use TRKNUM in the call rather
C-    than the link of the supporting FDCT bank since that can be corrupted
C-    after the MZFORM call.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZFDTH.LINK/LIST'
C
      INTEGER IXFDTH
      INTEGER LFDCT,GZFDCT
      INTEGER LFDTH,LKFDTH
      INTEGER TRKNUM
C
      LOGICAL FIRST
C
      SAVE FIRST,IXFDTH
      DATA FIRST/.TRUE./
C------------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL MZFORM('FDTH','/*S',IXFDTH)
        FIRST=.FALSE.
      END IF
C
      LKFDTH=0
      LFDCT = GZFDCT(TRKNUM)
      IF(LFDCT.LE.0) GOTO 999
C
C  Book FDTH bank and store associated hits in FDTH bank
C
      CALL MZBOOK(IXMAIN,LFDTH,LFDCT,-IZFDTH,'FDTH',3,0,105,IXFDTH,0)
      LKFDTH=LFDTH
C
C------------------------------------------------------------------------
  999 RETURN
      END
