      SUBROUTINE BKFITR(LKFITR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the bank FITR hanging from FTRH
C-
C-   Inputs  : none
C-   Outputs : LKFITR = Link of FITR bank after booking
C-
C-   Created   1-MAY-1989   Jeffrey Bantly
C-   Updated   2-JUL-1990   Jeffrey Bantly  use Link area, add SAVE 
C-   Updated   2-DEC-1991   Robert E. Avery  Increase initial number of tracks.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:FDLTRK.INC'
      INCLUDE 'D0$LINKS:IZFITR.LINK'
      INTEGER KZTRH, I, LKFITR, LKFTRH
      INTEGER NTRKS,NDAMAX, IOMAST, ICALL
      INTEGER GZFITR,GZFTRH
C
      SAVE NTRKS,NDAMAX,ICALL
      DATA NTRKS,NDAMAX,ICALL /500,9,0/
C----------------------------------------------------------------------
      IF (ICALL.EQ.0 ) THEN
        ICALL = 1
        CALL MZFORM ( 'FITR', '2I -F', IOMAST)
      ENDIF
C
C   Check for top FDC track bank FTRH, book if not found
C   Bank FTRH, header for FDC Tracks and segments
C
      LKFTRH = GZFTRH(0)
      IF ( LKFTRH .LE. 0 ) CALL BKFTRH(LKFTRH)
C
C   Book FITR if not already done
C
      LKFITR = GZFITR(0)
      IF ( LKFITR .LE. 0 ) THEN
        CALL MZBOOK(IXMAIN, LFITR, LFTRH, -IZFITR,
     &               'FITR', 0, 0, NTRKS*NDAMAX + 2, IOMAST, 0)  
        LKFITR = LFITR
        IQ ( LKFITR + 2 ) = NDAMAX
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
