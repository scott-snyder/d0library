      SUBROUTINE FTRHFL(LKFTRH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize the FTRH bank at booking time.
C-
C-   Inputs  : LKFTRH = bank link value
C-
C-   Created  20-JAN-1991   Jeffrey Bantly
C-   Updated  11-FEB-1993   Susan K. Blessing  Put size of CDD3 bank in 
C-    +8 word and ALIGN_LEVEL in +9 word.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCDD3.LINK'
C
      INTEGER LKFTRH,IER
      INTEGER NHIT
      INTEGER LCDD3
      INTEGER ALIGN_LEVEL
C
      REAL    Z0(2)
C
      LOGICAL FIRST
C
      SAVE FIRST,Z0
C
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('Z0',Z0,IER)
        CALL EZGET('ALIGN_LEVEL',ALIGN_LEVEL,IER)
        CALL EZRSET
      ENDIF
C
C  Version 0 uses only words 1 and 2 with all words booked as integers.
C  Version 1 uses words 1 to 6 with 3 and 4 booked as floating point.
C
      IQ(LKFTRH+1)=1                    ! Version number
      IQ(LKFTRH+2)=0                    ! Number of FDC tracks
      Q(LKFTRH+3)=Z0(1)                 ! Z0(FDC half 0)
      Q(LKFTRH+4)=Z0(2)                 ! Z0(FDC half 1)
      IQ(LKFTRH+5)=0                    ! Number of tracks in half 0
      IQ(LKFTRH+6)=0                    ! Number of tracks in half 1
C
      IQ(LKFTRH+7) = 0                  ! Number of SW hits in FDC
C
      LCDD3 = LQ(LHEAD - IZCDD3)        
      IQ(LKFTRH+8) = IQ(LCDD3-1)        ! Size of CDD3 bank
C
      IQ(LKFTRH+9) = ALIGN_LEVEL
      IQ(LKFTRH+10)=0                   ! Spare
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
