      SUBROUTINE L2_HIX(ID,IBIN,XVALUE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns X value of lower edge
C-                         of channel (CELL) number IBIN
C-
C-   Inputs  :   ID    = Histogram ID number
C-              IBIN   = channel number
C-   Outputs : XVALUE  = lower edge bin value
C-   Controls: none
C-
C-   Created  24-MAR-1993   Dan Claes (Following L2HBOOK routines
C-                                     by Jan Guida, SRajagapolan)
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBHST.INC'
      INCLUDE 'D0$INC:L2CLBH.INC'
C
      INTEGER ID,NBIN,IBIN,ICLRD
      REAL BIN_SIZE, LLIM, ULIM, XVALUE
      LOGICAL L2_HEXIST
      CHARACTER*80 MSG
      DATA ICLRD /'44524C43'X/
C----------------------------------------------------------------------
C
C  *** Check if Histogram exists
C
      XVALUE = 0
C
      IF (.NOT.L2_HEXIST(ID)) THEN
        CALL L2_HBERR('L2_HXI','Histogram does not exist',ID)
        GO TO 999
      ENDIF
C
      NBIN = IH(LCHST(ID)+2)
      IF(NBIN.LE.0)THEN
        CALL L2_HBERR('L2_HIX','Number of bins = 0',ID)
        GO TO 999
      ENDIF
C
      LLIM = H(LCHST(ID)+4)                      ! Lower limit of histogram
      ULIM = H(LCHST(ID)+5)                      ! Upper limit of histogram
C
      IF (IBIN.GT.NBIN) THEN
        CALL L2_HBERR('L2_HIX','Call exceeds number of bins',ID)
        XVALUE = ULIM                            ! Overflows
        GOTO 999
      ELSE
        BIN_SIZE = (ULIM - LLIM)/NBIN
        XVALUE = LLIM + (IBIN-1)*BIN_SIZE
        GOTO 999
      ENDIF
C
  999 RETURN
      END
