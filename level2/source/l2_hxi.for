      SUBROUTINE L2_HXI(ID,XVALUE,IBIN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns channel (CELL) number IBIN
C-                         corresponding to the real value  X
C-                         Under/overflows  return  0/NCHAN+1
C-
C-   Inputs  :   ID    = Histogram ID number
C-             XVALUE  = value along histograms X coordinate
C-   Outputs :  IBIN   = channel number
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
      INTEGER I,ID,NBIN,IBIN,ICLRD
      REAL BIN_LIMIT, BIN_SIZE, LLIM, ULIM, XVALUE
      LOGICAL L2_HEXIST
      CHARACTER*80 MSG
      DATA ICLRD /z'44524C43'/
C----------------------------------------------------------------------
C
C  *** Check if Histogram exists
C
      IF (.NOT.L2_HEXIST(ID)) THEN
        CALL L2_HBERR('L2_HXI','Histogram does not exist',ID)
        GO TO 999
      ENDIF
C
      NBIN = IH(LCHST(ID)+2)
      IF(NBIN.LE.0)THEN
        CALL L2_HBERR('HXI','Number of bins = 0',ID)
        GO TO 999
      ENDIF
C
      LLIM = H(LCHST(ID)+4)                      ! Lower limit of histogram
      ULIM = H(LCHST(ID)+5)                      ! Upper limit of histogram
C
      IF (XVALUE.LT.LLIM) THEN
        IBIN = 0                                 ! Underflows
      ELSE IF (XVALUE.GE.ULIM) THEN
        IBIN = NBIN + 1                          ! Overflows
      ELSE
        BIN_SIZE = (ULIM - LLIM)/NBIN
        DO I = 1, NBIN
          BIN_LIMIT = LLIM + I*BIN_SIZE
          IF (XVALUE.LT.BIN_LIMIT) THEN
            IBIN = I
            GOTO 999
          ENDIF
        ENDDO
      ENDIF
C
  999 RETURN
      END
