      SUBROUTINE L2_HFILL(ID,VALUEX,VALUEY,WEIGHT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill One dimensional Histograms in Zebra banks.
C-
C-   Inputs  : ID     = Histogram identifier <= 10000 (INTEGER)
C-             VALUEX  = X Value to be filled (REAL)
C-             VALUEY  = Y Value to be filled (REAL) Dummy
C-             WEIGHT = Weighting factor to be applied to VALUE.
C-   Outputs : none
C-   Controls: none
C-
C-   Created  18-JAN-1990   Jan Guida, Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBHST.INC'
      INCLUDE 'D0$INC:L2CLBH.INC'
C
      INTEGER ID,BIN,NBIN,ICLRD
C
      REAL VALUEX,VALUEY,WEIGHT,BINSIZ
      REAL ULIM,LLIM
C
      LOGICAL L2_HEXIST
      CHARACTER*60 TITLE
      DATA ICLRD /z'44524C43'/
C----------------------------------------------------------------------
C
C  *** Check if Histogram has been booked. If not ABORT.
C
      IF (.NOT.L2_HEXIST(ID)) THEN
        CALL L2_HBERR('L2_HFILL',
     &    'Attempt to fill unbooked Histogram',ID)
        GO TO 999
      ENDIF
C
      LLIM = H(LCHST(ID)+4)                         ! Lower limit
      ULIM = H(LCHST(ID)+5)                         ! Upper Limit
      IH(LCHST(ID)+3) = IH(LCHST(ID)+3) + 1
      IF (VALUEX.LT.LLIM) THEN
        H(LCHST(ID)+7) = H(LCHST(ID)+7) + WEIGHT    ! Underflows
      ELSE IF (VALUEX.GE.ULIM) THEN
        H(LCHST(ID)+8) = H(LCHST(ID)+8) + WEIGHT    ! Overflows
      ELSE
        H(LCHST(ID)+6)  = H(LCHST(ID)+6)  + WEIGHT
        NBIN = IH(LCHST(ID)+2)                      ! # of Bins
        IF (NBIN.GT.0) THEN
          BINSIZ = (ULIM-LLIM)/FLOAT(NBIN)
          BIN = INT((VALUEX-LLIM)/BINSIZ) + 1
          IF (IH(LCHST(ID)-4).NE.ICLRD) THEN       ! CLID bank
            KH(2*LCHST(ID)+2*CHEAD+BIN) = KH(2*LCHST(ID)+
     &                         2*CHEAD+BIN) + 1
          ELSE                                          ! CLRD bank
            H(LCHST(ID)+CHEAD+BIN) = H(LCHST(ID)+CHEAD+BIN) + WEIGHT
          ENDIF
        ENDIF
      ENDIF
C
  999 RETURN
      END
