      FUNCTION L2_HI(ID,IBIN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns channel contents
C-
C-   Inputs  : ID = Histogram ID number
C-   Outputs : IBIN = Will return channel contants of this bin
C-   Controls: none
C-
C-   Created  22-JAN-1990   Jan Guida, Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBHST.INC'
      INCLUDE 'D0$INC:L2CLBH.INC'
C
      INTEGER ID,IBIN,NBIN,ICLRD
      REAL L2_HI
      LOGICAL L2_HEXIST
      DATA ICLRD /z'44524C43'/
C----------------------------------------------------------------------
C
C  *** Check if histogram exists
C
      IF (.NOT.L2_HEXIST(ID)) THEN
        CALL L2_HBERR('L2_HI','Histogram does not exist',ID)
        GO TO 999
      ENDIF
C
      NBIN = IH(LCHST(ID)+2)
      IF(NBIN.EQ.0)THEN
        CALL L2_HBERR('L2_HI','Number of Bins = 0',ID)
        L2_HI = 0.
        GO TO 999
      ENDIF
C
      IF(IBIN.EQ.0)THEN
        L2_HI = H(LCHST(ID)+7)
      ELSEIF(IBIN.EQ.NBIN+1)THEN
        L2_HI = H(LCHST(ID)+8)
      ELSE
        IF (IH(LCHST(ID)-4).NE.ICLRD) THEN       ! CLID bank
          L2_HI = FLOAT(KH(2*LCHST(ID)+2*CHEAD+IBIN))
        ELSE                                          ! CLRD bank
          L2_HI = H(LCHST(ID)+CHEAD+IBIN)
        ENDIF
      ENDIF
C
  999 RETURN
      END
