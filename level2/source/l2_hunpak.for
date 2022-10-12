      SUBROUTINE L2_HUNPAK(ID,CONT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the contants of histogram.
C-
C-   Inputs  : ID = Histogram ID number
C-   Outputs : CONT = Array containing contents of each bin
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
      INTEGER ID,NBIN,IBIN,ICLRD
      REAL CONT(*)
      LOGICAL L2_HEXIST
      DATA ICLRD /z'44524C43'/
C----------------------------------------------------------------------
C
C  *** Check if histogram exists
C
      IF (.NOT.L2_HEXIST(ID)) THEN
        CALL L2_HBERR('L2HUNPAK','Histogram does not exist',ID)
        GO TO 999
      ENDIF
C
      NBIN = IH(LCHST(ID)+2)
      IF(NBIN.LE.0)THEN
        CALL L2_HBERR('L2HUNPAK','Number of bins = 0',ID)
        GO TO 999
      ENDIF
C
      IF (IH(LCHST(ID)-4).NE.ICLRD) THEN
        DO IBIN = 1,NBIN
          CONT(IBIN) = FLOAT(KH(2*LCHST(ID)+2*CHEAD+IBIN))
        ENDDO
      ELSE
        DO IBIN = 1,NBIN
          CONT(IBIN) = H(LCHST(ID)+CHEAD+IBIN)
        ENDDO
      ENDIF
C
  999 RETURN
      END
