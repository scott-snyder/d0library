      SUBROUTINE L2_HPAK(ID,CONT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Packs the contants of histogram.
C-
C-   Inputs  : ID = Histogram ID number
C-             CONT = Array containing contents of each bin
C-   Outputs : none
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
      CHARACTER*80 MSG
      DATA ICLRD /'44524C43'X/
C----------------------------------------------------------------------
C
C  *** Check if Histogram exists
C
      IF (.NOT.L2_HEXIST(ID)) THEN
        CALL L2_HBERR('L2_HPAK','Histogram does not exist',ID)
        GO TO 999
      ENDIF
C
      NBIN = IH(LCHST(ID)+2)
      IF(NBIN.LE.0)THEN
        CALL L2_HBERR('L2_HPAK','Number of bins = 0',ID)
        GO TO 999
      ENDIF
C
      IF (IH(LCHST(ID)-4).NE.ICLRD) THEN        ! CLID bank
        DO IBIN = 1,NBIN
          KH(2*LCHST(ID)+2*CHEAD+IBIN) = NINT(CONT(IBIN))
        ENDDO
      ELSE                                      ! CLRD bank
        DO IBIN = 1,NBIN
          H(LCHST(ID)+CHEAD+IBIN) = CONT(IBIN)
        ENDDO
      ENDIF
C
  999 RETURN
      END
