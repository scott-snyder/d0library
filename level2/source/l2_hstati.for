      FUNCTION L2_HSTATI(ID,ICASE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns mean value, standard deviation, skewness,
C-   kurtosis or number of equivalent events of a 1-D hist.
C-
C-   Inputs  : ID = Histogram ID number
C-             ICASE = 1 - mean value
C-                     2 - standard deviation
C-                     3 - skewness
C-   Outputs : L2_HSTATI
C-
C-   Created  22-JAN-1990   Jan Guida, Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBHST.INC'
      INCLUDE 'D0$INC:L2CLBH.INC'
C
      INTEGER ID,ICASE,NBIN,IBIN
      INTEGER ICLRD
      REAL MOM1,MOM2,MOM3,SIG2,AVR,L2_HSTATI
      REAL BINSIZ,LLIM,ULIM,BINVAL,WNEVT
      LOGICAL L2_HEXIST
      CHARACTER*45 MSG
      DATA ICLRD /'44524C43'X/
C----------------------------------------------------------------------
C
C  *** Check if histogram exists
C
      IF(ICASE.LE.0 .OR. ICASE.GT.3) THEN
        WRITE(MSG,30)ICASE
        CALL L2_HBERR('L2HSTATI',MSG,ID)
        L2_HSTATI = 0.
        GO TO 999
      ENDIF
C
      IF (.NOT.L2_HEXIST(ID)) THEN
        CALL L2_HBERR('L2HSTATI','Histogram not booked',ID)
        GO TO 999
      ENDIF
C
      IF(ICASE.EQ.3) THEN
        L2_HSTATI = 0.
        GO TO 999
      ENDIF
C
      NBIN = IH(LCHST(ID)+2)
      LLIM = H(LCHST(ID)+4)
      ULIM = H(LCHST(ID)+5)
      BINSIZ = (ULIM-LLIM)/FLOAT(NBIN)
      WNEVT = H(LCHST(ID)+6)
C
      IF(WNEVT.LE.0)THEN
        CALL L2_HBERR('L2HSTATI','No entries in Histogram',ID)
        GO TO 999
      ENDIF
C
      MOM1 = 0.
      IF (IH(LCHST(ID)-4).NE.ICLRD) THEN       ! CLID bank
        DO IBIN=1,NBIN
          BINVAL = (IBIN-0.5)*BINSIZ + LLIM
          MOM1 = MOM1 + KH(2*LCHST(ID)+2*CHEAD+IBIN)*BINVAL
        ENDDO
      ELSE                                     ! CLRD bank
        DO IBIN=1,NBIN
          BINVAL = (IBIN-0.5)*BINSIZ + LLIM
          MOM1 = MOM1 + H(LCHST(ID)+CHEAD+IBIN)*BINVAL
        ENDDO
      ENDIF
      AVR = MOM1/WNEVT
C
      IF(ICASE.EQ.1)THEN
        L2_HSTATI = AVR
        GO TO 999
      ENDIF
C
      MOM2 = 0.
      IF (IH(LCHST(ID)-4).NE.ICLRD) THEN       ! CLID bank
        DO IBIN=1,NBIN
          BINVAL = (IBIN-0.5)*BINSIZ + LLIM
          MOM2 = MOM2 + KH(2*LCHST(ID)+2*CHEAD+IBIN)*BINVAL**2
        ENDDO
      ELSE                                      ! CLRD bank
        DO IBIN=1,NBIN
          BINVAL = (IBIN-0.5)*BINSIZ + LLIM
          MOM2 = MOM2 + H(LCHST(ID)+CHEAD+IBIN)*BINVAL**2
        ENDDO
      ENDIF
      SIG2 = MOM2/WNEVT - AVR**2
      IF(SIG2.LT.0)THEN
        CALL L2_HBERR('L2HSTATI','Sigma**2 is < 0',ID)
        GO TO 999
      ENDIF
      L2_HSTATI = SQRT(SIG2)
C
   30 FORMAT('Invalid ICASE specification, ICASE = ',I3)
C
  999 RETURN
      END
