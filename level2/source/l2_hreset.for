      SUBROUTINE L2_HRESET(ID,NEWTITLE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Zeroes the contents of all channels
C-                         to histogram, ID
C-
C-   Inputs  : ID     = Histogram ID number to be reset
C-                      0 for all histograms (not implemented yet)
C-             TITLE  = NEW Histogram Title
C_                      ' ' retain old title
C-   Outputs : none
C-   Controls: none
C-
C-   Created  14-APR-1993   Dan Claes, following the L2HBOOK
C-                          form of J. Guida, S. Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBHST.INC'
      INCLUDE 'D0$INC:L2CLBH.INC'
C
      INTEGER ID, IBIN, NBIN, ICLRD
      INTEGER ITITLE(20),IT,LTITL
      LOGICAL L2_HEXIST
      CHARACTER*(*) NEWTITLE
      CHARACTER*80 TITLE
      CHARACTER*4 SHTITLE(20)
      EQUIVALENCE (ITITLE(1),SHTITLE(1),TITLE)
C
      DATA ICLRD /'44524C43'X/
C----------------------------------------------------------------------
C
C      IF(ID.EQ.0) THEN
C      ELSE
C
C     Check that Histogram has been booked. If not ABORT.
C
        IF (.NOT.L2_HEXIST(ID)) THEN
          CALL L2_HBERR('L2HRESET',
     &      'Attempt to reset unbooked Histogram',
     &      ID)
          GO TO 999
        ENDIF
        NBIN = IH(LCHST(ID)+2)
        IF(NBIN.LE.0)THEN
          CALL L2_HBERR('L2HRESET','Number of bins = 0',ID)
          GO TO 999
        ENDIF
C
        IF (IH(LCHST(ID)-4).NE.ICLRD) THEN        ! CLID bank
          DO IBIN = 1,NBIN
            KH(2*LCHST(ID)+2*CHEAD+IBIN) = 0
          ENDDO
        ELSE                                      ! CLRD bank
          DO IBIN = 1,NBIN
            H(LCHST(ID)+CHEAD+IBIN) = 0.00
          ENDDO
        ENDIF
C
C      ENDIF
C
       IH(LCHST(ID)+3) = 0               ! reset TOTAL # of entries
       H(LCHST(ID)+6) = 0.               ! reset Weighted Valid entries
       H(LCHST(ID)+7) = 0.               ! reset Underflows
       H(LCHST(ID)+8) = 0.               ! reset Overflows
C
       IF (NEWTITLE.NE.' ') THEN
         TITLE = NEWTITLE
         DO IT=1,20
           IH(LTITL+IT) = ITITLE(IT)
         ENDDO
       ENDIF
C
  999 RETURN
      END
