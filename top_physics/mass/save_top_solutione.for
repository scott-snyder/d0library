      SUBROUTINE SAVE_TOP_SOLUTIONE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SAVE TOP_SOLUTION IN EXACT CASE
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   2-APR-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:KINEQ.INC'
      INCLUDE 'D0$INC:TOP_SOLNSE.INC'
C
      LOGICAL first
      SAVE first
      DATA first / .true. /
C
      REAL    BUFFER(200)
      INTEGER IS1,IS2,IOFF,SOLM
      INTEGER IND
      DOUBLE PRECISION    TMASS,WT
      REAL    DEL_MASS_MAX
C
      INTEGER IER,I,ISOL
      DOUBLE PRECISION WT_RR,WT_DG,RSOL
      LOGICAL READ_RCP
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL EZPICK('TOP_MASS_RCP')
        CALL EZGET('READ_EVENT_FROM_RCP',READ_RCP,IER)
        CALL EZRSET
      ENDIF
C
      BUFFER(1) = RUNC
      BUFFER(2) = EVENTC
      BUFFER(3) = ICONFG
      BUFFER(4) = ICMB
      BUFFER(5) = NCNFGE
C
      IOFF = 5
      CALL UCOPYDS(LEPTON1,BUFFER(IOFF+1),4)
      CALL UCOPYDS(LEPTON2,BUFFER(IOFF+5),4)
      CALL UCOPYDS(JET1,BUFFER(IOFF+9),4)
      CALL UCOPYDS(JET2,BUFFER(IOFF+13),4)
      CALL UCOPYDS(JET3,BUFFER(IOFF+17),4)
      CALL UCOPYDS(PNUT,BUFFER(IOFF+21),2)
C
      BUFFER(IOFF+23) = NTOPS
      BUFFER(IOFF+24) = TMASS_LO
      BUFFER(IOFF+25) = TMASS_HI
      IOFF = IOFF + 25
!
      I=NTOPS
      BUFFER(IOFF+1) = TOP_MASS(I)
      BUFFER(IOFF+2) = TOP_CROSS(I)
      BUFFER(IOFF+3) = TOP_QQB(I)
      BUFFER(IOFF+4) = TOP_GG(I)
      BUFFER(IOFF+5) = NSOLS(I)
!
      IOFF = IOFF + 5
      DO ISOL = 1 , 4
        BUFFER(IOFF+1) = ISOL
        BUFFER(IOFF+2) = RWEIGHT(ISOL,I)
        BUFFER(IOFF+3) = WEIGHTD(ISOL,I)
        IOFF = IOFF+3
!
        CALL UCOPYDS(NEUT1(1,ISOL,I),BUFFER(IOFF+1),4)
        IOFF = IOFF+4
        CALL UCOPYDS(WB1(1,ISOL,I),BUFFER(IOFF+1),4)
        IOFF = IOFF+4
        CALL UCOPYDS(T1(1,ISOL,I),BUFFER(IOFF+1),4)
        IOFF = IOFF+4
!
        CALL UCOPYDS(NEUT2(1,ISOL,I),BUFFER(IOFF+1),4)
        IOFF = IOFF+4
        CALL UCOPYDS(WB2(1,ISOL,I),BUFFER(IOFF+1),4)
        IOFF = IOFF+4
        CALL UCOPYDS(T2(1,ISOL,I),BUFFER(IOFF+1),4)
        IOFF = IOFF+4
!
!
      ENDDO
C
      WT_RR = 0.0D0
      WT_DG = 0.0D0
C
      DO ISOL  = 1 , 4
        WT_RR = WT_RR + RWEIGHT(ISOL,I)
        WT_DG = WT_DG + WEIGHTD(ISOL,I)
      ENDDO
C
      TMASS= TOP_MASS(I)
      RSOL = NSOLS(I)
      IF ( READ_RCP ) THEN
        IF ( ICMB.EQ.1 ) THEN
          IOFF = (COMBNUM-1)*10.
          CALL DO_HF1D(2001+IOFF,TMASS,RSOL)
          CALL DO_HF1D(2002+IOFF,TMASS,WT_RR)
          CALL DO_HF1D(2003+IOFF,TMASS,WT_DG)
        ELSE
          IOFF = (COMBNUM-1)*10.
          CALL DO_HF1D(2004+IOFF,TMASS,RSOL)
          CALL DO_HF1D(2005+IOFF,TMASS,WT_RR)
          CALL DO_HF1D(2006+IOFF,TMASS,WT_DG)
        ENDIF
      ENDIF
C
      CALL DO_HFN('DILEPTON',200,BUFFER)
C
      LIKELY(I,ICMB,1) = LIKELY(I,ICMB,1) + WT_RR
      LIKELY(I,ICMB,2) = LIKELY(I,ICMB,2) + WT_DG
C
  999 RETURN
      END
