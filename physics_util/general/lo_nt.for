      SUBROUTINE LO_NT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  To set up and fill N-tuple
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  18-Oct-1993   Sandor Feher and Patrick Mooney
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:LO_COM.INC'
C----------------------------------------------------------------------
C     fake PAWC.INC
C----------------------------------------------------------------------
      INTEGER NPAWC
      PARAMETER (NPAWC=100000)
      INTEGER PAGELEN
      PARAMETER( PAGELEN = 128 )        ! LENGTH OF PAGE
      INTEGER MAXPAGES
      PARAMETER( MAXPAGES = NPAWC/PAGELEN )
      CHARACTER*32 GNAME
C
      COMMON/PAWC/NWPAW,IXPAWC,IHDIV,IXHIGZ,IXKU,FENC(5),LPMAIN,
     &  HCV(9989)
C
      REAL HMEMOR(PAGELEN*MAXPAGES)
      INTEGER NWPAW,IXPAWC,IHDIV,IXHIGZ,IXKU,FENC,LPMAIN,HCV
      EQUIVALENCE(NWPAW,HMEMOR(1))
      INTEGER IP(2),LP(8000)
      REAL    P(2)
      EQUIVALENCE (LP(1),LPMAIN),(IP(1),LP(9)),(P(1),IP(1))
C
C----------------------------------------------------------------------
C
C   set-up N-tuple arrays
C
      INTEGER     N,NT1
      PARAMETER   (NT1 = 4)
      CHARACTER   TAG1(NT1)*9,TAG2(NT1)*9,TAG3(NT1)*9,TAG4(NT1)*9
      CHARACTER   TITLE*80
      REAL        XT1(NT1)
      REAL*8      DIV_CHECK,DCOS_DPT2,DIS
      DATA TAG1/  'ETA1','ETA2','Pt','D3SIG'/
      DATA TAG2/  'ETAS','ETA2','Mjj','D3SIG'/
      DATA TAG3/  'ETAS','ETAB','Mjj','D3SIG'/
      DATA TAG4/  'XA','XB','Pt2','D3SIG'/
C
      LOGICAL     FIRST
      DATA        FIRST/.TRUE./
      IF(FIRST) THEN
        FIRST = .FALSE.
        PRINT 2000
 2000   FORMAT(//,'   Be patient the program is filling the N-tuple  ')
        CALL HLIMIT(-PAGELEN*MAXPAGES)
C
        CALL HCDIR('//PAWC',' ')
        CALL HMDIR('//PAWC/LO','S')
        CALL HROPEN(LUN,'LO',FILENAME(1:LEN),'N',1024,IER)
        CALL HCDIR('//PAWC/LO',' ')
        CALL HCDIR('//LO',' ')
C
C
        IF (TRIPLE_DIFF_FLAG.EQ.1) THEN       !V1=eta1,V2=eta2,V3=Pt
          CALL HBOOKN(1,'D3SIG/(deta1deta2dPt)',NT1,'LO',100,TAG1)
        ELSEIF (TRIPLE_DIFF_FLAG.EQ.2) THEN   !V1=eta_str,V2=eta2,V3=Mjj
          CALL HBOOKN(1,'D3SIG/(deta_strdeta2dMjj)',NT1,'LO',100,TAG2)
        ELSEIF (TRIPLE_DIFF_FLAG.EQ.3) THEN   !V1=eta_str,V2=eta_bst,V3=Mjj
          CALL HBOOKN(1,'D3SIG/(deta_strdeta_bstdMjj)',NT1,'LO',100,
     &      TAG3)
        ELSEIF (TRIPLE_DIFF_FLAG.EQ.4) THEN   !V1=x2,V2=x2,V3=Pt**2
          CALL HBOOKN(1,'D3SIG/(dXadXbdPt2)',NT1,'LO',100,TAG4)
        ENDIF
C
      ENDIF
C
      IF (TRIPLE_DIFF_FLAG.EQ.1) THEN       !V1=eta1,V2=eta2,V3=Pt
C
C----------------------------------------------------------------------
C
C   fill the N-tuple - XT1
C
        XT1(1) = ETA1
        XT1(2) = ETA2
        XT1(3) = PT
        XT1(4) = D3S_E1_E2_PT2 * 2 * PT
C
C
        CALL HCDIR('//PAWC/LO',' ')
        CALL HCDIR('//LO',' ')
        CALL HFN(1,XT1)
C
      ELSEIF (TRIPLE_DIFF_FLAG.EQ.2) THEN   !V1=eta_str,V2=eta2,V3=Mjj
C----------------------------------------------------------------------
C
C   fill the N-tuple - XT1
C
        XT1(1) = ETA_STR
        XT1(2) = ETA2
        XT1(3) = MJJ
        XT1(4) = D3S_E1_E2_PT2 * (2 * MJJ) / (1+COSH(2*ETA_STR))
C
C
        CALL HCDIR('//PAWC/LO',' ')
        CALL HCDIR('//LO',' ')
        CALL HFN(1,XT1)
C
      ELSEIF (TRIPLE_DIFF_FLAG.EQ.3) THEN   !V1=eta_str,V2=eta_bst,V3=Mjj
C----------------------------------------------------------------------
C
C   fill the N-tuple - XT1
C
        XT1(1) = ETA_STR
        XT1(2) = ETA_BST
        XT1(3) = MJJ
        XT1(4) = D3S_E1_E2_PT2 * (2 * MJJ) / (1+COSH(2*ETA_STR))
C
C
        CALL HCDIR('//PAWC/LO',' ')
        CALL HCDIR('//LO',' ')
        CALL HFN(1,XT1)
C
      ELSEIF (TRIPLE_DIFF_FLAG.EQ.4) THEN   !V1=x2,V2=x2,V3=Pt**2
C----------------------------------------------------------------------
C
C   fill the N-tuple - XT1
C
        XT1(1) = XA
        XT1(2) = XB
        XT1(3) = PT**2
        DIS = XA*XB*SQ_S**2*(XA*XB*SQ_S**2 - 4*PT**2)
        IF(DIS.GT.0.) THEN
          DCOS_DPT2 = 2*1/SQRT(DIS)
          XT1(4) = D3S_E1_E2_PT2 * .5 * SQ_S**2 * DCOS_DPT2
C
C
          CALL HCDIR('//PAWC/LO',' ')
          CALL HCDIR('//LO',' ')
          CALL HFN(1,XT1)
C
        ENDIF
      ENDIF
C
  999 RETURN
C
C----------------------------------------------------------------------
      END
