      SUBROUTINE LO_CALC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate leading order triple differential cross
C-                         sections and call routines to fill n-tuple
C-
C-   Inputs  :  None
C-   Outputs :  None
C-   Controls:  LO_RCP
C-
C-   Created  18-Oct-1993   Sandor Feher and Patrick Mooney
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      INCLUDE 'D0$INC:LO_COM.INC'
C
C----------------------------------------------------------------------
C
C   specify indexing integers
C
      INTEGER     IA,IB,I,J,K,ITYP,NTYP,JC,COUNTER
      PARAMETER   (NTYP = 12)
C
      REAL        Q2CHECK
      REAL*8      XMIN,XMAX,Q2MIN,Q2MAX
      REAL*8      DXPDF(-6:6),DXPDF_T(-6:6),DXPDF_B(-6:6)
C
C   specify integers which determine how many bins the differential cross
C    section is calculated for
C
      INTEGER     NVAR1,NVAR2,NVAR3
C
C   variables related to the differential cross sectoin calculation

      REAL*8      SHAT,THAT,UHAT
      REAL*8      ALPHAS2,A_S,ETA_STORE,DISCR,D3S_STORE
      REAL*8      DSIGMA_DT_ABTERM
      COMMON/W50513/ XMIN,XMAX,Q2MIN,Q2MAX
C
C
C----------------------------------------------------------------------
C
C   loop through values of VAR1, VAR2 and VAR3 and calculate the triple
C    differential cross section for each combination of them
C
      COUNTER = 0
      NVAR1 = NINT((VARUP1-VARLOW1)/SVAR1) + 1
      NVAR2 = NINT((VARUP2-VARLOW2)/SVAR2) + 1
      NVAR3 = NINT((VARUP3-VARLOW3)/SVAR3) + 1
      DO I = 1, NVAR1
        VAR1 = (I-1)*SVAR1 + VARLOW1
        IF(I.EQ.NVAR1) VAR1 = VARUP1
        DO J = 1, NVAR2
          VAR2 = (J-1)*SVAR2 + VARLOW2
          IF(J.EQ.NVAR2) VAR2 = VARUP2
          DO K = 1, NVAR3
            VAR3 = (K-1)*SVAR3 + VARLOW3
            IF(K.EQ.NVAR3) VAR3 = VARUP3
C
C   calculate KINEMATIC variables needed for differential cross section
C    calculation
C
            IF (TRIPLE_DIFF_FLAG.EQ.1) THEN       !V1=eta1,V2=eta2,V3=Pt
              ETA1 = VAR1
              ETA2 = VAR2
              PT = VAR3
              XA = (PT/SQ_S) * (EXP( ETA1)+EXP( ETA2))
              XB = (PT/SQ_S) * (EXP(-ETA1)+EXP(-ETA2))
            ELSEIF (TRIPLE_DIFF_FLAG.EQ.2) THEN   !V1=eta_str,V2=eta2,V3=Mjj
              ETA_STR = VAR1
              ETA2 = VAR2
              MJJ =VAR3
              ETA1 = 2*ETA_STR+ETA2
              PT = MJJ / (SQRT(2*(1+COSH(ETA1-ETA2))))
              XA = (PT/SQ_S) * (EXP( ETA1)+EXP( ETA2))
              XB = (PT/SQ_S) * (EXP(-ETA1)+EXP(-ETA2))
            ELSEIF (TRIPLE_DIFF_FLAG.EQ.3) THEN   !V1=eta_str,V2=eta_bst,V3=Mjj
              ETA_STR = VAR1
              ETA_BST = VAR2
              MJJ = VAR3
              ETA1 = ETA_BST+ETA_STR
              ETA2 = ETA_BST-ETA_STR
              PT = MJJ / (SQRT(2*(1+COSH(ETA1-ETA2))))
              XA = (PT/SQ_S) * (EXP( ETA1)+EXP( ETA2))
              XB = (PT/SQ_S) * (EXP(-ETA1)+EXP(-ETA2))
            ELSEIF (TRIPLE_DIFF_FLAG.EQ.4) THEN   !V1=x2,V2=x2,V3=Pt**2
              XA = VAR1
              XB = VAR2
              PT = SQRT(VAR3)
              DISCR = (XA*SQ_S/(2*PT))**2 - XA/XB
              IF(DISCR.LT.0.) GOTO 1011
              ETA1 = log(XA*SQ_S/(2*PT) + SQRT(DISCR))
              ETA2 = log(XA*SQ_S/(2*PT) - SQRT(DISCR))
              IF(ABS(ETA1).GT.6. .OR.  ABS(ETA2).GT.6.) GOTO 1011
            ENDIF
            IF(XA.LT.1. .AND. XB.LT.1.) THEN
C
C   Check PDFLIB parameters to see if there within range
C
            IF(LIMIT_FLAG.EQ.1.) THEN
            Q2CHECK = (PT * PT_SCALE_FACTOR)**2
             IF(XA.GT.XMAX .OR. XB.GT.XMAX .OR. 
     &         XA.LT.XMIN .OR. XB.LT.XMIN .OR.
     &         Q2CHECK.GT.Q2MAX .OR. Q2CHECK.LT.Q2MIN) GOTO 9922
            ENDIF
C
C   call PDFLIB routine to determine value of PARTON DISTRIBUTION FUNCTION at
C    the present values of XA, XB, and PT_SCALE_FACTOR*PT
C
              CALL PFTOPDG(XA,PT_SCALE_FACTOR*PT,DXPDF)
              CALL PFTOPDG(XB,PT_SCALE_FACTOR*PT,DXPDF_T)
C
C   for the ANTIPROTON the antiquark and quark distributions are EXCHANGED
C    relative to the PROTON
C
              DO IB = -6,6
                IF (IB.EQ.0) THEN
                  DXPDF_B(IB) = DXPDF_T(IB)
                ELSE
                  DXPDF_B(IB) = DXPDF_T(-IB)
                ENDIF
              ENDDO
C
C   use PDFLIB to determine the strong coupling constant, alpha_s
C
              JC = 0
  110         SHAT   =  XA*XB*SQ_S**2
              THAT   = -XA*PT*SQ_S*EXP(-ETA1)
              UHAT   = -XB*PT*SQ_S*EXP( ETA1)
              A_S    = ALPHAS2(PT_SCALE_FACTOR*PT)
C
C   call a subroutine to calculate the SUBPROCESS DIFFERENTIAL CROSS SECTION
C    for the choice of THAT and UHAT chosen above (i.e. THAT and UHAT are
C    functions of ETA1 and not of ETA2)
C
              CALL LO_DSIG_DT(SHAT,THAT,UHAT,A_S,DSIG_DT)
C
C
C   calculate the TRIPLE DIFFERENTIAL CROSS SECTION in the massless limit
C    (i.e. do not include the top quark)
C
              D3S_E1_E2_PT2 = 0.0
              DO IA = -5,5
                DO IB = -5,5
                  CALL LO_DSIG_DT_SEL(IA,IB,DSIG_DT,
     &                  DSIGMA_DT_ABTERM)
                  D3S_E1_E2_PT2 = D3S_E1_E2_PT2 + (DXPDF(IA) *
     &                  DXPDF_B(IB) * DSIGMA_DT_ABTERM) * 389000.0
                ENDDO
              ENDDO
C
C   fill the N-tuples
C
C
              IF (TRIPLE_DIFF_FLAG.EQ.4) THEN   !V1=x2,V2=x2,V3=Pt**2
                IF (JC.EQ.0) THEN
                  JC = 1
                  ETA1 = ETA2
                  D3S_STORE = D3S_E1_E2_PT2
                  GOTO 110
                ELSE
                  D3S_E1_E2_PT2 = D3S_E1_E2_PT2 + D3S_STORE
                ENDIF
              ENDIF
C
              CALL LO_NT
              COUNTER = COUNTER + 1
              IF(MOD(COUNTER,1000).EQ.0.0) THEN
                PRINT 2001, COUNTER
              ENDIF
C
 9922       ENDIF
 1011     ENDDO
        ENDDO
      ENDDO
C
C----------------------------------------------------------------------
 2001 FORMAT(I8,'  ******* BINS ARE PROCESSED *******')
  999 RETURN
      END
