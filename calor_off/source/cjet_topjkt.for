      SUBROUTINE CJET_TOPJKT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Connect CACL clusters formed in CJET_TOPJNN
C-      with a Kt type algorithm to define JETS with CMAP bank
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: CAJETS_RCP
C-
C-   Created 7-JUN-1993   Chip Stewart/Norm Amos 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CATENM.PARAMS'
      INCLUDE 'D0$INC:ZLINKC.INC'       ! Link area
      INCLUDE 'D0$INC:CLUPAR.INC'       ! Contains ILO, IHI and NREP
      INCLUDE 'D0$INC:CJET_ALGORITHM.INC'
C
C
      INTEGER CACLI,CACLJ,IER,IRSQR,I,J,K,L,N,IETA,IPHI,ICACL
      INTEGER LAYER(17),NLAYER,IMIN,JMIN,IETMIN,IETALIM,NTMAX,IV,NCACL
      INTEGER NJ,NJ_TARGET,GZCACL
      LOGICAL LNJ_TARGET
      PARAMETER( NTMAX = 70 )
      REAL ETMIN,PHEP(8,NTMAX),RSQR,E(7),THETA,PHI,DIFF_PHI,ETA,X,Y,Z
      REAL DISTMIN,DIST,ETALIM,D(NTMAX,NTMAX)
C
      EQUIVALENCE ( IRSQR, RSQR )
      EQUIVALENCE ( IETMIN,ETMIN  )
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      SAVE FIRST,RSQR,ETMIN
C----------------------------------------------------------------------
C
      IRSQR  = ALG_PARAMS(8)
      IETMIN = ALG_PARAMS(6)
      NJ_TARGET = ALG_PARAMS(9)
      LNJ_TARGET = ALG_PARAMS(10)
C
C ****  Check for CACL bank(s)
C
      LCACL = GZCACL()
      IF ( LCACL .LE. 0 ) THEN
        CALL ERRMSG('CALORIMETER','CJET_TOPJJETS',
     &    'NO CACL pre-cluster banks were found','W')
        GOTO 999
      ENDIF
C
C ****  Initialize CMAP bank
C
      CALL GTCACL_TOTAL (NCACL,IER)
      CALL BKCMAP (NCACL,LCMAP)
      CALL CMAPFL (LCACL)
C
C ****  Connect pre-clusters
C
      N = 0
      DO 10, ICACL = 1,NCACL
        CALL GTCACL(ICACL,IV,E,THETA,PHI,ETA,X,Y,Z,IER)
        IF(ABS(IETA).GT.ETALIM) GOTO 10
        N = N + 1
        CALL UCOPY(E(1),PHEP(1,N),5)
        PHEP(6,N) = PHI
        PHEP(7,N) = ETA
        PHEP(8,N) = ICACL
        IF(N.EQ.NTMAX) THEN
          CALL ERRMSG ('NCACL EXCEEDED','CJET_TOPJJETS',
     &      'TRUNCATE AT NTMAX','W')
          GOTO 11
        END IF
   10 CONTINUE
   11 CONTINUE
      CALL CACL_SCAN(ETMIN,NJ)
      DO I = 1, N
        DO J = I, N
          D(I,J) = MIN(PHEP(5,I)**2,PHEP(5,J)**2)
     &      *((DIFF_PHI(PHEP(6,I),PHEP(6,J))**2
     &      +(PHEP(7,I)-PHEP(7,J))**2)) / RSQR
          IF(I.EQ.J) D(I,I) = PHEP(5,J)**2
          D(J,I) = D(I,J)
        END DO
      END DO
    1 DISTMIN = 999999.
      DO I = 1, N
        DO J = I, N
          DIST  = D(I,J)
          IF(DIST.LT.DISTMIN) THEN
            DISTMIN = DIST
            IMIN = I
            JMIN = J
          END IF
        END DO
      END DO

      IF(IMIN.NE.JMIN) THEN   ! squeeze it out.
C
C ****  Quit if this step will take NJ below target
C
        IF((PHEP(5,IMIN).GT.ETMIN).AND.(PHEP(5,JMIN).GT.ETMIN)
     &    .AND.(NJ.EQ.NJ_TARGET)) GOTO 999
        DO 4, I = 1, 4
    4   PHEP(I,IMIN) = PHEP(I,IMIN) + PHEP(I,JMIN)
        PHEP(5,IMIN) = SQRT( PHEP(1,IMIN)**2 + PHEP(2,IMIN)**2)
        CALL ETOETA(PHEP(1,IMIN),PHEP(6,IMIN),THETA,PHEP(7,IMIN))
        CACLI = PHEP(8,IMIN)
        CACLJ = PHEP(8,JMIN)
        CALL CONNECT (IQ(LCMAP+1),CACLJ,CACLI)
        CALL CACL_SCAN(ETMIN,NJ)
        DO I = 1, N
          D(I,IMIN) = MIN(PHEP(5,I)**2,PHEP(5,IMIN)**2)
     &      *((DIFF_PHI(PHEP(6,I),PHEP(6,IMIN))**2
     &      +(PHEP(7,I)-PHEP(7,IMIN))**2)) / RSQR
          IF(I.EQ.IMIN) D(I,I) = PHEP(5,I)**2
          D(IMIN,I) = D(I,IMIN)
        END DO
      END IF
      DO I = JMIN+1, N
        DO 6, J = 1, 8
    6   PHEP(J,I-1) = PHEP(J,I)
        DO 8, J = 1, N
    8   D(I-1,J) = D(I,J)
        DO 9, J = 1, N
    9   D(J,I-1) = D(J,I)
      ENDDO
      N = N - 1
      IF(N.GE.2) GOTO 1
  999 RETURN
      END
