      SUBROUTINE CJET_KTCLUSTER
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created 27-MAY-1993   Norman Amos, Chip Stewart - Kt type ALGORITHM
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
      INTEGER TOWERI,TOWERJ,IER,GZCATE,IRSQR,IBIG,I,J,K,L,N,IETA,IPHI
      INTEGER LAYER(17),NLAYER,IMIN,JMIN,IETMIN,IETALIM,NTMAX
      PARAMETER( NTMAX = 300 )
      REAL ETMIN,PHEP(8,NTMAX),RSQR,BIG,E(7),THETA,PHI,DIFF_PHI,ETA
      REAL DISTMAX,DIST,ETALIM,D(NTMAX,NTMAX)
C
      EQUIVALENCE ( IRSQR, RSQR )
      EQUIVALENCE ( IETMIN,ETMIN  )
      EQUIVALENCE ( IETALIM, ETALIM  )
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./,BIG/9.9E10/
      SAVE FIRST,RSQR,BIG,ETMIN,ETALIM
C----------------------------------------------------------------------
C
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        IRSQR  = ALG_PARAMS(2)       
        IETALIM = ALG_PARAMS(3)      
        IETMIN = ALG_PARAMS(4)     
        CALL EZPICK ('CALEVT_RCP')      ! Select CALEVT SRCP bank
        CALL EZGET('CATE_CLASS_OFFSET',ICLASS,IER)
        CALL EZGET('CATE_NEXT_OFFSET',INEXT,IER)
        CALL EZRSET
      ENDIF
C
C ****  Check for CATE bank
C
      IF ( LCATE .LE. 0 ) THEN
        LCATE = GZCATE()
        IF ( LCATE .LE. 0 ) THEN
          CALL ERRMSG ('CAJETS','CJET_KTCLUSTER','NO CATE bank','W')
          GOTO 999
        ENDIF
      ENDIF
C
C ****  Initialize some words in CLUPAR.INC; needed by CNEIGH_CATE
C
      NREP   = IQ(LCATE+2)                      ! Repetition number
      CALL GTCATE_TOTAL(NTOWER,NEMTWR,IER)
      ILO    = NEMTWR + 1                       ! First tower
      CALL GTCATE_MIN_TOWER(2,ETMIN,IHI)          ! gives last tower
                                        ! corresponding to min energy
C
C ****  Make each tower into single-tower clusters
C
      CALL CONNECT_INIT (IQ(LCATE+1),NREP,ICLASS,INEXT,ILO,IHI)
C
C ****  Loop over towers in CATE bank
C
      N = 0
      DO 10, TOWERI = ILO,IHI
        CALL GTCATE(TOWERI,IETA,IPHI,LAYER,NLAYER,E,IER)
        IF(ABS(IETA).GT.ETALIM) GOTO 10
        N = N + 1
        CALL UCOPY(E(1),PHEP(1,N),5)
        CALL ETOETA(E,PHEP(6,N),THETA,PHEP(7,N))
        PHEP(8,N) = TOWERI
        IF(N.EQ.NTMAX) THEN
          CALL ERRMSG ('NTOWERS EXCEEDED','CJET_KTCLUSTER',
     &      'TRUNCATE AT NTMAX','W')
          GOTO 11
        END IF
   10 CONTINUE
   11 CONTINUE
      DO I = 1, N
        DO J = I, N
          D(I,J) = MIN(PHEP(5,I)**2,PHEP(5,J)**2)
     &      *((DIFF_PHI(PHEP(6,I),PHEP(6,J))**2
     &      +(PHEP(7,I)-PHEP(7,J))**2)) / RSQR
          IF(I.EQ.J) D(I,I) = PHEP(5,J)**2
          D(J,I) = D(I,J)
        END DO
      END DO
    1 DISTMAX = 999999.
      DO I = 1, N
        DO J = I, N
          DIST  = D(I,J)
          IF(DIST.LT.DISTMAX) THEN
            DISTMAX = DIST
            IMIN = I
            JMIN = J
          END IF
        END DO
      END DO

      IF(IMIN.NE.JMIN) THEN   ! squeeze it out.
        DO 4, I = 1, 4
    4   PHEP(I,IMIN) = PHEP(I,IMIN) + PHEP(I,JMIN)
        PHEP(5,IMIN) = SQRT( PHEP(1,IMIN)**2 + PHEP(2,IMIN)**2)
        CALL ETOETA(PHEP(1,IMIN),PHEP(6,IMIN),THETA,PHEP(7,IMIN))
        TOWERI = PHEP(8,IMIN)
        TOWERJ = PHEP(8,JMIN)
        CALL CONNECT (IQ(LCATE+1),TOWERJ,TOWERI)
        DO I = 1, N
          D(I,IMIN) = MIN(PHEP(5,I)**2,PHEP(5,IMIN)**2)
     &      *((DIFF_PHI(PHEP(6,I),PHEP(6,IMIN))**2
     &      +(PHEP(7,I)-PHEP(7,IMIN))**2)) / RSQR
          IF(I.EQ.IMIN) D(I,I) = PHEP(5,I)**2
          D(IMIN,I) = D(I,IMIN)
        END DO
      END IF
      DO I = JMIN+1, N
        PHEP(1,I-1) = PHEP(1,I)
        PHEP(2,I-1) = PHEP(2,I)
        PHEP(3,I-1) = PHEP(3,I)
        PHEP(4,I-1) = PHEP(4,I)
        PHEP(5,I-1) = PHEP(5,I)
        PHEP(6,I-1) = PHEP(6,I)
        PHEP(7,I-1) = PHEP(7,I)
        PHEP(8,I-1) = PHEP(8,I)
        DO J = 1, N
          D(I-1,J) = D(I,J)
        ENDDO
        DO J = 1, N
          D(J,I-1) = D(J,I)
        ENDDO
      ENDDO
      N = N - 1
      IF(N.GE.2) GOTO 1
C
  999 RETURN
      END
