      PROGRAM TOP_LIMIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate upper limit for top cross section.
C-     Input parameters must be defined by an RCP file pointed to by
C-     the logical name TOP_LIMIT_RCP. The limit is first calculated
C-     analytically. The result is then checked using a Monte Carlo
C-     method. See D0Note xxxx for a discussion of the algorithms.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   2-NOV-1992   Richard Partridge
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      EXTERNAL FSIGMA
      EXTERNAL FCONLV
C
      INCLUDE 'D0$INC:TOP_LIMDATA.INC'
C
      INTEGER I
C
      REAL*8 SIG_NEXT
      REAL*8 DELTA
      REAL*8 SIG_CL
      REAL*8 DERIV
      REAL*8 ERR
      REAL*8 DGAUSS
C
C----------------------------------------------------------------------
C
C  Initialization
C
      CALL TLINIT
C
C  Find the normalization for the probability function FSIGMA
C
      SIG_INT(0) = 0.
      SIG_NEXT = 999.
      SIG_MIN(0) = 0.
      SIG_MAX(0) = 0.
      DELTA = DSIGMA/4.
      ITER = 0
      DO WHILE (SIG_NEXT.GT.EPS*SIG_INT(ITER))
        ITER = ITER + 1
        SIG_MIN(ITER) = SIG_MAX(ITER-1)
        SIG_MAX(ITER) = SIG_MIN(ITER) + DELTA
        SIG_NEXT = DGAUSS(FSIGMA,SIG_MIN(ITER),SIG_MAX(ITER),EPS)
        SIG_INT(ITER) = SIG_INT(ITER-1) + SIG_NEXT
        DELTA = DELTA/DMAX1(SIG_NEXT,0.1D0)
      ENDDO
C
C  Find the prescribed CL limit
C
      ICL = 1
      DO WHILE (SIG_INT(ICL).LT.CL*SIG_INT(ITER))
        ICL = ICL + 1
      ENDDO
      CALL DZERO(SIG_MIN(ICL),SIG_MAX(ICL),SIG_CL,ERR,EPS,1000,FCONLV)
      DERIV = DGAUSS(FSIGMA,0.995*SIG_CL,1.005*SIG_CL,EPS)/(.01*SIG_CL)
      WRITE (LMSG,1000) 100.*CL,SIG_CL,SIG_INT(ITER),100.*DERIV
C
C  Do a MC check for the cross section just found
C
      CALL TLMC(SIG_CL,DERIV)
C
  999 STOP
C
 1000 FORMAT(//' ANALYTICAL CALCULATION OF CROSS SECTION LIMIT'//
     &  4x,'Cross Section Limit at ',F5.1,'% CL:',T45,F10.3,' pb'/
     &  4x,'Probability Function Normalization:',T45,F10.5/
     &  4x,'CL Change for 1 pb Change in Limit:',T45,F10.3,'%')
      END
