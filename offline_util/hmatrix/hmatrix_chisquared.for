      SUBROUTINE HMATRIX_CHISQUARED
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : WORK OUT CHISQUARED FOR VISIBLE QUANTITIES
C-
C-   Inputs  : QUAN(1,VIS_DIM) AND HVIS
C-   Outputs : CHISQ and PROB that CHISQUARED WILL BE EXCEEDED.
C-   Controls: 
C-
C-   Created  26-DEC-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
      INCLUDE 'D0$INC:HMATRIX_PARS.INC'
      INTEGER I,J,IER
      REAL    CHISQ,DI,DJ,PROB,PROBL,HDI
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST / .TRUE. /

      DOUBLE PRECISION DHVIS,HMATRIX_DVECT,HMATRIX_DARRAY
      LOGICAL DO_CHIS_VECTOR
C----------------------------------------------------------------------
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('HMATRIX_RCP')
        CALL EZGET('DO_CHIS_VECTOR',DO_CHIS_VECTOR,IER)
        CALL EZRSET
      ENDIF
C
      IF ( .NOT.ACCUMULATE ) THEN
        IF ( DO_CHIS_VECTOR ) THEN
C CREATE A STAND ALONE CHISQUARED VECTOR THAT CONTAINS INDIVIDUAL 
C CONTRIBUTIONS OF ELEMENTS TO CHISQUARED
          CALL BKCHIS(VIS_DIM)
        ENDIF
        CHISQ = 0.0
        DO I = 1 , VIS_DIM
          DI = C(LQUAN +I) -HMATRIX_DVECT(LAVER,I)
          HDI = 0.0
          DO J = 1 , VIS_DIM
            DJ = C(LQUAN+J)-HMATRIX_DVECT(LAVER,J)
            DHVIS = HMATRIX_DARRAY(LHVIS,I,J,VIS_DIM,VIS_DIM)
            HDI = HDI + DHVIS*DJ
            CHISQ = CHISQ + DHVIS*DI*DJ
          ENDDO
          IF ( DO_CHIS_VECTOR ) THEN
          C(LCHIS+I) = HDI*DI
          ENDIF
        ENDDO
      PROBL = PROB(CHISQ,VIS_DIM)
      C(LHMTR+3) = CHISQ
      C(LHMTR+4) = VIS_DIM
      C(LHMTR+5) = PROBL
      ELSE
        CALL ERRMSG('HMATRIX','HMATRIX_CHISQUARED',
     &    'CANNOT BE CALLED DURING ACCUMULATE PHASE ','W')
      ENDIF
  999 RETURN
      END
