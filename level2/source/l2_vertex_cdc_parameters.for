      SUBROUTINE L2_VERTEX_CDC_PARAMETERS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fetch THRESHOLDs, bi-linear TABLE, and
C-                         compute weighting for leading-edge calculation
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   28-APR-93   D. Claes
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER ERR, I, J, PULTH1(2), PULTH2(2), PULTH3(2), PULMAX(2)
      INTEGER TABLE(0:255)
      LOGICAL MORETK
      REAL    BIGSGM, CDCRES, CDLTOL, COEFF(50,2), COEF2(50,2)
      REAL    DLTZR1, DLTZR2, ITRLMT, PULWEI(2)
      REAL    SGMFCT, SGMFC2, TOLDST, ZCERMX, ZSIGMA
C
      COMMON/CDPULS_PARMS/ COEFF,  COEF2, PULTH1, PULTH2, PULTH3,
     &  PULMAX, PULWEI, TABLE
      COMMON/VERTEX_CUTS/ BIGSGM, CDCRES, ITRLMT, MORETK,
     &  SGMFCT, SGMFC2, TOLDST, ZCERMX, ZSIGMA
      COMMON/DELAY_TUNING/CDLTOL, DLTZR1, DLTZR2
C
      INTEGER     NSIZE
C      PARAMETER   (NSIZE=200000)
C      PARAMETER   (NSIZE=20000)
      PARAMETER   (NSIZE=1000)
C
      LOGICAL     FIRST
      DATA        FIRST /.TRUE./
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL L2_HLIMIT(NSIZE)                ! Histogram initialization
      ENDIF
C
      CALL EZPICK('L2TRAK_RCP')              
      CALL EZGET('ZTOL', CDLTOL ,ERR)
      CALL EZRSET
C
      CALL EZPICK('L2CDHT_RCP')              
      CALL EZGET('DLTZR1',DLTZR1,ERR)
      CALL EZGET('DLTZR2',DLTZR2,ERR)        ! for L2_CDGETZ
C
      CALL EZGET('PULTH1(1)',PULTH1(1),ERR)                ! - see CD_MATCH
      CALL EZGET('PULTH2(1)',PULTH2(1),ERR)                ! - see CD_MATCH
      CALL EZGET('PULTH3(1)',PULTH3(1),ERR)                ! - see CD_MATCH
      CALL EZGET('PULMAX(1)',PULMAX(1),ERR)                ! - see CD_MATCH
      CALL EZGET('PULWEI(1)',PULWEI(1),ERR)                ! - see CD_MATCH
      CALL EZGET('TABLE(1)',TABLE(0),ERR)                  !
      CALL EZRSET
C
      DO 7 J = 1, 2                                        !
        COEFF(1,J) = 1.                                    !
        COEF2(1,J) = 1.*COEFF(1,J)                         !
        DO 4 I = 2, 50                                     !
          COEFF(I,J) = COEFF(I-1,J) * PULWEI(J)            !
          COEF2(I,J) = COEFF(I,J) * FLOAT(I)               !
    4   CONTINUE                                           !
    7 CONTINUE                                             ! For L2_CDPULS
C
      CALL EZPICK('VERTEX_RCP')              ! Scaled down L2 version
C
      CALL EZGET('ZCERMX',ZCERMX,ERR)        ! For L2_VERTEX_CDC (Needed?)
C
      CALL EZGET('TOLDST',TOLDST,ERR)        ! For L2_ZCDCGZ
      CALL EZGET('MORETK',MORETK,ERR)        ! For L2_ZCDCGZ
C
      CALL EZGET('ZSIGMA',ZSIGMA,ERR)        ! L2_ZCDCHS
      CALL EZGET('SGMFCT',SGMFCT,ERR)        ! L2_ZCDCHS
      CALL EZGET('ITRLMT',ITRLMT,ERR)        ! L2_ZCDCHS
      CALL EZGET('BIGSGM',BIGSGM,ERR)        ! L2_ZCDCHS
      CALL EZGET('SGMFC2',SGMFC2,ERR)        ! L2_ZCDCHS
      CALL EZGET('CDCRES',CDCRES,ERR)        ! L2_ZCDCHS

      CALL EZRSET
C
  999 RETURN
      END
