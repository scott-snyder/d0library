      SUBROUTINE CALC_NONGAUSS_LIKELY(DIAG,NDIM,SUBDIR,CHSQ_NONGAUSS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate non-gaussian likelihhods using
C-   Diagonalized histograms provided in file Hmatrix_diag.
C-
C-   Inputs  : DIAG = dioagonalized vector
C-             NDIM = Dimensions of vector
C-             SUBDIR = Subdirecotry in hmatrix_diag where the likelihhod curves
C-             are to be found
C-   Outputs : CHSQ_NONGAUSS = Non Gaussian Chisquared
C-   Controls: 
C-
C-   Created  16-FEB-1995   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    DIAG(*)
      INTEGER NDIM
      REAL    CHSQ_NONGAUSS
      CHARACTER*(*) SUBDIR
      CHARACTER*64 PATH
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER IER
      INTEGER I
      REAL    TOT
      CHARACTER*64 TIT
      INTEGER NX,NY,LOC,NWT
      REAL    XMI,XMA,YMI,YMA,BIN,LIKE
      REAL    HSUM,HX,ORD,CHSQ
      REAL    CHICOMP(5)
C----------------------------------------------------------------------
      CALL DHDIR_SAVE_FILE            !SAVE PREVIOUS TOPDIR
      CALL DHDIR_DECLARE_FILE('HMATRIX_DIAG')
      PATH = '//'//'HMATRIX_DIAG'//'/'//SUBDIR
C      CALL DHDIRECTORY(' ',SUBDIR,IER,'HMATRIX_DIAG ')
      CALL DHDIRECTORY(' ',PATH,IER,'HMATRIX_DIAG ')
C         ! Create/Set HBOOK directory
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('LJTOP_HMATRIX','CALC_NONGAUSS_LIKELY',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
      DO I = 1 , NDIM
        CALL HRIN(I,0,0)
      ENDDO
C
C      CALL HLDIR(' ','T')
      CHSQ_NONGAUSS=0.0
      DO I = 1 , NDIM
        CALL HGIVE(I,TIT,NX,XMI,XMA,NY,YMI,YMA,NWT,LOC)
        TOT = HSUM(I)
        ORD = HX(I,DIAG(I))
        BIN = (XMA-XMI)/NX
        LIKE = ORD/(BIN*TOT)
        LIKE = LIKE*SQRT(TWOPI)  !UNIT SIGMA
        IF ( LIKE.EQ.0.0 ) THEN
          CHSQ = XMA**2
        ELSE
          CHSQ = -2.0*ALOG(LIKE)  !EQUIVALENT CHSQ
        ENDIF
        CHICOMP(I) = CHSQ
        CHSQ_NONGAUSS = CHSQ_NONGAUSS + CHSQ
      ENDDO
C
      DO I = 1 , NDIM
        CALL HDELET(I)
      ENDDO
      CALL DHDIR_RESTORE_FILE
C
  999 RETURN
      END
