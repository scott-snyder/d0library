      SUBROUTINE DO_EISRS1
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FIND EIGEN VALUES AND EIGEN VECTORS OF
C-                         H matrices .
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  14-SEP-1989   Rajendran Raja
C-   Updated  15-DEC-1989   N.A. Graf  Switched matrix indices and fixed
C-                          bug in HBOOK calls.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CHMATR.INC'
      REAL  SHMAT_VIS(NDIMVFR,NDIMVFR)
      REAL    SHMAT_EIGENVAL(NDIMVFR)
      REAL    SHMAT_EIGENVEC(NDIMVFR,NDIMVFR)
      REAL    WORK(NDIMVFR,NDIMVFR)
C
      REAL SHMATRL_VIS(NDIMVL,NDIMVL)
      REAL    SHMATRL_EIGENVAL(NDIMVL)
      REAL    SHMATRL_EIGENVEC(NDIMVL,NDIMVL)
C
      INTEGER IER
      INTEGER I,J
      REAL    YDUM
C----------------------------------------------------------------------
C
      DO J = 1,NDIMVFR
        DO I = 1,NDIMVFR
          SHMAT_VIS(I,J) = HMAT_VIS(I,J)
        ENDDO
      ENDDO
C
      DO J = 1,NDIMVL
        DO I = 1,NDIMVL
          SHMATRL_VIS(I,J) = HMATRL_VIS(I,J)
        ENDDO
      ENDDO
C
      CALL EISRS1(NDIMVFR,NDIMVFR,SHMAT_VIS,SHMAT_EIGENVAL,
     &        SHMAT_EIGENVEC,IER,WORK)
C
      CALL EISRS1(NDIMVL,NDIMVL,SHMATRL_VIS,SHMATRL_EIGENVAL,
     &        SHMATRL_EIGENVEC,IER,WORK)
C
      CALL HBOOK1(1001,'EIGENVALUES FOR LONGITUDINAL MATRIX',NDIMVL,0.5,
     &  FLOAT(NDIMVL)+0.5,0.)
C
      CALL HBOOK1(1002,'EIGENVALUES FOR TRANSVERSE MATRIX',NDIMVFR,0.5,
     &  FLOAT(NDIMVFR)+0.5,0.)
C
C
      DO I = 1 , NDIMVL
        CALL HFILL(1001,FLOAT(I),YDUM,SHMATRL_EIGENVAL(I))
      ENDDO
C
      DO I = 1 , NDIMVFR
        CALL HFILL(1002,FLOAT(I),YDUM,SHMAT_EIGENVAL(I))
      ENDDO
C
  999 RETURN
      END
