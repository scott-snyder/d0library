      SUBROUTINE CHSCALE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Scales H matrix and error matrix elements
C-                         to observed energy.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  10-AUG-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CHMATR.INC'
      INTEGER I,J,NVLYR,IET,IPH,ILYR
      REAL    EVIS,FACT,FACTL
      INTEGER IX
C----------------------------------------------------------------------
      IX(IET,IPH,ILYR) = 1 + (IET-NETLO) + (IPH-NPHLO)*NETTOT +
     &  (ILYR-1)*NETTOT*NPHTOT                ! INDEX STATEMENT FUNCTION.
C
C
C ****  the philosophy here is as follows. Error matrices contain
C ****  energy variances. So they scale with energy. (Sigam**2/E**2 = 1/E.
C ****  so, Sigma**2 = E).  Hmatrices go as 1/E since they are the inverse.
C ****  At a future date, we will actually extrapolate between various energies
C
C
C ****  NOW TO SCALE LONGITUDINAL MATRIX
C
      EVIS = 0.
      DO 50 I = 1,NDIML-3
        EVIS = EVIS + QUANTL(I)
   50 CONTINUE
      FACT = EVIS/BEAMEN
C
      DO 200 I = 1,NDIML
        AVERL(I) = AVERL(I)*FACT        ! SCALING UP AVERAGES.
        DO 200 J = 1,NDIML
          EMATRL(I,J) = EMATRL(I,J)*FACT
          HMATRL(I,J) = HMATRL(I,J)/FACT
  200 CONTINUE
C
C ****  NOW FOR FULL MATRIX
C
      NVLYR = NLYRH - 3                 ! 7 LAYERS not including FH
      EVIS = 0.
      DO 100 IET = -NET,NET
        DO 100 IPH = -NPH,NPH
          DO 100 ILYR = 1,NVLYR
            EVIS = QUAN(IX(IET,IPH,ILYR)) + EVIS
  100 CONTINUE
      FACTL = FACT
      FACT = EVIS/BEAMEN
      IF(ABS(FACT-FACTL).GT.0.01)THEN
        CALL ERRMSG('CALORIMETER','CHSCALE',
     &    'FACT NOT EQ FACTL ','W')
      ENDIF
C
      DO 300 I = 1,NDIMH
        AVR(I) = AVR(I)*FACT            ! SCALING UP AVERAGES.
        DO 300 J = 1,NDIMH
          EMAT(I,J) = EMAT(I,J)*FACT
          HMAT(I,J) = HMAT(I,J)/FACT
  300 CONTINUE
C
      BEAMEN = EVIS             ! To scale up next.
C
  999 RETURN
      END
