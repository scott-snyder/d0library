      SUBROUTINE D0VERI(ID1,ID2,NDO,CHOPT,RESULT,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to verify if the two histograms are "the same"
C-
C-   Inputs  : histogram IDs, ID1 is the reference histogram (the "gold" hist)
C-             CHOPT options
C-   Outputs : results of tests, error code
C-   Controls: NDO number of tests to perform (NDO should be < NTEST)
C-
C-   Created  17-DEC-1991   Krzysztof L. Genser
C-
C-   The Metodes: 
C-   
C-   The routine applies statistical tests to give the probability, that the
C-   histograms are the same.
C-   
C-   The following tests are planned: (based on Eadie at al)
C-   
C-   1. The Kolmogorov test
C-   2. The liklihood test
C-   3. The chi2 test
C-   4. The run test
C-   
C-   The routine assumes the histograms were prepared prior to it's call
C-   e.g. if the bins should be equidistant or of equel probability it should be
C-   taken care of befor calling the routine
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NTEST
      PARAMETER( NTEST = 4 )
      REAL     RESULT(NTEST)
      INTEGER ID1,ID2
      INTEGER NDO,IER
      CHARACTER*(*) CHOPT
C----------------------------------------------------------------------
      IF (NDO.GT.0) THEN
         CALL HDIFF(ID1,ID2,RESULT(1),CHOPT)
      ENDIF
C
C ****  currently only one test performed
C
      IF ( NDO.GT.1 ) THEN
         IER=2
      ENDIF

      RETURN

      END
