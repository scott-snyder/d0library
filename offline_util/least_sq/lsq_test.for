      PROGRAM LSQ_TEST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TEST THE LEAST SQUARES PACKAGE
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  22-FEB-1992   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL OK
      LOGICAL LSQ_INI,LSQ_EVENT,LSQ_FIN,LSQ_EVENT_LAMBDA2
      LOGICAL LSQ_EVENT_LAMBDA,LSQ_EVENT_LAMBDA1,LSQ_EVENT_LAMBDA3
      LOGICAL DO_LAMBDA,DO_LAMBDA1,DO_LAMBDA2,DO_LAMBDA3
      INTEGER IER
C----------------------------------------------------------------------
      OK = LSQ_INI()

      CALL EZPICK('LSQ_RCP')
      CALL EZGET('DO_LAMBDA',DO_LAMBDA,IER)
      CALL EZGET('DO_LAMBDA1',DO_LAMBDA1,IER)
      CALL EZGET('DO_LAMBDA2',DO_LAMBDA2,IER)
      CALL EZGET('DO_LAMBDA3',DO_LAMBDA3,IER)
C
      IF(DO_LAMBDA)OK = LSQ_EVENT_LAMBDA()
      IF(DO_LAMBDA1)OK = LSQ_EVENT_LAMBDA1()
      IF(DO_LAMBDA2)OK = LSQ_EVENT_LAMBDA2()
      IF(DO_LAMBDA3)OK = LSQ_EVENT_LAMBDA3()
C
      OK = LSQ_FIN()
C
      END
