      LOGICAL FUNCTION RUN1A()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Define if data belongs to run 1 A or run 1B
C-
C-   Inputs  :
C-   Outputs :True if run belongs to run 1A, False if not
C-   Controls:
C-
C-   Adapted from: TRD_NWIRE_PER_LAYER
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      RUN1A=.FALSE.
      IF(IQ(LHEAD+6).LT.68 000 .OR. IQ(LHEAD+1) .GT. 1000)
     &            RUN1A=.TRUE.
  999 RETURN
      END
 
