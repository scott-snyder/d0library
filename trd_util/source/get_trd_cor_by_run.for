      SUBROUTINE GET_TRD_COR_BY_RUN
     &  (RCP_FILE_NAME,RCP_BANK_NAME,RCP_ARRAY_NAME,
     &   RUN_DEPENDENT_X,X_DIM,RUN_LIMITS,N_ZONES)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns an array of the run number dependent
C-                         variable X
C-
C-   Inputs  : RCP_FILE_NAME   character(*) RCP file name (e.g. 'TRD_RCP')
C-             RCP_BANK_NAME   character(*) RCP bank name (e.g. 'TRD_RCP')
C-             RCP_ARRAY_NAME  character(*) RCP array name (e.g. 'URANIUM')
C-             X_DIM           integer      dimension of variable X
C-                                          (e.g. 1 for general variable like
C-                                          gas reference, or 3 for layer
C-                                          dependent variable like uranium
C-                                          peak positions)
C-   Outputs : RUN_DEPENDENT_X real(*)      RUN_DEPENDENT_X(i,j)=value of
C-                                          X(i) for time period j
C-             RUN_LIMITS      integer(*)   RUN_LIMITS(1,j)=run number mini
C-                                          for time period j
C-                                          RUN_LIMITS(2,j)=run number maxi
C-                                          for time period j
C-             N_ZONES         integer      number of time periods found
C-
C-   Controls: 'RCP_FILE_NAME'
C-
C-   Created  28-FEB-1994   Alain PLUQUET
C-   Updated  29-SEP-1994   Alain PLUQUET  Increased number of run zones. 
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCP_FILE_NAME,RCP_BANK_NAME,RCP_ARRAY_NAME
      INTEGER M
      PARAMETER (M=200)
      INTEGER X_DIM,I,J,K,N,IER,LOC
      INTEGER RUN_LIMITS(2,M),N_ZONES
      REAL RUN_DEPENDENT_X(X_DIM,M)
      CALL EZLOC(RCP_FILE_NAME,LOC)
      IF (LOC.LE.0) CALL INRCP (RCP_FILE_NAME,IER)
      CALL EZPICK (RCP_BANK_NAME)
      CALL EZGETA (RCP_ARRAY_NAME,0,0,0,N,IER)
      J=X_DIM+2
      N_ZONES=N/J
      DO I=1,N_ZONES
        K=(I-1)*J
        CALL EZGETA
     &    (RCP_ARRAY_NAME,K+1,K+X_DIM,1,RUN_DEPENDENT_X(1,I),IER)
        CALL EZGETA
     &    (RCP_ARRAY_NAME,K+X_DIM+1,K+X_DIM+2,1,RUN_LIMITS(1,I),IER)
      ENDDO
      CALL EZRSET
      END
