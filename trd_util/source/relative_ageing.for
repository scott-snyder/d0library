      REAL FUNCTION RELATIVE_AGEING
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reads TRD ageing factor (1 corresponds to no ageing)
C-
C-   Returned value  :
C-   Inputs  : none
C-   Outputs : ageing factor 
C-   Controls: TRD.RCP
C-
C-   Created   4-MAY-1994   Alain PLUQUET
C-   Updated  29-SEP-1994   Alain PLUQUET  Run 1B 'ageing' from TRD.RCP (1) 
C-                                         Increased number of run zones.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER M,IER,R,LOC
      PARAMETER (M=200)
      INTEGER RUN_LIMITS(2,M),N_ZONES,RUN_NUMBER_ZONE,RUNNO
      REAL AGEING(M)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZLOC('TRD_RCP',LOC)
        IF (LOC.LE.0) CALL INRCP('TRD_RCP',IER)
        CALL EZPICK ('TRD_RCP')
        CALL GET_TRD_COR_BY_RUN
     &  ('TRD_RCP','TRD_RCP','AGEING',AGEING,1,RUN_LIMITS,N_ZONES)
        CALL EZRSET
      END IF
      R=RUN_NUMBER_ZONE(RUN_LIMITS,RUNNO())
      RELATIVE_AGEING=AGEING(R)
      END
