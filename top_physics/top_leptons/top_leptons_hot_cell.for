      SUBROUTINE TOP_LEPTONS_HOT_CELL(LJETS,IKILL,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Flags Hot Cell 'Jets'
C-
C-   Inputs  : 
C-             LJETS - current LJETS Bank pointer
C-   Outputs : 
C-             IKILL(5) :
C-                     1 : 0/-1  Ratio Hottest/Next Hottest cell OK/Bad
C-                     2 : 0/-1  CH fraction OK/Bad
C-                     3 : 0/-1  EM fraction OK/Bad
C-                     4 : 0/-1  ICD/MG fraction OK/Bad
C-                     5 : spare
C-              IOK   0/-1 OK/Bad (combination of above)
C-   Controls: 
C-
C-   Created  28-APR-1993   Stephen J. Wimpenny
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      LOGICAL FIRST
C
      INTEGER LJETS,IKILL(5),IOK,IER
C
      REAL HOTCELL_RAT,CH_FRAC,EM_FRAC,ICD_FRAC
C
      DATA FIRST/.TRUE./
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        IER = 0
C
C *** Get all latest parameter/Options Values
C
        CALL EZPICK('TOP_LEPTONS_RCP')
C
        CALL EZGET('HOTCELL_RAT_CUT',HOTCELL_RAT,IER)
        IF(IER.EQ.0) CALL EZGET('CH_FRAC_CUT',CH_FRAC,IER)
        IF(IER.EQ.0) CALL EZGET('EM_FRAC_CUT',EM_FRAC,IER)
        IF(IER.EQ.0) CALL EZGET('ICD_FRAC_CUT',ICD_FRAC,IER)
C
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_HOT_CELL',' ','F')
        CALL EZRSET
      ENDIF
C
      CALL VZERO(IKILL,5)
C
      IOK=0
C
C *** test ratio of hottest to next-to-hottest cell
C
      IF(Q(LJETS+19).GT.HOTCELL_RAT) THEN
        IKILL(1)=-1
        IOK=-1
      ENDIF
C
C *** test CH fraction
C
      IF(Q(LJETS+18).GT.CH_FRAC) THEN
        IKILL(2)=-1
        IOK=-1  
      ENDIF
C
C *** test EM fraction
C
      IF(Q(LJETS+14).LT.EM_FRAC) THEN
        IKILL(3)=-1
        IOK=-1
      ENDIF
C
C *** test ICD fraction
C
      IF(Q(LJETS+17).GT.ICD_FRAC) THEN
        IKILL(4)=-1
        IOK=-1
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
