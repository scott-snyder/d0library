      FUNCTION CTTR_DIAL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  19-AUG-1990   Serban D. Protopopescu
C-   Updated  22-MAR-2004   sss - compile with g77
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CTTR_DIAL,NOISE,SMEAR,YES
      REAL    CUTOFF
      CHARACTER*78 MSG
      CHARACTER*4 PATH,SPATH
C----------------------------------------------------------------------
      CTTR_DIAL=.TRUE.
      YES=.TRUE.
      CALL GETPAR(1,'Add noise to trigger towers?[Y]>','L',YES)
      NOISE=YES
      YES=.TRUE.
      CALL GETPAR(-1,'Smear trigger towers?[Y]>','L',YES)
      SMEAR=YES
      CUTOFF=1.0
      CALL GETPAR(-1,'Give energy cutoff [1.0]>','R',CUTOFF)
      CALL CTTOWER_SMEAR(NOISE,SMEAR,CUTOFF)
      CALL PATHRS
      CALL PATHGT(SPATH)
      WRITE(MSG,111)SPATH
      CALL GETPAR(1,MSG(1:42),'U',PATH)
      IF(PATH.EQ.' ') PATH=SPATH
      CALL PATHDF(PATH)
      CALL PATHRS
      WRITE(MSG,113) PATH
      CALL INTMSG(MSG)
  999 RETURN
  111 FORMAT(' Chose PATH (RECO, FAKE or GEAN), [',A4,']:>')
  113 FORMAT(' Default PATH is now ',A4,'.')
      END
