      REAL FUNCTION QCD_CUT_EFF(CONE,ZVERT,ET,ETA,ETLEAD,MISS_FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculates the Efficiency of the Jet Standard 
C-                         Cuts (applied in a jet by jet basis) as a 
C-                         function of jet ET and ETA. As an option, 
C-                         the returned value also includes the efficiency 
C-                         of the Missing ET Cut (applied in an event by 
C-                         event basis).
C-
C-   Returned value  : QCD_CUT_EFF
C-   Inputs  : CONE [R]  : cone size of the algorithm used
C-             ZVERT [R] : z-vertex position of the event that includes the
C-                         jet of interest
C-             ET [R]    : ET of the jet
C-             ETA [R]   : physics ETA of the jet
C-             ETLEAD [R]: ET of the leading jet of the event that includes the
C-                         jet of interest
C-             MISS_FLAG [L]: if TRUE, QCD_CUT_EFF included Miss ET Cut Effic.
C-   Outputs : 
C-   Controls: 
C-
C-   Created   2-MAR-1994  Victor Daniel Elvira. Based on parameters 
C-                         calculated by Mrinmoy Bhattacharjee in D0-note 2197.
C-                         See D0-notes 1662, 1763 and 2197 for additional
C-                         information on Jet Standard Cuts and Efficiencies.  
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C-
      REAL CONE,ET,ETA,DETA,ETLEAD,ZVERT
      REAL ETABIN(3),ETTH(6)
      REAL CT1(3),CT2(2),CT3(2),IC1(3),IC2(3),IC3(2),FW1(3),FW2(2)
      REAL MET1(3),MET2(2)
      REAL PETA_TO_DETA
C-
      LOGICAL MISS_FLAG
C-----------------------------------------------------------------------
      DATA ETABIN/0.,1.,1.6/
      DATA ETTH/50.,115.,35.,90.,70.,90./
      DATA CT1/97.58,42.43,191.78/
      DATA CT2/98.45,-0.01833/
      DATA CT3/96.53,-0.00264/
      DATA IC1/88.53,0.455,-0.0062/
      DATA IC2/96.45,0.0218,-0.0003/
      DATA IC3/99.57,-0.04/
      DATA FW1/96.9,0.0294,-0.00031/
      DATA FW2/103.3,-0.085/
      DATA MET1/98.545,0.028,-0.000162/
      DATA MET2/100.08,-0.00331/
C-----------------------------------------------------------------------
C-
C-    Parameters for cone size=0.7     
C-
      IF (CONE.NE.0.7) THEN
        CALL ERRMSG('ERROR','QCD_CUT_EFF',
     &   'This cone size is not implemented','F')
        GOTO 999
      ENDIF
C-----------------------------------------------------------------------
      DETA=PETA_TO_DETA(ETA,ZVERT)
C-----------------------------------------------------------------------
      IF ((ABS(DETA).GT.ETABIN(1)).AND.(ABS(DETA).LT.ETABIN(2))) THEN
        IF (ET.LT.ETTH(1)) THEN
          QCD_CUT_EFF=CT1(1)*EXP(-0.5*((ET-CT1(2))**2/CT1(3)**2))
        ELSE 
          IF (ET.LT.ETTH(2)) THEN
            QCD_CUT_EFF=CT2(1)+CT2(2)*ET
          ELSE
            QCD_CUT_EFF=CT3(1)+CT3(2)*ET
          ENDIF
        ENDIF
      ELSE IF ((ABS(DETA).GT.ETABIN(2)).AND.(ABS(DETA).LT.ETABIN(3)))
     &  THEN
        IF (ET.LT.ETTH(3)) THEN
          QCD_CUT_EFF=IC1(1)+IC1(2)*ET+IC1(3)*ET**2
        ELSE IF (ET.LT.ETTH(4)) THEN
          QCD_CUT_EFF=IC2(1)+IC2(2)*ET+IC2(3)*ET**2
        ELSE
          QCD_CUT_EFF=IC3(1)+IC3(2)*ET
        ENDIF
      ELSE
        IF (ET.LT.ETTH(5)) THEN
          QCD_CUT_EFF=FW1(1)+FW1(2)*ET+FW1(3)*ET**2
        ELSE
          QCD_CUT_EFF=FW2(1)+FW2(2)*ET
        ENDIF
      ENDIF
C-----------------------------------------------------------------------
      QCD_CUT_EFF=QCD_CUT_EFF/100.
C-----------------------------------------------------------------------
      IF (MISS_FLAG) THEN
        IF (ETLEAD.LT.ETTH(6)) THEN
          QCD_CUT_EFF=0.01*(MET1(1)+MET1(2)*ET+MET1(3)*ET**2)
     &      *QCD_CUT_EFF
        ELSE
          QCD_CUT_EFF=0.01*(MET2(1)+MET2(2)*ET)*QCD_CUT_EFF
        ENDIF
      ENDIF
C-----------------------------------------------------------------------
  999 RETURN
      END
