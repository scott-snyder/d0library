      SUBROUTINE QCD_ETA_BIAS(CONE,ZVERT,ENER,ETA_IN,ETA_OUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : QCD_ETA_BIAS
C-   Inputs  : CONE [R]   : cone size of the algorithm used (only 0.7
C-                         implemented)
C-             ENER [R]   : uncorrected jet energy
C-             ETA_IN [R] : calorimeter physics eta
C-             ETA_OUT [R]: corrected physics eta
C-            
C-   Outputs : 
C-   Controls: 
C-
C-   Created   6-April-1994  Victor Daniel Elvira. Based on Monte Carlo 
C-                           study (HERWIG)
C-            10-June-1994   V.D.Elvira. If ABS_DETA.gt.3, returns 
C-                           correction for DETA=3.
C----------------------------------------------------------------------
      IMPLICIT NONE
C-
      REAL CONE,ENER,ETA_IN,ETA_OUT,ZVERT
      REAL ETH(6,6),ETATH(6)
      REAL ETABIN(7),CT1(6),CT2(6),CT3(6),CT4(6),CT5(6),CT6(6),CT7(6)
      REAL PETA_TO_DETA,DETA_TO_PETA,DETA,ABS_DETA
      REAL COR1,COR2,COR,SLOPE
C-
      INTEGER I,J
C-
C-----------------------------------------------------------------------
      DATA ETABIN/0.,0.5,1.,1.5,2.,2.5,3./
      DATA ETATH/1.75,1.75,1.75,1.75,1.75,1.9/
C-----------------------------------------------------------------------
      DATA ETH/30.,30.,29.,31.,36.,36.,54.,53.,54.,54.,55.,62.,83.,85.,
     &  84.,84.,84.,84.,150.,139.,140.,141.,142.,145.,251.,249.,243.,
     &  239.,242.,243.,338.,363.,402.,380.,365.,371./
C-----------------------------------------------------------------------
      DATA CT1/0.003,0.00161,0.000469,0.000465,-0.000952,-0.000939/
      DATA CT2/-0.0214,-0.00109,-0.00356,0.00737,0.00685,0.0162/
      DATA CT3/0.0791,0.0426,0.0385,0.00953,0.0101,-0.00858/
      DATA CT4/-0.0359,-0.0224,-0.0205,-0.00722,-0.00765,0.000533/
      DATA CT5/-0.0591,-0.0717,0.0464,0.00553,0.0123,0.00592/
      DATA CT6/0.0436,0.047,-0.077,-0.0257,-0.0266,-0.0188/
      DATA CT7/0.,0.,0.0295,0.0141,0.0117,0.00888/
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
      DETA=PETA_TO_DETA(ETA_IN,ZVERT)
      ABS_DETA=ABS(DETA)
      IF (ABS_DETA.GE.3.) ABS_DETA=2.99
C-----------------------------------------------------------------------
      DO I=1,6
C          
        IF ((ABS_DETA.GT.ETABIN(I)).AND.(ABS_DETA.LT.ETABIN(I+1)))
     &    THEN
C
          IF (ENER.LE.ETH(I,1)) THEN
            IF(ABS_DETA.LT.ETATH(1)) THEN  
              COR=CT1(1)+CT2(1)*ABS_DETA+CT3(1)*ABS_DETA**2+CT4(1)
     &          *ABS_DETA**3
            ELSE
              COR=CT5(1)+CT6(1)*ABS_DETA+CT7(1)*ABS_DETA**2
            ENDIF
            IF (DETA.LT.0.) COR=-COR
            GOTO 10
          ENDIF
C
          DO J=1,5
            IF ((ENER.GE.ETH(I,J)).AND.(ENER.LT.ETH(I,J+1))) THEN
              IF(ABS_DETA.LT.ETATH(J)) THEN  
                COR1=CT1(J)+CT2(J)*ABS_DETA+CT3(J)*ABS_DETA**2+CT4(J)
     &            *ABS_DETA**3
                COR2=CT1(J+1)+CT2(J+1)*ABS_DETA+CT3(J+1)
     &            *ABS_DETA**2+CT4(J+1)*ABS_DETA**3
              ELSE
                COR1=CT5(J)+CT6(J)*ABS_DETA+CT7(J)*ABS_DETA**2
                COR2=CT5(J+1)+CT6(J+1)*ABS_DETA+CT7(J+1)*ABS_DETA**2
              ENDIF
              SLOPE=(COR2-COR1)/(ETH(I,J+1)-ETH(I,J))
              COR=SLOPE*(ENER-ETH(I,J))+COR1
              IF (DETA.LT.0.) COR=-COR
	      GOTO 10
            ENDIF
          ENDDO
C
          IF (ENER.GE.ETH(I,6)) THEN
            IF(ABS_DETA.LT.ETATH(6)) THEN  
              COR=CT1(6)+CT2(6)*ABS_DETA+CT3(6)*ABS_DETA**2+CT4(6)
     &          *ABS_DETA**3
            ELSE
              COR=CT5(6)+CT6(6)*ABS_DETA+CT7(6)*ABS_DETA**2
            ENDIF
            IF (DETA.LT.0.) COR=-COR
	    GOTO 10
          ENDIF
C
   10     DETA=DETA+COR
          ETA_OUT=DETA_TO_PETA(DETA,ZVERT)
C          
        ENDIF
      ENDDO
C-----------------------------------------------------------------------
  999 RETURN
      END
