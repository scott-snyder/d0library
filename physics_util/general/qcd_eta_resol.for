      REAL FUNCTION QCD_ETA_RESOL(CONE,ENER,ETA,ZVERT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : QCD_ETA_RESOL
C-   Inputs  : CONE [R] : cone size of the algorithm used (only 0.7
C-                        implemented)
C-             ENER [R] : uncorrected jet energy
C-             ETA  [R] : calorimeter physics eta
C-             ZVERT [R]: z-vertex position
C-            
C-   Outputs : 
C-   Controls: 
C-
C-   Created   6-April-1994  Victor Daniel Elvira. Based on Herwig 
C-                           Monte Carlo study
C-            10-June-1994   V.D.Elvira. If ABS_DETA.gt.3, returns
C-                           resolution for DETA=3.
C----------------------------------------------------------------------
      IMPLICIT NONE
C-
      REAL CONE,ENER
      REAL PETA_TO_DETA,ETA,DETA,ABS_DETA,ZVERT
      REAL ETATH(6,6)
      REAL ETH(5)
      REAL CT1(6),CT2(6),CT3(6)
      REAL RES1,RES2,RES,SLOPE
C-
      INTEGER I,J
C-
C-----------------------------------------------------------------------
      DATA ETH/40.,70.,100.,200.,300./
      DATA ETATH/0.24,0.25,0.25,0.24,0.24,0.25,0.74,0.75,0.74,0.75,
     &  0.71,0.71,1.23,1.23,1.24,1.24,1.23,1.20,1.72,1.74,1.
     &  74,1.74,1.75,1.73,2.11,2.23,2.24,2.24,2.25,2.23,2.62,
     &  2.62,2.71,2.72,2.73,2.74/
C-----------------------------------------------------------------------
      DATA CT1/0.00574,0.0039,0.00515,0.00367,0.00112,0.000808/
      DATA CT2/0.82,1.19,1.74,2.42,4.9,8.08/
      DATA CT3/-0.96,-3.86,-10.98,-17.1,-100.3,-248.9/
C-----------------------------------------------------------------------
C-
C-    Parameters for cone size=0.7     
C-
      IF (CONE.NE.0.7) THEN
        CALL ERRMSG('ERROR','QCD_CUT_RES',
     &   'This cone size is not implemented','F')
        GOTO 999
      ENDIF
C-----------------------------------------------------------------------
      DETA=PETA_TO_DETA(ETA,ZVERT)
      ABS_DETA=ABS(DETA)
      IF (ABS_DETA.GE.3.) ABS_DETA=2.99
C-----------------------------------------------------------------------
C
      IF (ENER.LT.ETH(1)) THEN 
C          
        IF (ABS_DETA.LT.ETATH(1,1)) THEN  
          RES=CT1(1)+CT2(1)/ENER+CT3(1)/ENER**2
          GOTO 10
        ENDIF
C
        DO J=1,5
          IF ((ABS_DETA.GE.ETATH(1,J)).AND.(ABS_DETA.LT.ETATH(1,J+1)))
     &      THEN
            RES1=CT1(J)+CT2(J)/ENER+CT3(J)/ENER**2
            RES2=CT1(J+1)+CT2(J+1)/ENER+CT3(J+1)/ENER**2
            SLOPE=(RES2-RES1)/(ETATH(1,J+1)-ETATH(1,J))
            RES=SLOPE*(ABS_DETA-ETATH(1,J))+RES1
            GOTO 10
          ENDIF
        ENDDO
C
        IF (ABS_DETA.GE.ETATH(1,6)) THEN  
          RES=CT1(6)+CT2(6)/ENER+CT3(6)/ENER**2
          GOTO 10
        ENDIF
C
      ENDIF   
C
      DO I=1,4
        IF ((ENER.GE.ETH(I)).AND.(ENER.LT.ETH(I+1))) THEN 
C          
          IF (ABS_DETA.LT.ETATH(I+1,1)) THEN  
            RES=CT1(1)+CT2(1)/ENER+CT3(1)/ENER**2
            GOTO 10
          ENDIF
C
          DO J=1,5
            IF ((ABS_DETA.GE.ETATH(I+1,J)).AND.(ABS_DETA.LT.ETATH(I+1,
     &        J+1))) THEN
              RES1=CT1(J)+CT2(J)/ENER+CT3(J)/ENER**2
              RES2=CT1(J+1)+CT2(J+1)/ENER+CT3(J+1)/ENER**2
              SLOPE=(RES2-RES1)/(ETATH(I+1,J+1)-ETATH(I+1,J))
              RES=SLOPE*(ABS_DETA-ETATH(I+1,J))+RES1
              GOTO 10
            ENDIF
          ENDDO
C
          IF (ABS_DETA.GE.ETATH(I+1,6)) THEN  
            RES=CT1(6)+CT2(6)/ENER+CT3(6)/ENER**2
            GOTO 10
          ENDIF
C
        ENDIF
      ENDDO
C   
      IF (ENER.GE.ETH(5)) THEN 
C          
        IF (ABS_DETA.LT.ETATH(6,1)) THEN  
          RES=CT1(1)+CT2(1)/ENER+CT3(1)/ENER**2
          GOTO 10
        ENDIF
C
        DO J=1,5
          IF ((ABS_DETA.GE.ETATH(6,J)).AND.(ABS_DETA.LT.ETATH(6,J+1)))
     &      THEN
            RES1=CT1(J)+CT2(J)/ENER+CT3(J)/ENER**2
            RES2=CT1(J+1)+CT2(J+1)/ENER+CT3(J+1)/ENER**2
            SLOPE=(RES2-RES1)/(ETATH(6,J+1)-ETATH(6,J))
            RES=SLOPE*(ABS_DETA-ETATH(6,J))+RES1
            GOTO 10
          ENDIF
        ENDDO
C
        IF (ABS_DETA.GE.ETATH(6,6)) THEN  
          RES=CT1(6)+CT2(6)/ENER+CT3(6)/ENER**2
          GOTO 10
        ENDIF
C
      ENDIF   
C
   10 QCD_ETA_RESOL=RES
C-----------------------------------------------------------------------
  999 RETURN
      END
