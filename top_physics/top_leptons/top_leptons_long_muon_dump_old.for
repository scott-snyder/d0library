      SUBROUTINE TOP_LEPTONS_LONG_MUON_DUMP_OLD(LPMUO,LUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Produce Detailed Diagnostic Dump of Muon
C-                         track information. Routine is called from
C-                         TOP_LEPTONS_EVENT_SUMMRY.
C-                         This version is for PMUO Bank vesrions 1
C-                         and 2 ONLY.
C-
C-   Inputs  : 
C-                PMUO Bank Versions 1 and 2
C-   Outputs : 
C-                Printout on unit LUN
C-   Controls: 
C-                None
C-
C-   Created  26-JAN-1993   Stephen J. Wimpenny
C-   Modifed  10-Feb-1993   IFW1 decoding updated
C-   Modified 17-Mar-1993   Name changed for library compatibility
C-   Modified 21-Mar-1993   Name change to routines Decode_Ifw2,
C-                          Caltrak_Angles, Dr_From_Deta_Dphi
C-   Modified 16-Jun-1993   LUN 16 Reco dump switched by RCP file
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      EXTERNAL TOP_LEPTONS_UTIL_CALC_DR
C
      LOGICAL FIRST,DO_RECO_DUMP
C
      INTEGER LPMUO,LMUOT,LZTRK,LDTRK,LFDCT,LVTXT,LMUON,LMUCD
      INTEGER LUN,IER,ITEMP,IV
      INTEGER NO_IFW2_BITS,IFW2_BITS(32)
      INTEGER NORUN,NOEVT,GZHEAD
C
      REAL TEMP,PI,TWOPI
      REAL RZ_IMPACT,XY_IMPACT
      REAL ZTRAK_PHI,ZTRAK_THETA,ZTRAK_ETA
      REAL DETA_VEC12,DPHI_VEC12,DR_VEC12,TOP_LEPTONS_UTIL_CALC_DR
C
      DATA PI,TWOPI/3.141593,6.283185/
      DATA FIRST/.TRUE./
C
      IF(FIRST) THEN
        IER=0
C
C *** Get all latest parameter/Options Values
C
        CALL EZPICK('TOP_LEPTONS_RCP')
        CALL EZGET('DO_RECO_DIAGNOSTICS',DO_RECO_DUMP,IER)
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_LONG_MUON_DUMP',' ','F')
        CALL EZRSET
        FIRST=.FALSE.
      ENDIF
C
C *** Unpack IFW2 bit pattern
C
      CALL TOP_LEPTONS_UTIL_DECODE_IFW2(LPMUO,NO_IFW2_BITS,
     1  IFW2_BITS,IER)
      IF(NO_IFW2_BITS.GT.0) WRITE(LUN,1000) (IFW2_BITS(IV),IV=1,
     1  NO_IFW2_BITS)
C
C *** IFW4 + Quadrant No.
C
      WRITE(LUN,1010) IQ(LPMUO+9),IQ(LPMUO+7)
C
C *** Hits/track and bend+non-bend impact parameters (using MUOT)
C
      LMUOT=LQ(LPMUO-2)
      RZ_IMPACT=0.
      XY_IMPACT=0.
      IF(LMUOT.GT.0) THEN
C
C *** MUON diagnostic output for reco studies
C
        IF(DO_RECO_DUMP) THEN
          LHEAD=GZHEAD()
          IF(LHEAD.NE.0) THEN
            NORUN=IQ(LHEAD+6)
            NOEVT=IQ(LHEAD+9)
          ENDIF
          ITEMP=IQ(LPMUO+2)
          IF(ITEMP.LT.0) THEN
            ITEMP=1
          ELSE
            ITEMP=-1
          ENDIF
          WRITE(16,5000) NORUN,NOEVT,IQ(LPMUO+7),Q(LPMUO+13),
     1      Q(LPMUO+14),Q(LPMUO+41),Q(LPMUO+38),Q(LPMUO+39),
     2      IQ(LMUOT+1),Q(LMUOT+20),Q(LMUOT+21),ITEMP
        ENDIF
C
        TEMP=SQRT(Q(LMUOT+17)**2+Q(LMUOT+18)**2)
        IF(TEMP.GT.0) THEN
          XY_IMPACT=(Q(LMUOT+11)*Q(LMUOT+18)-Q(LMUOT+12)*
     1      Q(LMUOT+17))/TEMP
        ENDIF
        ITEMP=IQ(LMUOT+4)
        IF(ITEMP.GE.10) THEN
          ITEMP=ITEMP-10
          WRITE(LUN,1021) ITEMP,IQ(LMUOT+6),Q(LMUOT+22),IQ(LMUOT+1),
     1      IQ(LMUOT+2),Q(LMUOT+20),Q(LMUOT+21)
        ELSE
          WRITE(LUN,1020) ITEMP,IQ(LMUOT+6),Q(LMUOT+22),IQ(LMUOT+1),
     1      IQ(LMUOT+2),Q(LMUOT+20),Q(LMUOT+21)
        ENDIF
      ENDIF
C
C *** trackfit chisq, floating t0, 3-D impact param, bend + non-bend impact
C
      WRITE(LUN,1030) Q(LPMUO+23),Q(LPMUO+24),Q(LPMUO+41),RZ_IMPACT,
     1  XY_IMPACT
C
C *** Matching Central Tracking Information :
C
      WRITE(LUN,1040) IQ(LPMUO+6),Q(LPMUO+40)
C
C *** best track match and dE/dx using Central tracking
C
      LDTRK=0
      LFDCT=0
      LVTXT=0
      LZTRK=LQ(LPMUO-5)
      IF(LZTRK.GT.0) THEN
        LDTRK=LQ(LZTRK-7)
        LFDCT=LQ(LZTRK-8)
        LVTXT=LQ(LZTRK-6)
        CALL TOP_LEPTONS_UTIL_CALTRAK_ANGLES(LZTRK,ZTRAK_THETA,
     1    ZTRAK_PHI,ZTRAK_ETA)
        DETA_VEC12=Q(LPMUO+16)-ZTRAK_ETA
        DPHI_VEC12=Q(LPMUO+17)-ZTRAK_PHI
        IF(DPHI_VEC12.GT.PI) DPHI_VEC12=TWOPI-DPHI_VEC12
        DR_VEC12=TOP_LEPTONS_UTIL_CALC_DR(DETA_VEC12,DPHI_VEC12)
        WRITE(LUN,1050) ZTRAK_THETA,ZTRAK_PHI,ZTRAK_ETA,DR_VEC12
C
C *** dE/dx from tracking chambers
C
        IF(LDTRK.GT.0) WRITE(LUN,1051) Q(LDTRK+20)
        IF(LFDCT.GT.0) WRITE(LUN,1052) Q(LFDCT+20)
        IF(LVTXT.GT.0) WRITE(LUN,1053) Q(LVTXT+20)
      ENDIF
C
C *** all track matches - using (MUON and) MUCD Banks
C
      IF(IQ(LPMUO+6).GT.1) THEN
        LMUON=LQ(LPMUO-3)
        IF(LMUON.GT.0) THEN
          LMUCD=LQ(LMUON-5)
          DO WHILE (LMUCD.GT.0)
            LZTRK=LQ(LMUCD-1)
            IF(LZTRK.GT.0) THEN
              CALL TOP_LEPTONS_UTIL_CALTRAK_ANGLES(LZTRK,
     1          ZTRAK_THETA,ZTRAK_PHI,ZTRAK_ETA)
              DETA_VEC12=Q(LPMUO+16)-ZTRAK_ETA
              DPHI_VEC12=Q(LPMUO+17)-ZTRAK_PHI
              IF(DPHI_VEC12.GT.PI) DPHI_VEC12=TWOPI-DPHI_VEC12
              DR_VEC12=TOP_LEPTONS_UTIL_CALC_DR(DETA_VEC12,DPHI_VEC12)
              WRITE(LUN,1054) DR_VEC12,IQ(LMUCD+5),ZTRAK_THETA,
     1          ZTRAK_PHI,ZTRAK_ETA
            ENDIF
            LMUCD=LQ(LMUCD)
          ENDDO
        ENDIF
      ENDIF
C
C *** Calorimeter Energy Depositions
C
      WRITE(LUN,1060) Q(LPMUO+33),Q(LPMUO+43),(Q(LPMUO+33+IV),IV=1,3)
C----------------------------------------------------------------------
  999 RETURN
C
 1000 FORMAT(5X,' Muon flag bits (word2) = ',8(I2,2X),/31X,8(I2,2X))
 1010 FORMAT(5X,' Muon flag word 4 = ',I4,3X,' Quadrant = ',I5)
 1020 FORMAT(5X,' Muon layer flag (word1) = ',I2,
     1 ' ( Vertex not used in bend-view fit ) ',/5X,
     2 ' Mixed orientation flag (word3) = ',I4,/,
     3 5X,' Integ. B.dl used for momentum = ',F8.2,/,
     4 5X,' No points used in WAMUS / SAMUS = ',2I8,/,
     5 5X,' Muon Quality of find in bend / non-bend views = ',
     6 2F8.1)
 1021 FORMAT(5X,' Muon layer flag (word1) = ',I2,
     1 ' ( Vertex used in bend-view fit ) ',/5X,
     2 ' Mixed orientation flag (word3) = ',I4,/,
     3 5X,' Integ. B.dl used for momentum = ',F8.2,/,
     4 5X,' No points used in WAMUS / SAMUS = ',2I8,/,
     5 5X,' Muon Quality of find in bend / non-bend views = ',
     6 2F8.1)
 1030 FORMAT(5X,' Trackfit Chisquare / dof = ',F8.2,
     1 2X,' Floated t0 offset = ',F8.2,' ns ',/,
     2 5X,' 3-D Impact parameter = ',F8.1,/,
     3 5X,' Bend (RZ) / Non-Bend View (XY) Impact Parameters = ',
     4 2F8.2)
 1040 FORMAT(5X,' No. matching CD tracks = ',I3,' in cone of dR = ',
     1 F8.1)
 1050 FORMAT(7X,' theta,phi,eta,dR of closest track = ',4F8.2)
 1051 FORMAT(7X,' dE/dx in CDC = ',F8.1)
 1052 FORMAT(7X,' dE/dx in FDC = ',F8.1)
 1053 FORMAT(7X,' dE/dx in VTX = ',F8.1)
 1054 FORMAT(9X,' dR = ',F4.2,' ZTRAK id = ',I4,' theta,phi,eta = ',
     1 3F8.2)
 1060 FORMAT(5X,' Predicted dEdx in Calorimeter / Muon Iron = ',2F8.1,
     1 /5X,' Calorimeter Deposition (Cone 0.2,0.4,0.6) = ',3F8.1)
C
 5000 FORMAT(2I8,I4,5F6.2,I4,2F6.2,I4)
C
      END
