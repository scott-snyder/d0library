      SUBROUTINE TOP_LEPTONS_LONG_MUON_DUMP(LPMUO,LUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Produce Detailed Diagnostic Dump of Muon
C-                         track information. Routine is called from
C-                         TOP_LEPTONS_EVENT_SUMMRY.
C-                         This version is for PMUO Bank versions 3
C-                         onwards.
C-
C-   Inputs  : 
C-                PMUO Bank Versions 3 and higher
C-   Outputs : 
C-                Printout on unit LUN
C-   Controls: 
C-                None
C-
C-   Created  26-JAN-1993   Stephen J. Wimpenny
C-   Modified 10-Feb-1993   IFW1 decoding updated + Vertex no added.
C-                          Back-to-back depositions added
C-   Modified 17-Mar-1993   Name change for library compatibility
C-   Modified 21-Mar-1993   Name chage to routines Decode_ifw2,
C-                          Decode_Muon_Plane_Info, Caltrak_Angles,
C-                          Dr_From_Deta_Dphi
C-   Modified 16-Jun-1993   LUN 16 Reco dump switched by RCP file
C-   Modified 16-May-1994   IFW2 and IFW3 decoding updated
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
      INTEGER LUN,IER,ITEMP,IV,NORUN,NOEVT,GZHEAD
      INTEGER WAM_HIT(6),SAM_HIT(6)
      INTEGER NO_IFW2_BITS,IFW2_BITS(32)
      INTEGER NO_IFW3_BITS,IFW3_BITS(32)
C
      REAL PI,TWOPI
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
C *** IFW1, IFW3, Integ B.dL, Muon quality of fit in bend, non-bend
C
      LMUOT=LQ(LPMUO-2)
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
        ITEMP=IQ(LMUOT+4)
        IF(ITEMP.GE.10) THEN
          ITEMP=ITEMP-10
          WRITE(LUN,1012) ITEMP
        ELSE
          WRITE(LUN,1011) IQ(LMUOT+4)
        ENDIF
C
C *** Decode IFW3
C
        CALL TOP_LEPTONS_UTIL_DECODE_IFW3(LPMUO,NO_IFW3_BITS,
     1    IFW3_BITS,IER)
        IF(NO_IFW3_BITS.GT.0) WRITE(LUN,1013) (IFW3_BITS(IV),IV=1,
     1    NO_IFW3_BITS)
C
C *** B.dl and Fit Quality Variables
C
        WRITE(LUN,1014) Q(LMUOT+22),Q(LMUOT+20),Q(LMUOT+21)
      ENDIF
C
C *** Hits/track and bend+non-bend impact parameters 
C
      ITEMP=1
      CALL TOP_LEPTONS_UTIL_DECODE_PLANES(LPMUO,ITEMP,WAM_HIT,SAM_HIT)
      WRITE(LUN,1020) (WAM_HIT(IV),IV=1,6),(SAM_HIT(IV),IV=1,3)
      ITEMP=2
      CALL TOP_LEPTONS_UTIL_DECODE_PLANES(LPMUO,ITEMP,WAM_HIT,SAM_HIT)
      WRITE(LUN,1021) (WAM_HIT(IV),IV=1,6),(SAM_HIT(IV),IV=1,3)  
C
C *** trackfit chisq, floating t0, 3-D impact param, bend + non-bend impact
C
      WRITE(LUN,1030) Q(LPMUO+23),Q(LPMUO+24),IQ(LPMUO+55),Q(LPMUO+41),
     1  Q(LPMUO+56),Q(LPMUO+57)
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
C *** hit cell, hit cell +1nn, hit cell +2nn, dR=0.4 cone, dR=0.6 cone
C
      WRITE(LUN,1060) Q(LPMUO+33),Q(LPMUO+43),Q(LPMUO+83),Q(LPMUO+84),
     1 Q(LPMUO+34),Q(LPMUO+35),Q(LPMUO+36)
      WRITE(LUN,1061) Q(LPMUO+78),Q(LPMUO+79),Q(LPMUO+80),Q(LPMUO+81)
      WRITE(LUN,1062) Q(LPMUO+86),Q(LPMUO+87),Q(LPMUO+88),Q(LPMUO+89)
C----------------------------------------------------------------------
  999 RETURN
C
 1000 FORMAT(5X,' Muon flag bits (word2) = ',8(I2,2X),/31X,8(I2,2X))
 1010 FORMAT(5X,' Muon flag word 4 = ',I4,3X,' Quadrant = ',I5)
 1011 FORMAT(5X,' Muon layer flag (word1) = ',I2,
     1 ' ( Vertex not used in bend-view fit ) ')
 1012 FORMAT(5X,' Muon layer flag (word1) = ',I2,
     1 ' ( Vertex used in bend-view fit ) ')
 1013 FORMAT(5X,' Muon flag bits (word3) = ',8(I2,2X),/31X,8(I2,2X))
 1014 FORMAT(5X,' Integ. B.dl used for momentum = ',F8.2,/,
     1 5X,' Muon Quality of find in bend / non-bend views = ',2F8.1)
 1020 FORMAT(5X,' Available Hits : Wamus = ',6(I2,1X),
     1  ' Samus = ',3(I2,1X))
 1021 FORMAT(5X,' Used Hits      : Wamus = ',6(I2,1X),' Samus = ',
     1 3(I2,1X))
 1030 FORMAT(5X,' Trackfit Chisquare / dof = ',F8.2,
     1 2X,' Floated t0 offset = ',F8.2,' ns ',/,
     2 5X,' Vertex no. = ',I5,
     3 '     3-D Impact parameter = ',F8.1,/,
     4 5X,' Bend (RZ) / Non-Bend View (XY) Impact Parameters = ',
     5 2F8.2)
 1040 FORMAT(5X,' No. matching CD tracks = ',I3,' in cone of dR = ',
     1 F8.1)
 1050 FORMAT(7X,' theta,phi,eta,dR of closest track = ',4F8.2)
 1051 FORMAT(7X,' dE/dx in CDC = ',F8.1)
 1052 FORMAT(7X,' dE/dx in FDC = ',F8.1)
 1053 FORMAT(7X,' dE/dx in VTX = ',F8.1)
 1054 FORMAT(9X,' dR = ',F4.2,' ZTRAK id = ',I4,' theta,phi,eta = ',
     1 3F8.2)
 1060 FORMAT(5X,' Predicted dEdx in Calorimeter / Muon Iron = ',2F8.1,
     1 /5X,' Calorimeter Observed Deposition : a.) total ',/,
     2 5X,' (Hit cells,+1NN,+2NN,Cone 0.4,0.6)   = ',5F6.1)
 1061 FORMAT(5X,' b.) em (Hit cells,+1NN,+2NN,+4NN)    = ',4F6.1)
 1062 FORMAT(5X,' c.) b2b total (Cells,+1NN,+2NN,+3NN) = ',4F6.1)
C
 5000 FORMAT(2I8,I4,5F6.2,I4,2F6.2,I4)
C
      END
