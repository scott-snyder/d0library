      SUBROUTINE OBJECT_MUON1(MUON_TYPE,IMU,NINFO,XMU_ARRAY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns muon information for Top analysis
C-                         Has to have PARTICLE_SELECT and
C-                         MUJETS_MU_SELECT linked in
C-
C-   Inputs  :  MUON_TYPE = 'ISOLMUON' or "BTAGMUON'
C-              IMU        Muon number (IMU=1 is the highest pt muon)
C-              NINFO      Number of information requested
C-
C-   Outputs :  XMU_ARRAY  Array of information returned. The maximum
C-                         list is : PX PY PZ P PT ETA PHI ETAD QF
C-                                   ISOL1 ISOL2 ISOL4 NCD ANGCD IMPT
C-                                   IFW4 IFW1 BDL DPHI DTHETA CAL_EN
C-                                   TRK_HITS FIT_HITS
C-   Controls:
C-
C-    Fix GZPMUO bug - correct at last one hopes
C-   Created    9-JUN-1994   Rajendran Raja   
C-                           based on OBJECT_MUON GM package
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) MUON_TYPE
      INTEGER IMU,NINFO
      REAL XMU_ARRAY(*)
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER MAXINFO
      PARAMETER (MAXINFO=25)
      INTEGER IQMUO(MAXINFO)
      REAL QMUO(MAXINFO)
      EQUIVALENCE (QMUO,IQMUO)
      INTEGER I,LPMUO,LMUOT,GZPMUO,NINFO1,ITRAK
      INTEGER NMUONS,NMUINF
      CHARACTER*80 MSG
C
C will allow this many max muons of any type
C
      INTEGER MAX_MUONS
      PARAMETER( MAX_MUONS = 5 ) 
      INTEGER MUON_LINKS(MAX_MUONS)
C
      REAL    DRR,DEDIFF,PTREL
C----------------------------------------------------------------------
      NINFO1 = NINFO
      IF(NINFO .GT. MAXINFO) THEN
        MSG = 'NUMBER OF INFO REQUESTED IS LARGER THAN AVAILABLE.
     &    WILL BE SET TO MAXINFO'
        CALL ERRMSG('OBJECT_MUON','OBJECT_MUON',MSG,'I')
        NINFO1 = MAXINFO
      ENDIF
C
      CALL GTSLINK_ONE(MUON_TYPE,IMU,LPMUO)
C
C By using the above call we leave the links in protected area
C
      QMUO(1) = Q(LPMUO + 10)         !px
      QMUO(2) = Q(LPMUO + 11)         !py
      QMUO(3) = Q(LPMUO + 12)         !pz
      QMUO(4) = Q(LPMUO + 13)         !p
      QMUO(5) = Q(LPMUO + 14)         !pt
      QMUO(6) = Q(LPMUO + 16)         !eta
      QMUO(7) = Q(LPMUO + 17)         !phi
      QMUO(8) = Q(LPMUO + 16)         !eta_detector (eta for now)
      IQMUO(9) = IQ(LPMUO + 44)       !quality flag (IFW2)
      QMUO(10) = Q(LPMUO + 29)        !isolation_1
      QMUO(11) = Q(LPMUO + 30)        !isolation_2
      QMUO(12) = Q(LPMUO + 31)        !isolation_4
      QMUO(13) = IQ(LPMUO + 6)        !No of CD tracks
      QMUO(14) = Q(LPMUO + 37)        !angle between muon and CD (degrees)
      QMUO(15) = Q(LPMUO + 41)        !impact parameter
      QMUO(16) = IQ(LPMUO + 9)        !quality flag 2 (IFW4)
      IQMUO(17) = IQ(LMUOT + 4)       ! IFW1
      QMUO(18) = Q(LMUOT + 22)        ! integral B.dl
      QMUO(19) = Q(LPMUO + 38)        ! ZTRAK_DPHI
      QMUO(20) = Q(LPMUO + 39)        ! ZTRAK_DTHETA
      QMUO(21) = Q(LPMUO + 34)        ! Calorimeter energy in tight cone
      QMUO(22) = Q(LPMUO + 46)        ! Hits on track
      QMUO(23) = Q(LPMUO + 47)        ! Fitted hits on track
C
      IF ( MUON_TYPE.EQ.'ISOLMUON' ) THEN
        CALL ISOL_MU_GETPAR(LPMUO,DRR,DEDIFF)
        QMUO(24) = DRR               !deltaR  from nearest jet
        QMUO(25) = DEDIFF            !diff energy in 2x2 neighbor and .4 cone
      ELSEIF ( MUON_TYPE.EQ.'BTAGMUON' ) THEN
        CALL TAG_MU_GETPAR(LPMUO,DRR,PTREL)
        QMUO(24) = DRR               !deltaR  from nearest jet
        QMUO(25) = PTREL             !Pt Relative to nearest jet
      ENDIF
C
      DO I=1,NINFO1
        XMU_ARRAY(I) = QMUO(I)
      ENDDO
C
      CONTINUE
C
  999 RETURN
C
C**************************************************************************
C
      ENTRY NOBJ_MUONS1(MUON_TYPE,NMUONS,NMUINF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Used in global monitoring
C-
C-   Inputs  :   MUON_TYPE = 'ISOLMUON' or 'BTAGMUON'
C-
C-   Outputs  :  NMUONS     Number of pmuo muons
C-               NMUINF    Number of information per muon
C-   Outputs :
C-   Controls:
C-
C-   Created   9-JUN-1994   Rajendran Raja   
C-
C----------------------------------------------------------------------
      CALL GTSLINK(MUON_TYPE,MAX_MUONS,NMUONS,MUON_LINKS)
      NMUINF = MAXINFO
C
C----------------------------------------------------------------------
      RETURN
      END
