      SUBROUTINE GTPMUO(IMUO,E,ET,SIG,SIGET,THETA,ETA,PHI,CHISQ,
     &  CHISQ_PROB,XYZ,R_ISOLATION,XPCTD_E_LOSS,CAL_E_LOSS,ANGLE,CHARGE,
     &  NDF,METHOD,IFLAG,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get info from PMUO bank
C-
C-   Inputs  : IMUO = Number of muon desired
C-   Outputs :
C-              E(4) = (PX,PY,PZ,ENERGY)
C-              ET
C-              SIG(4) = error on the Ith E(I)
C-              SIGET
C-              THETA = theta of muon
C-              ETA = eta of muon
C-              PHI = phi of muon
C-              CHISQ = chi**2 of muon track fit
C-              CHISQ_PROB = probability of CHISQ
C-              XYZ = X, Y AND Z WHERE TRACK DEFINED
C-              R_ISOLATION(J) = various isolation values:
C                         J=1 isolation parameter (based on cells hit only)
C                         J=2 isolation parameter (based on cells
C                         hit+neighbors)
C                         J=3 isolation parameter (based on cone size 0.2)
C                         J=4 isolation parameter (based on cone size 0.4)
C                         J=5 isolation parameter (based on cone size 0.6)
C-              XPCTD_E_LOSS = expected energy lost in the calorimeter
C-              CAL_E_LOSS(3) = actual calorimeter energy associated with muon
C-              ANGLE = ANGLE BETWEEN MUON AND NEAREST CD TRACK
C-              CHARGE = charge of muon
C-              NDF = number of degrees of freedom of muon fit
C-              METHOD = i2*100 + i1*10 +i0
C                             where
C                                i0: 1= vertex point is used in fitting
C                                i1: 1= central track is used
C                                i2: 1= E loss in CAL is corrected.
C-              IFLAG = where track vector is defind.
C                                1= vertex point
C                                2= in the muon system.
C                                3= other place
C               IER = 0 OKAY
C-                   -4 No PMUOs found
C-                   -5 Requested PMUO not found
C-                   -13 MDST bank not found and path is set to 'MDST'
C-   Controls:
C-
C-      Return total number of PMUO BANKS
C-
C-      ENTRY GTPMUO_TOTAL(NUM_MUO,IER)
C-          NUM_MUO   [I]  Number of PMUO banks
C-          IER       [I]  0 = ok
C-                        -13= no MDST bank and path is set to 'MDST'
C
C-
C-   Created  19-JUN-1991   Andrew J. Milder
C-   Modified 18-SEP-1993   R. Astur "Make offical and mdst compatible",
C-   Updated  15-OCT-1993   Marc Paterno  Reset CLEAN_ONLY to FALSE after every
C-                                        call.  Correct size of E() and SIG()
C-                                        vectors.
C-   Update  21-FEB-1994   R. Astur "Remove CLEAN_ code to eliminate link
C-                                    problems "
C=======================================================================
C
C  Bank Name : PMUO
C  Author    : Suichi Kunori
C  Modified  : Shahriar Abachi
C  Date      : 27-SEP-1989
C  Modified  : SK, 29-Jun-90, SA 07-MAR-91
C  Tree description : proc_TREE.ZEB
C
C  Bank description : muon particle bank
C
C     LQ     Q/IQ
C-----------------------------------------------------------------------
C  -NS-4          reference link to best ZTRK
C  -NS-3          reference link to VERT
C  -NS-2          reference link to MUON
C  -NS-1          reference link to MUOT
C     -1          free structual link
C      0          Next   link to PMUO
C     +1          Up     link to PARH
C     +2          Origin link to PARH (for first)
C.......................................................................
C             -5         Bank number
C             -4         Bank name, 'PMUO'
C             -3         NL = 5
C             -2         NS = 1
C             -1         ND = 38
C              0         Status
C             +1     I       bank version no.(=1)
C              2     I       id( 14=mu-,   -14=mu+ )
C              3     I       number of degree of freedom
C              4     I       method of calculation
C                                i2*100 + i1*10 +i0
C                             where
C                                i0: 1= vertex point is used in fitting
C                                i1: 1= central track is used
C                                i2: 1= E loss in CAL is corrected.
C              5     I       flag-  where track vector is defind.
C                                1= vertex point
C                                2= in the muon system.
C                                3= other place
C              6     I       number of CD tracks
C              7     I       free
C              8     I       free
C              9     I       free
C             10     F       Px
C             11     F       Py
C             12     F       Pz
C             13     F       P
C             14     F       Pt
C             15     F       theta
C             16     F       eta
C             17     F       phi
C             18     F       (sigPx)**2
C             19     F       (sigPy)**2
C             20     F       (sigPz)**2
C             21     F       (sigP )**2
C             22     F       (sigPt)**2
C             23     F        chisq
C             24     F        chisq probability
C             25     F       x coordinate where track vector defiend
C             26     F       y   :
C             27     F       z   :
C             28     F       isolation parameter (based on cells hit only)
C             29     F       isolation parameter (based on cells hit+neighbors)
C             30     F       isolation parameter (based on cone size 0.2)
C             31     F       isolation parameter (based on cone size 0.4)
C             32     F       isolation parameter (based on cone size 0.6)
C             33     F       E loss expected in CAL
C             34     F       E observed in CAL (in cone 0.2)
C             35     F       E observed in CAL (in cone 0.4)
C             36     F       E observed in CAL (in cone 0.6)
C             37     F       Angle between muon & nearest CD track (degrres)
C             38     F       free
C=======================================================================
C   note (1) :  Items 28 and 29 could be used for excluding punchthroughs
C               and perhaps non-existent or badly fit muons. Items 30-32
C               are best suited for isolation determination. Isolation
C               parameter is defined as: (E_observed - E_calculated) / Sigma
C               where Sigma is the combined width of the numerator.
C   note (2) :  The cone sizes may change in different releases. To get
C               the correct sizes see array ISO_CONE_SIZE in MURECO.RCP.
C=======================================================================
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      CHARACTER*4 PATH
      REAL    E(4),SIG(4),THETA,ETA,PHI,CHISQ,CHISQ_PROB,R_ISOLATION(5)
      REAL    XPCTD_E_LOSS,CAL_E_LOSS(3),XYZ(3),ET,ANGLE,SIGET
      INTEGER NDF,METHOD,IFLAG,IMUO,NUM_PMUO,LPMUO,IER,GZPMUO,NZBANK
      INTEGER CHARGE
      INTEGER LMDST,GZMDST,NUM_MUO,NREP,IOFF
C: Entry point
      INTEGER STATUS
      LOGICAL OK
C----------------------------------------------------------------------
      CALL PATHGT(PATH)
      IF (PATH.EQ.'MDST') THEN
        IER = 0
        LMDST = GZMDST()
        IF (LMDST .LE. 0) THEN
          IER = -4
          GOTO 999
        ENDIF
        NUM_MUO=IQ(LMDST+6)
        IF( IMUO.GT.NUM_MUO) THEN
          IER = -5
          GOTO 999
        ENDIF
        NREP=IQ(LMDST+5)
        IOFF=IQ(LMDST+7)+(IMUO-1)*NREP-1
        LPMUO = LMDST + IOFF
        CHARGE = -NINT(Q(LPMUO+2))/14
        NDF    = NINT(Q(LPMUO+3))
        METHOD = NINT(Q(LPMUO+4))
        IFLAG  = NINT(Q(LPMUO+5))

      ELSE
        IER = 0
        LPMUO = GZPMUO(IMUO)
        IF (LPMUO.LE.0) THEN
          IER = -4
          GOTO 999
        ENDIF
        CHARGE = -IQ(LPMUO+2)/14
        NDF    = IQ(LPMUO+3)
        METHOD = IQ(LPMUO+4)
        IFLAG  = IQ(LPMUO+5)
      ENDIF

C
C   Get bank information
C
      E(1)   = Q(LPMUO+10)
      E(2)   = Q(LPMUO+11)
      E(3)   = Q(LPMUO+12)
      E(4)   = Q(LPMUO+13)
      ET     = Q(LPMUO+14)
      THETA  = Q(LPMUO+15)
      ETA    = Q(LPMUO+16)
      PHI    = Q(LPMUO+17)
      SIG(1) = Q(LPMUO+18)
      SIG(2) = Q(LPMUO+19)
      SIG(3) = Q(LPMUO+20)
      SIG(4) = Q(LPMUO+21)
      SIGET  = Q(LPMUO+22)
      CHISQ      = Q(LPMUO+23)
      CHISQ_PROB = Q(LPMUO+24)
      XYZ(1)     = Q(LPMUO+25)
      XYZ(2)     = Q(LPMUO+26)
      XYZ(3)     = Q(LPMUO+27)
      R_ISOLATION(1) = Q(LPMUO+28)
      R_ISOLATION(2) = Q(LPMUO+29)
      R_ISOLATION(3) = Q(LPMUO+30)
      R_ISOLATION(4) = Q(LPMUO+31)
      R_ISOLATION(5) = Q(LPMUO+32)
      XPCTD_E_LOSS   = Q(LPMUO+33)
      CAL_E_LOSS(1)  = Q(LPMUO+34)
      CAL_E_LOSS(2)  = Q(LPMUO+35)
      CAL_E_LOSS(3)  = Q(LPMUO+36)
      ANGLE = Q(LPMUO+37)
  999 RETURN
C
C ****************************************************
C ****  ENTRY point to get total number of PMUO banks
C ****************************************************
      ENTRY GTPMUO_TOTAL(NUM_PMUO,IER)
C
      CALL PATHGT(PATH)
      IF ( PATH.EQ.'MDST') THEN
        LMDST = GZMDST()
        IF( LMDST.LE.0 ) THEN
          IER = -13
          NUM_PMUO = 0
        ELSE
          NUM_PMUO = IQ(LMDST+6)
          IER = 0
        ENDIF
      ELSE
        LPMUO = GZPMUO(0)
        IF ( LPMUO .GT. 0 ) THEN
          NUM_PMUO = NZBANK(IXCOM,LPMUO)
          IER = 0
        ELSE
          NUM_PMUO = 0
          IER = 0
        ENDIF
      ENDIF
      RETURN
C
C************************************************
C Get CLEAN muons only
C************************************************
C      ENTRY GTPMUO_CLEAN_ONLY
C      CLEAN_ONLY = .TRUE.
C      RETURN
      END
