      SUBROUTINE SSWHIT(ISIDE,R0,TGTHE,PHIC,NSOL,PAV,CH,ICH,NCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Look for hits in WAMUS C chambers that
C-                         are in a road defined by triplets in SAMUS A and
C-                         SAMUS B;
C-
C-
C-   Inputs  : ISIDE - North, South
C-             R0    \
C-             TGTHE  > road in Samus
C-             PHIC  /
C-
C-   Outputs : NSOL  - number of solutions (0 or 1 now)
C-             PAV   - average of WAMUS C hits coordinates
C-             CH    - WC hits coordinates
C-             ICH   - plane, module and MUOH hit of point
C-             NCH   - number of hits found in WC (inside road)
C-   Controls: none
C-
C-   Created  17-FEB-1994   Joao de Mello
C-   modified  3-JUN-1994   Andre Sznajder - MUMODU call only if FIRST
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C     Zebra pointers
      INCLUDE 'D0$INC:ZEBCOM.INC'
C     inputs
      INTEGER ISIDE
      REAL R0, TGTHE, PHIC
C     outputs
      INTEGER NSOL
      REAL PAV(3)
C     modules in C station
      INTEGER NMOD(2),PDT_C_NORTH(20), PDT_C_SOUTH(20)
C     variables for sub. MUMODU
      INTEGER  IMOD, NBUF, IBUF, NSPAR,NPLN, NWIR
      REAL XPAR(3),SPAR(3),ROTM(3,3), HALFSIZE(2,20,3)
      REAL XPARMOD(2,20,3)
      CHARACTER*4 HSHAPE
C     muon chamber orientation
      INTEGER IORIENT, MUORIENT
C     half size of the window and hit coordinates
      REAL XWIN, YWIN, XH, YH, ZH
C     auxiliary variables for the calculation of the average
      REAL XSUM, YSUM, ZSUM 
C     arguments for subroutine GTMPHT
C         ICELLHIT: hit number within a cell (1 or 2)
C         SIGN: the positive/negative ambiguity of DRIFT
C             -1 Negative ambiguity
C             +1 positive ambiguity
C              0 neglect ambiguity - drift coord. and wire
C          ICELLHIT = 1
C          SIGN = 0
      INTEGER  ICELLHIT, SIGN
      PARAMETER( ICELLHIT = 1)
      PARAMETER( SIGN     = 0)
C     How many hits for a given module there is in MUOH and flag
      INTEGER NHIT, IERR
C     arguments for gtmuoh
      INTEGER KHIT,IWADD,IFW1,IFW2,INRAW,IORIEN,NHWIR
      REAL  CORT1,CORT2,CORP1,CORP2,CORDT1,CORDT2,DDIS1
      REAL  DDIS2,TDIV1,TDIV2,VERD1,VERD2,XCWIR,YCWIR,ZCWIR
C     Window parameters
      REAL  BEND_WIN, NON_BEND_WIN
C     position in C
      REAL RC, XC, YC, ZC,ZBEAM
C     counter of hits
      INTEGER NCH, ICHMAX
      PARAMETER( ICHMAX = 24 )
      REAL RICH
C     hits array
      REAL CH(ICHMAX,3)
      INTEGER ICH(ICHMAX,4)
C     if two hits were found in one module, it is true
      LOGICAL ONE_MOD
C     keep values of planes, modules and # hits in each module
C      that are inside the road
      INTEGER FIRST_MOD,SEC_MOD,THIRD_MOD,FOURTH_MOD
      INTEGER NH1STMOD,NH2NDMOD,NH3RDMOD,NH4THMOD
C     Cuts from RCP
      INTEGER  NMAXSOL,NMINH,NMAXH
C     do loop indexes
      INTEGER K, I, J, L
C     if it's the first call to sswhit it's true
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST /.TRUE./
C
C
C *** Loop over modules to fill module parameter vectors
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK ('SAMUS_UTIL_PARAM')
        CALL EZGET  ('ZBEAM', ZBEAM, IERR)
        CALL EZRSET
        CALL EZPICK ('SSW_UTIL_PARAM')
        CALL EZGETA ('PDT_C_NORTH',0,0,0,NMOD(1),IERR)
        CALL EZGETA ('PDT_C_NORTH',1,NMOD(1),1,PDT_C_NORTH,IERR)
        CALL EZGETA ('PDT_C_SOUTH',0,0,0,NMOD(2),IERR)
        CALL EZGETA ('PDT_C_SOUTH',1,NMOD(2),1,PDT_C_SOUTH,IERR)
        CALL EZGET  ('BEND_WIN',BEND_WIN,IERR)
        CALL EZGET  ('NON_BEND_WIN',NON_BEND_WIN,IERR)
        CALL EZGET  ('NMAXSOL',NMAXSOL,IERR)
        CALL EZGET  ('NMINH',NMINH,IERR)
        CALL EZGET  ('NMAXH',NMAXH,IERR)
        CALL EZRSET
        DO 10 I = 1, 2
        DO 20 K = 1, NMOD(I)
          IF (I.EQ.1) IMOD = PDT_C_NORTH(K)
          IF (I.EQ.2) IMOD = PDT_C_SOUTH(K)
C
C ******* Get module center coordenates (XPAR) and geometrical parameters
C
          CALL MUMODU(IMOD, HSHAPE, NSPAR, SPAR, XPAR, ROTM,
     &                  NBUF, IBUF)
          IF(NSPAR.NE.3) THEN
            CALL ERRMSG('NSPAR HAS NO MEANING','SSWHIT',' ','W')
            GOTO 20
          ENDIF
C
C ******* Transform the hafsizes of the modules from local coordinates to
C         global coordinates
C
          DO L = 1,3
            HALFSIZE(I,K,L) = 0.0
            DO J = 1,3
               HALFSIZE(I,K,L) = HALFSIZE(I,K,L) +
     &                                  ROTM(L,J)*SPAR(J)
            END DO
            HALFSIZE(I,K,L) = ABS(HALFSIZE(I,K,L))
            XPARMOD(I,K,L)=XPAR(L)
          END DO
   20   CONTINUE
   10   CONTINUE
      ENDIF
C
C *** Initialization 
C
      NCH = 0
      NSOL = 0
C
C ***  Loop over modules
C
      DO 30 K = 1, NMOD(ISIDE)
        IF (ISIDE.EQ.1) IMOD = PDT_C_NORTH(K)
        IF (ISIDE.EQ.2) IMOD = PDT_C_SOUTH(K)
C
C ***** Find the projection of the road in the module
C
        ZC = XPARMOD(ISIDE,K,3)
        RC = R0 + TGTHE*(ZC-ZBEAM)
        XC = RC*COS(PHIC)
        YC = RC*SIN(PHIC)
C
C ***** If this module is NOT on the way of this road
C
        IF  (ABS(XC - XPARMOD(ISIDE,K,1)).GT.HALFSIZE(ISIDE,K,1)
     &  .OR. ABS(YC - XPARMOD(ISIDE,K,2)).GT.HALFSIZE(ISIDE,K,2))
     &      GO TO 30
C
C ***** Defines the search window in C based on module orientation
C
        IORIENT = MUORIENT(IMOD)
        IF (IORIENT .EQ. 3) THEN
           XWIN = BEND_WIN
           YWIN = NON_BEND_WIN
        ELSE IF (IORIENT .EQ. 4) THEN
           XWIN = NON_BEND_WIN
           YWIN = BEND_WIN
        ENDIF
C
C ***** Initalizes MUHMOD. Get # of processed hits in this module and hit #.
C
        CALL MUHMOD(0, NHIT, KHIT)
        CALL MUHMOD(IMOD, NHIT, KHIT)
        IF ((NHIT.LT.NMINH).OR.(NHIT.GT.NMAXH)) GOTO 30   
C
C ***** Loop over all hits in this module
C
        DO 40 I = 1, NHIT
C
C ******* Get information in MUOH for this hit
C
          CALL GTMUOH(KHIT,IWADD,IFW1,IFW2,INRAW,IORIEN,NHWIR,
     &         CORT1,CORT2,CORP1,CORP2,CORDT1,CORDT2,DDIS1,
     &         DDIS2,TDIV1,TDIV2,VERD1,VERD2,XCWIR,YCWIR,ZCWIR)
C
C ******* Get which plane in this  module
C
          CALL MUADD(IWADD, IMOD, NPLN, NWIR, IERR)
C
C ******* Get the coordinates for this point
C
          CALL GTMPHT(KHIT,ICELLHIT,SIGN,XH,YH,ZH)
C
C ******* If this hit is within the window fill hit arrays
C
          IF (ABS(XH-XC).LT.XWIN.AND.ABS(YH-YC).LT.YWIN) THEN
            IF (NCH.LT.NMAXSOL) THEN
                NCH = NCH + 1
                CH(NCH, 1) = XH
                CH(NCH, 2) = YH
                CH(NCH, 3) = ZH
                ICH(NCH, 1) = NPLN
                ICH(NCH, 2) = IMOD
                ICH(NCH, 3) = KHIT
                ICH(NCH, 4) = IWADD
            ENDIF
          ENDIF
C
C ****** Next hit pointer in MUOH
C
          KHIT = KHIT + 1
C
   40   CONTINUE
   30 CONTINUE
C
C *** Process hits. Renumber planes acording to module order. 
C      A road can cross at most 4 modules. It's necessary that 
C      modules are correctly ordered in RCP file
C 
      FIRST_MOD=ICH(1,2)
      SEC_MOD=0
      THIRD_MOD=0
      FOURTH_MOD=0
      NH1STMOD=1
      NH2NDMOD=0
      NH3RDMOD=0
      NH4THMOD=0
      DO 50 K = 2, NCH
          IF ((SEC_MOD.EQ.0).AND.(ICH(K,2).NE.FIRST_MOD)) 
     &         SEC_MOD=ICH(K,2)
          IF ((THIRD_MOD.EQ.0).AND.(ICH(K,2).NE.FIRST_MOD) 
     &       .AND.(ICH(K,2).NE.SEC_MOD)) THIRD_MOD=ICH(K,2)
          IF ((FOURTH_MOD.EQ.0).AND.(ICH(K,2).NE.FIRST_MOD) 
     &       .AND.(ICH(K,2).NE.SEC_MOD).AND.(ICH(K,2).NE.
     &            THIRD_MOD))  FOURTH_MOD=ICH(K,2)
          IF (ICH(K,2).EQ.FIRST_MOD) THEN
            NH1STMOD = NH1STMOD+1
          ELSE IF (ICH(K,2).EQ.SEC_MOD) THEN
            ICH(K,1) = ICH(K,1) + 3
            NH2NDMOD = NH2NDMOD+1
          ELSE IF (ICH(K,2).EQ.THIRD_MOD) THEN
            ICH(K,1) = ICH(K,1) + 6
            NH3RDMOD = NH3RDMOD+1
          ELSE IF (ICH(K,2).EQ.FOURTH_MOD) THEN
            ICH(K,1) = ICH(K,1) + 9
            NH4THMOD = NH4THMOD+1
          ENDIF
   50 CONTINUE
C
C ***** Check if there is any module with at least two hits inside the road
C
        IF ((NH1STMOD.GE.2).OR.(NH2NDMOD.GE.2).OR.
     &     (NH3RDMOD.GE.2).OR.(NH4THMOD.GE.2)) THEN
          ONE_MOD = .TRUE.
        ELSE
          ONE_MOD = .FALSE.
        ENDIF
C
C ***** Calculates Wamus C hits center of gravity
C
        IF(ONE_MOD) THEN
          RICH = FLOAT(NCH)
          XSUM = 0.
          YSUM = 0.
          ZSUM = 0.
          DO K = 1, NCH
            XSUM = XSUM + CH(K,1)
            YSUM = YSUM + CH(K,2)
            ZSUM = ZSUM + CH(K,3)
          END DO
          PAV(1) = XSUM/RICH
          PAV(2) = YSUM/RICH
          PAV(3) = ZSUM/RICH
          NSOL = 1
        END IF
C      
  999 RETURN
      END
