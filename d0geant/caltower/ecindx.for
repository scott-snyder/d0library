      SUBROUTINE ECINDX(LAYER,SUBLAY,X,Y,Z,IETA,IPHI,ILYR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given X,Y,Z position in an End Cap Calorimeter
C-                         Geant volume specified by the LAYER and SUBLAYer
C-                         indices, determine the physics indices IETA, IPHI
C-                         and ILYR of the corresponding cell.
C-
C-   Inputs  : LAYER   = Physics Layer Index
C-             SUBLAY  = Sublayer within LAYER
C-             X,Y,Z   = Step position in the Global coordinate system
C-   Outputs : IETA    = Physics Eta Index
C-             IPHI    = Physics Phi Index
C-             ILYR    = Physics Layer Index
C-                       ( = Layer except that Layer = 3 is expanded to 3->6)
C-   Controls:
C-
C-   Created   2-FEB-1989   Alan M. Jonckheere
C-   Updated  24-APR-1989   Alan M. Jonckheere  Change names of SRCP routines 
C-   Updated  24-APR-1989   Alan M. Jonckheere  Corrected IPHI calculation 
C-   Updated  27-JUN-1989   N.A. Graf  Now handles new MH_DIVISION volumes
C-   Updated  17-MAY-1990   Alan M. Jonckheere  Handle live Endplates that are
C-                              larger than real live area.
C-   Updated  25-MAR-1992   K. Wyatt Merritt  Use value of PI from GCONST.INC;
C-                              should fix IPHI = 65 channels 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C ****  Include files
      INCLUDE 'D0$INC:GCONST.INC'
C
C ****  Input/Output
      INTEGER LAYER,SUBLAY
      REAL    X,Y,Z
      INTEGER IETA,IPHI,ILYR
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C ****  SRCP Names
      INTEGER MNAME
      PARAMETER( MNAME = 9 )
      CHARACTER*3 NAMES(MNAME)
      DATA NAMES/'EM1','EM2','EM3','EM4','IFH','ICH','MFH','MCH',' OH'/
      CHARACTER*1 PLSMIN(2)
      DATA PLSMIN/'+','-'/
      CHARACTER*15 NAME
C
C ****  SRCP data arrays
      INTEGER IVAL(1000)
      REAL    RVAL(1000)
      EQUIVALENCE ( IVAL, RVAL )
C
      INTEGER ISVAL(1000)
      REAL    RSVAL(1000)
      EQUIVALENCE ( ISVAL, RSVAL )
C
C ****  local variable
      INTEGER INDX,INDX2,INDX3,IADD,I,J,K,L,IE1
      REAL    R,PHI,CLPRAD
C
      INTEGER NSET,DETNAM(100),IDTYPE(100),KDET,LAY,SLAY
C ****  SRCP variables
      INTEGER NZDIV,VOLNAM,NBOUND,NPAD,NPHI,MPHI
      REAL    PHISTG,BOUND,ETAIND
C
      REAL    RLAY(2,37,0:3,1:17),RLAY3(2,37,0:2)
      REAL    UBOUND(0:3,1:17)
      INTEGER UBNDID(0:3,1:17)
      REAL    RL
C
C
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
C ****  LOOP OVER NAMES AND +- ENDS
        DO 500 I = 1, 1                 ! Only need + z for now 
C
C ****  Need IDTYPEs from SRCP
          CALL EZPICK('SRCP_REST')        ! Access General SRCP bank
          IF ( I.EQ.1 ) THEN
            CALL EZGSET('IUSET_END_CALORIMETER+Z',ISVAL,1)
          ELSE
            CALL EZGSET('IUSET_END_CALORIMETER-Z',ISVAL,1)
          ENDIF
          NSET = ISVAL(6)
          INDX = 6
          DO 100 J =  1, NSET
            DETNAM(J) = ISVAL(INDX+1)   ! Detector name
            IDTYPE(J) = ISVAL(INDX+2)   ! IDTYPE
            INDX = INDX + 13
  100     CONTINUE
C
C ****  Now get information about Z divisions
          CALL EZPICK('SRCP_ECAL')        ! Access ECAL SRCP bank
          DO 400 J =  1, MNAME
            WRITE (NAME,9000) NAMES(J),PLSMIN(I)
            CALL EZGSET(NAME,IVAL,1)
            NZDIV = IVAL(1)
            PHISTG = RVAL(3)
            INDX = 3
            DO 300 K =  1, NZDIV
              VOLNAM = IVAL(INDX+1)
              DO 200 L =  1, NSET
                IF ( DETNAM(L).EQ.VOLNAM ) THEN
                  KDET = IDTYPE(L)/1000
                  LAY = MOD(IDTYPE(L),1000)/10
                  SLAY = MOD(IDTYPE(L),10)
                  GOTO 210
                ENDIF
  200         CONTINUE
              KDET = 4
              LAY = 0
              SLAY = 0
  210         CONTINUE
C
              INDX = INDX + 3
c
              if(name.eq.'MFH_DIVISIONS+Z'.OR.
     +           name.eq.'MFH_DIVISIONS-Z'.OR. 
     +           name.eq.'MCH_DIVISIONS+Z'.OR.
     +           name.eq.'MCH_DIVISIONS-Z')  indx = indx + 1
c
              NBOUND = IVAL(INDX+1)
              INDX2 = INDX + NBOUND + 1
              NPAD = IVAL(INDX2+1)
              INDX3 = INDX2 + NPAD + 1
              NPHI = IVAL(INDX3+1)
              INDX = INDX + 1
              INDX2 = INDX2 + 1
              INDX3 = INDX3 + 1
C
C ****  Loop over pad boundaries (don't need high radius boundary)
C ****  Calculate corresponding Eta - store boundaries into appropriate working
C ****  array.
C
              DO 250 L =  1, NBOUND-1
                BOUND = RVAL(INDX+L)
                ETAIND = RVAL(INDX2+L)
                IF ( NPHI.GT.0 ) THEN
                  MPHI = IVAL(INDX3+L)
                ELSE
                  MPHI = 64
                ENDIF
                IETA = ETAIND
                IF ( LAY.EQ.3 ) THEN
                  IF ( MOD(INT(2*ETAIND),2).EQ.0 ) THEN
                    RLAY(1,IETA,SLAY,LAY) = BOUND
                    RLAY(2,IETA,SLAY,LAY) = MPHI
                  ELSE
                    RLAY3(1,IETA+1,SLAY) = BOUND
                    RLAY3(2,IETA+1,SLAY) = MPHI
                  ENDIF
                ELSE
                  RLAY(1,IETA,SLAY,LAY) = BOUND
                  RLAY(2,IETA,SLAY,LAY) = MPHI
                ENDIF
  250         CONTINUE
              UBOUND(SLAY,LAY) = RVAL(INDX+NBOUND)      ! Get upper bound
              UBNDID(SLAY,LAY) = RVAL(INDX2+NPAD)
              INDX = INDX3 + NPHI
  300       CONTINUE
  400     CONTINUE
  500   CONTINUE
      ENDIF
C
C ****  Working part of routine
      R = SQRT(X*X + Y*Y)
C
C ****  If Radius greater than Upper Bound, pick up IDTYPE of volume into which
C ****  to store energy.
      IF ( R.GT.UBOUND(SUBLAY,LAYER) ) THEN
        ILYR = UBNDID(SUBLAY,LAYER)    ! Dead layer IDTYPE
        ILYR = MOD(ILYR/10,100)        ! Extract layer number
        IF ( ILYR.LE.17 ) ILYR = 21    ! Force dead layer
        ILYR = -ILYR                   ! Return with negative layer number
        GOTO 999
      ENDIF
C
C ****   Find first non-zero entry
      DO 510 I = 37, 1, -1
        IF ( RLAY(1,I,SUBLAY,LAYER).GT.0 ) THEN
          IETA = I
          GOTO 520
        ENDIF
  510 CONTINUE
C
  520 IE1 = IETA
      DO 600 I =  IE1, 1, -1
        IF ( R.GT.RLAY(1,I,SUBLAY,LAYER) ) THEN
          IF ( (RLAY(1,I,SUBLAY,LAYER).NE.0) ) IETA = I
        ELSE
          GOTO 700
        ENDIF
  600 CONTINUE
  700 CONTINUE
      ILYR = LAYER
      IADD = 0
      IF ( LAYER.EQ.3 ) THEN
       IF ( Z.GE.0 ) THEN
        IF ( R.LT.RLAY3(1,IETA,SUBLAY) ) THEN
          IADD = 2
        ENDIF
          IF ( IETA.EQ.14) IADD = 2
       ELSE
        IF ( R.GT.RLAY3(1,IETA,SUBLAY)
     +       .and.RLAY3(1,IETA,SUBLAY).ne.0 ) THEN
          IADD = 2
        ENDIF
           IF ( IETA.EQ.14) IADD = 0 
       ENDIF
      ENDIF
C
C ****  Layer 17, Ieta 15 added into layer 15
      IF ( IETA.EQ.15 .AND. ILYR.EQ.17 ) ILYR = 15
C
C ****  PHI
      PHI = ATAN2(-Y,-X) + PI
      RL = RLAY(2,IETA,SUBLAY,LAYER)
      CLPRAD = RL/(2.*PI)
      IPHI = INT(CLPRAD*PHI)
      IF ( (RL.EQ.128.) .AND. 
     &          (MOD(IPHI,2).EQ.1) ) IADD = IADD + 1
      IPHI = IPHI*64/(RL-1.E-3)
      IPHI = IPHI + 1
      ILYR = ILYR + IADD
      IF ( Z.LT.0 ) IETA = -IETA
C
  999 RETURN
 9000       FORMAT(A3,'_DIVISIONS',A1,'Z')
      END
