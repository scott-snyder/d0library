      SUBROUTINE
     &  FHITYZ(HALF,QUAD,SECTOR,NH,IIWIRE,LRWIR,IIHIT,YLOC,ZLOC,HLOC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  21-FEB-1992   Susan K. Blessing
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER I
      INTEGER HALF,UNIT,QUAD,SECTOR
      INTEGER NH,IIWIRE(NBTSEN),LRWIR(NBTSEN),IIHIT(NBTSEN)
      INTEGER IPTR(0:NBTSEN-1)
      INTEGER I1,IWIRE,IHIT,LR,NEL,NWORDS
      INTEGER HLOC(0:NBTSEN-1,MX_HIT_WIRE*2,3)
      INTEGER NOFF
C
      REAL    YLOC(0:NBTSEN-1,MX_HIT_WIRE*2)
      REAL    ZLOC(0:NBTSEN-1,MX_HIT_WIRE*2)
      REAL    RADIUS,STAGGER,XC,YC,ZC,DIR
      REAL    CONT(18)
      REAL    QHIT(18*MX_HIT_TSEC)
      REAL    FSTAGR
C
C----------------------------------------------------------------------
C
C  Initializations.
C
      UNIT = 0
      CALL VZERO_i(HLOC,NBTSEN*MX_HIT_WIRE*2*3)
      CALL VZERO(YLOC,NBTSEN-1*MX_HIT_WIRE*2)
      CALL VZERO(ZLOC,NBTSEN-1*MX_HIT_WIRE*2)
      DIR = 1.
      IF (SECTOR .EQ. 1) DIR = -1.
      CALL GTFTSC(HALF,QUAD,SECTOR,'WIR',0,NEL,NWORDS,CONT)
      CALL UCOPY(CONT(NEL+1),IPTR,NEL)
C
      CALL GTFTSC(HALF,QUAD,SECTOR,'ALL',IPTR(0),NEL,NWORDS,QHIT)
      NOFF = 2*NEL+4
C
C  Loop over sense wires in IIWIRE array
C
      DO 10 I = 1,NH
        IWIRE = IIWIRE(I)
        CALL GTFALH(HALF,UNIT,QUAD,SECTOR,IWIRE,XC,YC,ZC)
        IF (ZC.EQ. 0.0) GOTO 10
        RADIUS = ((XC)**2. + (YC)**2.)**.5
        STAGGER = 0.
        STAGGER = FSTAGR(HALF,UNIT,QUAD,SECTOR,IWIRE)
C
C Use hit given in IIHIT array
        IHIT = IIHIT(I)
C
        I1 = IPTR(IWIRE)-NOFF+NWORDS*(IHIT-1)
        IF (QHIT(I1+2).GT.6.0) GO TO 10
C
C Use LR given in LRWIR array
C        LR = MOD(LRWIR(IWIRE),2)
        LR = MOD(LRWIR(I),2)
C
        YLOC(IWIRE,1) = RADIUS+(QHIT(I1+2+LR)*DIR)-STAGGER
C                    stagger subtracted to use hits located from central wire
C                    plane.  XC,YC includes the stagger, FTSC does not.
        ZLOC(IWIRE,1) = ZC         ! record wire z position
        HLOC(IWIRE,1,1) = IWIRE    ! record wire
        HLOC(IWIRE,1,2) = IHIT     ! record hit location
        HLOC(IWIRE,1,3) = LR       ! record L/R ambiguity
C
   10 CONTINUE
C
  999 RETURN
      END
