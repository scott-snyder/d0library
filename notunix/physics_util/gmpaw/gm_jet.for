      SUBROUTINE GM_JET(SWITCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : COMIS routine to histogram some JET quantities.
C-   NOTE: In PAW this routine will be called once per object as follows
C-
C-   PAW > NT/LOOP 1 GM_JET.FOR(0)
C-
C-   after the histograms have been booked or reset.
C-
C-   HISTOGRAMS
C-
C-   Inputs  : SWITCH   [R]   0.0 -- ALL TRIGGERS
C-                            1.0 -- JET TRIGGERS ONLY
C-   Outputs : None
C-
C-   Created  15-DEC-1992   Harrison B. Prosper
C-   Updated   3-JAN-1994   Harrison B. Prosper
C-    Use GMPAW.INC
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    SWITCH
C--------------------------------------------------------------------------
      INCLUDE 'GM$PAW:GMPAW.INC'
C--------------------------------------------------------------------------
      INTEGER I,J,IDOFF
      REAL    COST,MASS,PX1,PX2,PY1,PY2,PZ1,PZ2,E1,E2,ET1,ET2,P1,P2, P12
      LOGICAL FIRST, EM_OBJECT, BAD
C--------------------------------------------------------------------------
      DATA FIRST  /.TRUE./
C--------------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        IDOFF = 0
        PRINT *, ' GM_JET: Started'
      ENDIF
C
C *********************************************************************
C ****  FILL OBJECT BUFFERS
C *********************************************************************
C
      IOBJECT = IFIX(OBJECT)
C
      IF     ( IOBJECT .EQ. ID_VERT ) THEN
C
C ****  INITIALIZE
C
        NVERT  = ICOUNT
        NJET   = 0
        NNU    = 0
C
      ELSEIF ( IOBJECT .EQ. ID_JET  ) THEN
        INUMBER = MIN(IFIX(NUMBER),NOBJ)
        ICOUNT  = IFIX(COUNT)
        NJET    = ICOUNT
        CALL UCOPY(PX,JET(1,INUMBER),NSIZ)
C
      ELSEIF ( IOBJECT .EQ. ID_NU   ) THEN
        INUMBER = MIN(IFIX(NUMBER),NOBJ)
        ICOUNT  = IFIX(COUNT)
        NNU     = ICOUNT
        CALL UCOPY(PX,NU(1,INUMBER),NSIZ)
C
      ENDIF
C
C *********************************************************************
C ****  RETURN UNLESS THIS IS END-OF-EVENT OBJECT
C *********************************************************************
C
      IF ( IOBJECT .NE. ID_END  ) GOTO 999
C
C *********************************************************************
C ****  WE NOW HAVE ALL THE OBJECTS OF INTEREST
C *********************************************************************
C
      IF ( NJET .GE. 2 ) THEN
        PX1 = JET(IPX,1)
        PY1 = JET(IPY,1)
        PZ1 = JET(IPZ,1)
        E1  = JET(IE,1)
        ET1 = JET(IET,1)
C
        PX2 = JET(IPX,2)
        PY2 = JET(IPY,2)
        PZ2 = JET(IPZ,2)
        E2  = JET(IE,2)
        ET2 = JET(IET,2)
C
        P1  = PX1*PX1+PY1*PY1
        P2  = PX2*PX2+PY2*PY2
        P12 = PX1*PX2+PY1*PY2
C
C ****  COMPUTE INVARIANT MASS
C
        MASS= E1*E2 - (P12+PZ1*PZ2)
        IF ( MASS .GT. 0.0 ) THEN
          MASS = SQRT(MASS)
        ELSE
          MASS =-1.0
        ENDIF
C
C ****  COMPUTE ANGLE BETWEEN JETS
C
        COST = P12/SQRT(P1*P1)
C
        CALL HF1(5000,ET1,1.0)
        CALL HF2(5010,ET1,ET2,1.0)
        CALL HF1(5020,COST,1.0)
        CALL HF1(5030,MASS,1.0)
      ENDIF
C
  999 RETURN
      END
