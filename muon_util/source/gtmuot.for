C====================================================================
      SUBROUTINE GTMUOT(ITRAK,NPTRAK,NSAMUS,QUAD,IFW1,IFW2,IFW3,IFW4,
     X  XI,YI,ZI,XMAGC,YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,
     X  YCOSOM,ZCOSOM,CHSQBV,CHSNBV,MOM,MOMER,ELCAL,ELFE,BDL,CT)
C====================================================================
C
C  Description:  Gets all the information from the MUOT Zebra bank for
C  ============  track # ITRAK.
C
C  Argument Declarations:
C  ========================
C  ITRAK - INTEGER - Input - Number of the track for which one would like info.
C  NPTRAK - INTEGER - Output - Number of wide angle points on track.
C  NSAMUS - INTEGER - Output - Number of low angle points on track.
C  QUAD   - INTEGER - tracking quadrant
C  IFW1   - INTEGER - Output - Flagging word 1 (no. of modules)
C  IFW2   - INTEGER - Output - Flagging word 2 (good track)
C  IFW3   - INTEGER - Output - Flagging word 3 (multiple fit solutions)
C  XI,YI,ZI        - POSITION IN A-LAYER
C  XMAGC  - Real    - Output - X Nominally at center of magnet
C  YMAGC  - Real    - Output - Y Nominally at center of magnet
C  ZMAGC  - Real    - Output - Z Nominally at center of magnet
C  XCOSIM - Real    - Output - X Direction cosine inside magnet
C  YCOSIM - Real    - Output - Y Direction Cosine inside magnet
C  ZCOSIM - Real    - Output - Z Direction Cosine inside magnet
C  XCOSOM - Real    - Output - X Direction Cosine outside magnet
C  YCOSOM - Real    - Output - Y Direction Cosine outside magnet
C  ZCOSOM - Real    - Output - Z Direction Cosine outside magnet
C  CHSQBV - Real    - Output - CHISQ Bend View
C  CHSQNB - Real    - Output - CHISQ Non-bend View
C  MOM    - Real    - Output - Momentum
C  MOMER  - Real    - Output - Momentum Error
C  ELCAL  - Real    - Output - Energy loss in calorimeter
C  ELFE   - Real    - Output - Energy loss in iron
C       plus spares....
C
C  Author:
C  =======
C  Tami Kramer
C
C  Revision History:
C  =================
C  Original Creation - August 3,1987
C  DH 11/88 ADD QUAD
C  DH 9/89 ADD XI,YI,ZI
C  DH 1/91 add SAMUS and energy loss
C  DH 8/91 exit gracefully if bank isn't there
C  AT 2/92 fix bug in LMUOT
C=======================================================================
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
C  Argument Declarations:
C  ======================
C
      INTEGER ITRAK,NPTRAK,NSAMUS,IFW1,IFW2,IFW3,QUAD,IFW4
      REAL XMAGC,YMAGC,ZMAGC,XI,YI,ZI
      REAL XCOSIM,YCOSIM,ZCOSIM,XCOSOM,YCOSOM,ZCOSOM
      REAL CHSQBV,CHSNBV,MOM,MOMER,ELCAL,ELFE,BDL,CT
C
C  Local Declarations:
C  ===================
C
      INTEGER LMUOT,GZMUOT
C
C  Executable Code:
C  ================
C
      LMUOT = GZMUOT(ITRAK)
      IF(LMUOT.GT.0) THEN             ! AT 2/92
        NPTRAK = IQ(LMUOT+1)
        NSAMUS=  IQ(LMUOT+2)
        QUAD = IQ(LMUOT+3)
        IFW1 = IQ(LMUOT+4)
        IFW2 = IQ(LMUOT+5)
        IFW3 = IQ(LMUOT+6)
        IFW4 =IQ(LMUOT+7)
        XI = Q(LMUOT+8)
        YI = Q(LMUOT+9)
        ZI = Q(LMUOT+10)
        XMAGC = Q(LMUOT+11)
        YMAGC = Q(LMUOT+12)
        ZMAGC = Q(LMUOT+13)
        XCOSIM = Q(LMUOT+14)
        YCOSIM = Q(LMUOT+15)
        ZCOSIM = Q(LMUOT+16)
        XCOSOM = Q(LMUOT+17)
        YCOSOM = Q(LMUOT+18)
        ZCOSOM = Q(LMUOT+19)
        CHSQBV = Q(LMUOT+20)
        CHSNBV = Q(LMUOT+21)
        BDL    = Q(LMUOT+22)
        MOM    = Q(LMUOT+23)
        MOMER  = Q(LMUOT+24)
        ELCAL  = Q(LMUOT+25)
        ELFE   = Q(LMUOT+26)
        CT     = Q(LMUOT+27)
      ELSE
        NPTRAK = 0
        NSAMUS=  0
        QUAD = 0
        IFW1 = 99
        IFW2 = 99
        IFW3 = 99
        IFW4 = 99
        XI = 999.
        YI = 999.
        ZI = 999.
        XMAGC = 999.
        YMAGC = 999.
        ZMAGC = 999.
        XCOSIM = 999.
        YCOSIM = 999.
        ZCOSIM = 999.
        XCOSOM = 999.
        YCOSOM = 999.
        ZCOSOM = 999.
        CHSQBV = 999.
        CHSNBV = 999.
        BDL    = 999.
        MOM    = 999.
        MOMER  = 999.
        ELCAL  = 999.
        ELFE   = 999.
        CT     = 999.
      ENDIF
      RETURN
      END
