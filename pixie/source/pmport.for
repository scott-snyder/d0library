      SUBROUTINE PMPORT(ITRAK,VIEW)
C======================================================================
C                                                                      
C    Purpose and Methods : To determine viewport for blow-up of track
C 
C    Inputs  : ITRAK - Tells which track to show
C              VIEW - Tells whether to show bend or nonbend view
C 
C    Created   7-JAN-1990   Carol Francis
C   DH 4/90 USE MULAYR CALL
C   DH 1/91 CHANGE GTMUOT
C======================================================================
      IMPLICIT NONE
C======================================================================
C    Variable Declarations
C    =====================
      INTEGER ITRAK,NPTRAK,QUAD,IFW1,IFW2,NSAMUS,IFW3,ISPARE
      INTEGER VIEW,ILAYR,MULAYR
      INTEGER IHIT,IWADD,IHMUOH,ITSIGN,IDELT,IPAD  !for GTMHTT
      INTEGER NMOD,NPLN,NWIR,JERR,JLAYR(4),I
      REAL XI,YI,ZI,YMIN,ELCAL,ELFE,SPARE1,SPARE2
      REAL XMAGC,YMAGC,ZMAGC
      REAL XCOSIM,YCOSIM,ZCOSIM
      REAL XCOSOM,YCOSOM,ZCOSOM
      REAL CHSQBV,CHSQNB
      REAL MOM,MOMER
C    Get information about each track
C    ================================
  50  CALL GTMUOT(ITRAK,NPTRAK,NSAMUS,QUAD,IFW1,IFW2,IFW3,ISPARE,XI,
     X YI,ZI,XMAGC,YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,YCOSOM,
     X ZCOSOM,CHSQBV,CHSQNB,MOM,MOMER,ELCAL,ELFE,SPARE1,SPARE2)
C
C    Set the viewing ports based on whether hit is in A, B, or
C    C layer
C    =========================================================
      DO I=1,4
        JLAYR(I)=0
      ENDDO
C
      DO 100 IHIT = 1,NPTRAK
        CALL GTMHTT(ITRAK,IHIT,IWADD,IHMUOH,ITSIGN,IDELT,IPAD)
        CALL MUADD(IWADD,NMOD,NPLN,NWIR,JERR)
        ILAYR=MULAYR(NMOD)
        JLAYR(ILAYR)=JLAYR(ILAYR)+1
        IF(JLAYR(ILAYR).EQ.1) THEN      ! CAAL EACH LAYER ONLY ONCE
          YMIN=-1.+(ILAYR-1)*.5
          CALL JVPORT(-.44,.22,YMIN,YMIN+.5)
          CALL PMWIND(VIEW,NMOD,QUAD,XMAGC,YMAGC,ZMAGC,
     X                XCOSOM,YCOSOM,ZCOSOM)
        ENDIF
  100 CONTINUE
C
  999 RETURN
      END
