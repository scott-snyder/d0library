C======================================================================
      SUBROUTINE PMCHNM2(ITRAK,VIEW)
C======================================================================
C
C    Purpose and Methods : To put chamber numbers on the blow-up 
C                          views
C 
C    Inputs  :   NMOD   the number of the module
C 
C    Created  14-JUN-1990   Carol C. Francis
C     DH 1-91 CHANGE GTMUOT CALL
C======================================================================
      IMPLICIT NONE
C======================================================================
C    LOCAL DECLARATIONS
C    ==================
C
      INTEGER ITRAK,NPTRAK,QUAD,IFW1,IFW2,NSAMUS,IFW3,ISPARE
      INTEGER VIEW,ILAYR,MULAYR
      INTEGER IHIT,IWADD,IHMUOH,ITSIGN,IDELT,IPAD  !for GTMHTT
      INTEGER NMOD,NPLN,NWIR,JERR,JLAYR(4),I
      INTEGER MODOLD,OLDLAYR
      REAL XI,YI,ZI,YMIN,YMAX
      REAL XMAGC,YMAGC,ZMAGC,ELCAL,ELFE,SPARE1,SPARE2
      REAL XCOSIM,YCOSIM,ZCOSIM
      REAL XCOSOM,YCOSOM,ZCOSOM
      REAL CHSQBV,CHSQNB
      REAL MOM,MOMER
      CHARACTER*3 CHNM
C
C    EXECUTABLE CODE 
C    ===============
C    Get information about each track
C    ================================
  50  CALL GTMUOT(ITRAK,NPTRAK,NSAMUS,QUAD,IFW1,IFW2,IFW3,ISPARE,
     X  XI,YI,ZI,XMAGC,YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,
     X  YCOSOM,ZCOSOM,
     X  CHSQBV,CHSQNB,MOM,MOMER,ELCAL,ELFE,SPARE1,SPARE2)
C
C    Set the viewing ports based on whether hit is in A, B, or
C    C layer and put in the chamber number
C    =========================================================
      DO 100 IHIT = 1,NPTRAK
        CALL GTMHTT(ITRAK,IHIT,IWADD,IHMUOH,ITSIGN,IDELT,IPAD)
        CALL MUADD(IWADD,NMOD,NPLN,NWIR,JERR)
        CALL PXITOC(NMOD,3,CHNM)
        ILAYR=MULAYR(NMOD)
        IF (NMOD.NE.MODOLD) THEN
          MODOLD = NMOD
          YMIN=-1.+(ILAYR-1)*.5
          IF (ILAYR.EQ.OLDLAYR) YMIN = YMIN+.11
          YMAX=YMIN+.5
          IF(YMAX.GT.1.0)THEN
             YMAX=1.0
             YMIN=.5
          ENDIF
          CALL JVPORT(.25,.75,YMIN,YMAX)
          CALL JWINDO(0.,50.,0.,50.)
          CALL JOPEN
            CALL JSIZE(3.,3.)
            CALL JMOVE(10.,26.)
            CALL J3STRG(CHNM)
          CALL JCLOSE
          OLDLAYR = ILAYR
        ENDIF
  100 CONTINUE
C
  999 RETURN
      END
