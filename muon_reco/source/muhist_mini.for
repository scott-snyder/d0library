      SUBROUTINE MUHIST_MINI(IHOFF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Minimum histograms for MUON_RECO.
C-
C-   Inputs  : IHOFF    I    Offset for histogram ID.
C-   Outputs :
C-   Controls:
C-
C-   Created  28-MAR-1990   Shuichi Kunori
C-   DH 1-91 CHANGE MUOT
C-   Updated  30-JUL-1991   Daria Zieminska include samus-wamus tracks,
C-                          add a histogram of pseudorapidity
C   DH 11-91 major overhaul for verification
C   DH 12-91 global fit defined in different word
C   DH reduce somehat the number of histograms
C   DH 6/92 use IFW4=0 for good
C   DD 11-MAY-1993  Add SAMUS histogram
C   DD 18-APR-1994  Correct SAMUS quadrants - 13 and 14 only
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IHOFF,LPMUO,GZPMUO,NTRAKS,ITRAK,NPTRAK,JJQ,IFW1,IFW2,
     A NSAMUS,IFW3,IFW4
      REAL    XI,YI,ZI,XMAGC,THET
      REAL    YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,YCOSOM,ZCOSOM
      REAL    CHSQBV,CHSNBV,RMOM,RMOMER,ELCAL,ELFE,SPARE1,SPARE2
      REAL    THETD,PHID, ETA,GMT,GP1,GP2
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL HBOOK1(IHOFF+3,'=MUOT ALL= THETA ',18,0.,180.,0.0)
        CALL HBOOK1(IHOFF+7,'=MUOT GOOD= THETA ',18,0.,180.,0.0)
        CALL HBOOK1(IHOFF+13,'=PMUO= THETA ',18,0.,180.,0.0)
        CALL HBOOK1(IHOFF+14,'=PMUO= PHI '  ,18,0.,360.,0.0)
        CALL HBOOK1(IHOFF+15,'=PMUO GLOBAL FIT= THETA ',18,0.,
     A   180.,0.0)
        CALL HBOOK1(IHOFF+6,'=MUOT SAMUS= MOMENTUM',100,-100.,100.,0.0)
      ENDIF
C
      GMT=0.
      CALL GTMTRH(NTRAKS)      ! SEE HOW MANY TRACKS
      IF(NTRAKS.GT.0) THEN
        DO ITRAK=1,NTRAKS
          CALL GTMUOT(ITRAK,NPTRAK,NSAMUS,JJQ,IFW1,IFW2,IFW3,IFW4,
     X      XI,YI,ZI,XMAGC,YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,
     X      YCOSOM,ZCOSOM,CHSQBV,CHSNBV,RMOM,RMOMER,ELCAL,ELFE,
     X      SPARE1,SPARE2)
          IF(JJQ.GE.13.AND.JJQ.LE.14) CALL HFILL(IHOFF+6,RMOM,0.,1.) ! SAMUS
          IF (NPTRAK+NSAMUS.GT.0) THEN
            THET=ACOS(ZCOSIM)
            THETD=THET*180./3.14159
            PHID=ATAN2(YCOSIM,XCOSIM)*180./3.14159
            IF(PHID.LT.0.) PHID=PHID+360.
            CALL HFILL(IHOFF+3,THETD,0.,1.)
            IF (IFW4.EQ.0) THEN
              CALL HFILL(IHOFF+7,THETD,0.,1.)
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C   LOOK AT PMUO BANK
      LPMUO=GZPMUO(0)
      IF(LPMUO.NE.0) THEN
        DO WHILE (LPMUO.GT.0)
          THETD=Q(LPMUO+15)*180./3.14159
          PHID=Q(LPMUO+17)*180./3.14159
          CALL HFILL(IHOFF+13,THETD,0.,1.)
          CALL HFILL(IHOFF+14,PHID,0.,1.)
CCCCCCCCCCC GLOBAL FIT TRACKS
          IF(IQ(LPMUO+4).EQ.1.OR.IQ(LPMUO+4).EQ.11) THEN
            CALL HFILL(IHOFF+15,THETD,0.,1.)
          ENDIF
          LPMUO=LQ(LPMUO)          ! pointer to next muon
        ENDDO
      ENDIF
  999 RETURN
      END
