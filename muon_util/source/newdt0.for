      SUBROUTINE NEWDT0(MTRAK,PTRAK,WTRAK,WCEN,TDIV1,MODN,RES1,RES2,
     X  RES3,RES4,IOE,IOE2,IOE3,IOE4,FITPOS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : calculates a new dt0 value based on old
C-                         and new hit postions along wire. plots
C-                         new value by module/motherboard.
C-
C-
C-   Inputs  : MTRAK,PTRAK,WTRAK  module,plane,wire of hit
C-             WCEN               global coordinate of center of wire
C-                                in wire direction
C-             TDIV1              position correction along wire in global
C-                                coordinates due to original dt0
C-             MODN               module number (from loop)
C-             RES1,RES2          plane one residuals based on track through
C-                                hits on planes 0 and 2 (planes 0 and 3)
C              RES3,RES4        plane 2 residuals using planes 0,3 and 1,3
C-             IOE,IOE2,ioe3,ioe4                flag for odd-even combo
C-             FITPOS             calculated position in plane 1 from hits
C-                                on other planes
C-   Outputs : NONE
C-   Controls:
C-
C-   Created   4-JAN-1991   ERIC JAMES
C-      DH 1-8-92  extra - sign in CORDT definition
C-    DH 4/92 add whole mod histogram. change corrected value definition
C     DH 6/92 have only even wires in call to MUGTCN
C     DH 8/92 eliminate mobo stuff
C     DH 6/94 update for extended a-layer
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C  Local Declarations:
C  ====================
C
      REAL WCEN,TDIV1,RES1,RES2,FITPOS,IOE,IOE2,IOE3,IOE4,RES3,RES4
      INTEGER MTRAK,PTRAK,WTRAK,MODN,WTRAKE
      INTEGER K,IORIENT,ID,KID
      REAL TRKDIV1,NEWTDIV,WLEN,VECT(3),VOFF,NEWCORDT,CORDT
      REAL T01,T02,TRES1,TRES2,DT01,DT02,DTSLP1,DTSLP2
      REAL SPEED,RAWDT,DT0NEW
      DATA SPEED/30./
C
C   use same calib constants for all possibilities
      WTRAKE=WTRAK
      IF(MOD(WTRAK,2).EQ.1) WTRAKE=WTRAK-1   ! EVEN WIRE OF PAIR
      CALL MUGEOM(MTRAK,PTRAK,WTRAK,VECT,WLEN,VOFF,IORIENT)
      CALL MUGTCN(MTRAK,PTRAK,WTRAKE,T01,TRES1,T02,TRES2,DT01,
     X  DTSLP1,DT02,DTSLP2)
      ID=MODN
      IF (ABS(IOE).GT..1) THEN
       CALL HFILL(11000+ID,RES1,0.,1.)
       DT0NEW=RES1*IOE
       IF (IORIENT.LT.0) THEN
         DT0NEW=-DT0NEW
       ENDIF
       DT0NEW=DT0NEW/DTSLP1/SPEED     ! CONVERT TO ADC COUNTS
CC    fill new value dt0 histograms
C
       CALL HFILL(10000+ID,DT0NEW,0.,1.)
      ENDIF
      IF (ABS(IOE2).GT..1) THEN
       CALL HFILL(11000+ID,RES2,0.,1.)
       DT0NEW=RES2*IOE2
       IF (IORIENT.LT.0) THEN
         DT0NEW=-DT0NEW
       ENDIF
       DT0NEW=DT0NEW/DTSLP1/SPEED     ! CONVERT TO ADC COUNTS
CC    fill new value dt0 histograms
C
       CALL HFILL(10000+ID,DT0NEW,0.,1.)
      ENDIF
      IF (ABS(IOE3).GT..1) THEN
       CALL HFILL(11000+ID,RES3,0.,1.)
       DT0NEW=RES3*IOE3
       IF (IORIENT.LT.0) THEN
         DT0NEW=-DT0NEW
       ENDIF
       DT0NEW=DT0NEW/DTSLP1/SPEED     ! CONVERT TO ADC COUNTS
CC    fill new value dt0 histograms
C
       CALL HFILL(10000+ID,DT0NEW,0.,1.)
      ENDIF
      IF (ABS(IOE4).GT..1) THEN
       CALL HFILL(11000+ID,RES4,0.,1.)
       DT0NEW=RES4*IOE4
       IF (IORIENT.LT.0) THEN
         DT0NEW=-DT0NEW
       ENDIF
       DT0NEW=DT0NEW/DTSLP1/SPEED     ! CONVERT TO ADC COUNTS
CC    fill new value dt0 histograms
C
       CALL HFILL(10000+ID,DT0NEW,0.,1.)
      ENDIF
C
  999 RETURN
      END
