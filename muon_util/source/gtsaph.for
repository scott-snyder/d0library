      SUBROUTINE GTSAPH(IHIT,IWADD,IRAW,IFLG1,IFLG2,IFLG3,ITYPE,SPLIT,
     &                  SPARE,WLEN,WCEN,WDIR,DTIME,DTERR,DIST,DERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get contnents of SAPH for one hit.
C-
C-   Inputs  : 
C-      IHIT    I   Hit number in bank
C-
C-   Outputs : 
C-      IWADD   I   Wire address
C-      IRAW    I   Hit number in bank MUHP
C-      IFLG1   I   Flag word
C-      IFLG2   I   spare
C-      IFLG3   I   spare
C-      ITYPE   I   Tube type
C-      SPLIT   F   1/2 distance between split tubes
C-      SPARE   F   spare
C-      WLEN    F   Wire length
C-      WCEN(3) F   Center of wire
C-      WDIR(3) F   Direction cosines along wire
C-      DTIME   F   Drift time
C-      DTERR   F   Error on drift time
C-      DIST    F   Drift distance
C-      DERR    F   Error on drift distance
C-
C-   Controls: 
C-
C-   Created  22-AUG-1994   M. Fortner
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER IHIT,IWADD,IRAW,IFLG1,IFLG2,IFLG3,ITYPE
      REAL SPLIT,SPARE,WLEN,WCEN(3),WDIR(3),DTIME,DTERR,DIST,DERR
C
      INTEGER ISAPH,LSAPH,GZSAPH,NWD
      EXTERNAL GZSAPH
      DATA NWD/19/
C 
      IWADD = 0
      LSAPH = GZSAPH(0)
      IF (LSAPH .EQ. 0) RETURN
      ISAPH = NWD*(IHIT-1)
      IF (ISAPH.GE.IQ(LSAPH-1)) RETURN
      ISAPH = LSAPH + ISAPH
C
      IWADD = IQ(ISAPH+1)
      IRAW  = IQ(ISAPH+2)
      IFLG1 = IQ(ISAPH+3)
      IFLG2 = IQ(ISAPH+4)
      IFLG3 = IQ(ISAPH+5)
      ITYPE = IQ(ISAPH+6)
      SPLIT = Q(ISAPH+7)
      SPARE = Q(ISAPH+8)
      WLEN  = Q(ISAPH+9)
      WCEN(1) = Q(ISAPH+10)
      WCEN(2) = Q(ISAPH+11)
      WCEN(3) = Q(ISAPH+12)
      WDIR(1) = Q(ISAPH+13)
      WDIR(2) = Q(ISAPH+14)
      WDIR(3) = Q(ISAPH+15)
      DTIME = Q(ISAPH+16)
      DTERR = Q(ISAPH+17)
      DIST  = Q(ISAPH+18)
      DERR  = Q(ISAPH+19)
C
      RETURN
      END
