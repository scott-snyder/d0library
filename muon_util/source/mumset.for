      SUBROUTINE MUMSET(ITRG,IREG,NMOD,JMOD,NQUAD,JQUAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO SET WHAT MODULES SHOULD HAVE their hits
C        converted from MUD1 to MUOH. also, decides which quadrants
C        should be looped over in tracking.  Uses trigger information
C        from level 1.0 and 1.5 as needed (bank MTRG)
C-
C-   Inputs  : ITRG  = trigger level to require:
C-                     0 = none (use all modules)
C-                     1 = level 1
C-                     2 = level 1.5 low pt
C-                     3 = level 1.5 high pt
C-             IREG  = region to require
C-                     1 = (y1) CF only
C-                     2 = (y2) CF and EF wamus
C-                     3 = (y3) CF and EF incl. overlap
C-                     4 = (y4) Full muon system
C-
C-   Outputs : NMOD  = number of modules
C-             JMOD  = module list
C-             NQUAD = number of quadrants
C-             JQUAD = quadrant list
C-
C-   Created  22-APR-1990   David Hedin
C-   DH 4/91 order quadrants differently
C-   DH 11/92 do central first
C-   MF 10/93 use new format including trigger bank
C-   MF 1-2/94 add SAMUS and overlap quadrants and MUHM bank
C-   MF 6/94 use MULAYR,MUQUAD to set SAMUS and overlap quadrants
C-   MF 3/95 call samus quadrants last
C-   MF 5/95 use new version MUMTRG
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ITRG,IREG,NMOD,JMOD(200),NQUAD,JQUAD(18)
      INTEGER I,J,NMUOF,NMUD1,JHIT,NCEL,NLAT,IADC(8)
      INTEGER MMOD,IMUD1,ITRUNC,NMUHP,IMUHP,JPLN,NMUOH,IMUOH
      INTEGER NMSCT,IMSCT,ISP1,ISP2,ITRIG,ICENFL,JCRS,JFINE(4),JLAT(3)
      INTEGER JREG,JTRG,JEND,IQUAD(18),JQ(18)
      INTEGER MULAYR,MUQUAD,KLAYR,KQUAD,KEND(4,3),KSAM,KSSW,KSWW
      EXTERNAL MULAYR,MUQUAD
      DATA JQ/6,5,7,8,10,9,11,12,1,2,3,4,17,18,15,16,13,14/
C
C             Initialize
C
      NMOD = 0
      DO I=1,18
          IQUAD(I)=0
          IF (I.LE.12) THEN
              J = (I-1)/3
              KEND(J+1,I-J*3) = 0
          ENDIF
      ENDDO
      CALL MUDMOD(NMOD,NMUOF,NMUHP,NMUD1)
      CALL MUDHIT(NMOD,JHIT,NCEL,NLAT,IADC)
C
C              Select modules by trigger request
C
      DO 100 I = 10,460
      CALL GTMUHM(I,MMOD,IMUD1,ITRUNC,NMUHP,IMUHP,JPLN,NMUOH,IMUOH,
     &            NMSCT,IMSCT,ISP1,ISP2,ITRIG,ICENFL,JCRS,JFINE,JLAT)
          IF (MMOD.EQ.0) GOTO 100
C
C              Test trigger region and level
C
          IF (JPLN.EQ.0) ITRIG=-1
          CALL MUMTRG(ITRIG,MMOD,JREG,JTRG)
          IF (IREG.GT.0.AND.JREG.GT.IREG) GOTO 100
          IF (IREG.LT.0.AND.JREG.LT.-IREG) GOTO 100
          IF (ITRG.GT.0.AND.ITRG.GT.JTRG) GOTO 100
C
C              Good module, set quadrant and module lists
C
          KLAYR = MULAYR(MMOD)
          IF (KLAYR.EQ.4) KLAYR=3
          KQUAD = MUQUAD(MMOD)
          JEND = 0
          IF (KQUAD.GE.4.AND.KQUAD.LE.8) JEND=1
          IF (KQUAD.GE.9.AND.KQUAD.LE.12) JEND=2
          IF (KQUAD.GT.12) JEND=KQUAD-10
          IF (JEND.NE.0) KEND(JEND,KLAYR)=1
C
          IF (JPLN.EQ.0) THEN                    ! New module to unpack
              NMOD = NMOD + 1
              JMOD(NMOD) = MMOD
              IF (JEND.NE.0) KEND(JEND,KLAYR)=2
              IF (MMOD.LT.310) IQUAD(KQUAD)=1    ! Set tracking quadrant
          ENDIF
C
 100  CONTINUE
C
C              See what quadrants are hit
C
      DO JEND = 1,2
        KSAM = KEND(JEND+2,1)*KEND(JEND+2,2)*KEND(JEND+2,3)
        KSSW = KEND(JEND+2,1)*KEND(JEND+2,2)*KEND(JEND,3)
        KSWW = KEND(JEND+2,1)*KEND(JEND,2)*KEND(JEND,3)
        IF ((IREG.GE.4.OR.IREG.LE.-3).AND.KSAM.GE.2) IQUAD(12+JEND)=1
        IF ((IREG.GE.3.OR.IREG.LE.-2).AND.KSSW.GE.2) IQUAD(14+JEND)=1
        IF ((IREG.GE.3.OR.IREG.LE.-2).AND.KSWW.GE.2) IQUAD(16+JEND)=1
      ENDDO
C
      NQUAD = 0
      DO I = 1,18
          J = JQ(I)
          IF (IQUAD(J).EQ.1) THEN
              NQUAD = NQUAD + 1
              JQUAD(NQUAD) = J
          ENDIF
      ENDDO
C
      RETURN
      END
