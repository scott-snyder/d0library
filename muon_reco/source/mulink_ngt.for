      SUBROUTINE MULINK_NGT(IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :  IERR   I   error code  (always 0 for now)
C-   Controls:
C-
C-   Created  FROM MULINK.FOR 21-DEC-1990   S. ABACHI  DOES NOT HAVE GEANT
C-                                                      AND TRACKING
C-
C-   Created  13-FEB-1990   Shuichi Kunori
C-   Updated  29-MAR-1990   Shahriar Abachi
C-          To connect muon track with central detector, road for muon
C-          track was calculated.
C-   Updated  18-MAY-1990   Shahriar Abachi
C-           MUON and MUCD banks filled
C-   Updated  21-MAY-1990   Shahriar Abachi
C-          MUON bank partially filled.
C-   Updated  15-June-1990 Shuichi Kunori
C-      (1)  change to subroutine from logical function.
C-   Updated  24-JUN-1990   Shahriar Abachi
C-          THETA & PHI calculation corrected.
C-   Updated  25-JUN-1990   Shahriar Abachi
C-          If momentum funny declared error and set IERR = 1
C-   Updated  01-DEC-1990   Shahriar Abachi
C-          Only good muons processed (iflg=0), Muon bank was drastically
C-          Changed and filled. MUCD bank was filled with new information.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZTRLNK.INC/LIST'
      INCLUDE 'D0$INC:ZLINKA.INC/LIST'
      INCLUDE 'D0$INC:PI.DEF/LIST'
      INCLUDE 'D0$INC:MUPHYS.INC/LIST'
C  -- variables in input/output arguments...
      INTEGER IERR
C  -- local variables...
      INTEGER IVF1,NV,LL
C     -- indecies of temporary link area for MUOT,MUON,MUCD and
C     -- ZTRK banks.    MXZTRK is maximum number of ZTRAKs within
C     -- a muon road...
      INTEGER MXZTRK
      PARAMETER (MXZTRK=20)
      INTEGER IMUOT,IMUON,IMUCD,IZTRK(MXZTRK),KMUON(100),KMUOT(100)
      INTEGER ZLINKR(250)
C     -- number of tracks and bank addresses for central tracks...
      INTEGER NZTR,LMUOT,LMUON,LMUCD
C
      REAL XI,YI,ZI,XCOSIM,YCOSIM,ZCOSIM,CHSQBV,CHSQNB,MOM,MOMER
      REAL X01,Y01,Z01,X02,Y02,Z02,ZVER(5),DZVER(5)
      REAL PHI,THETA,XY
      REAL ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX,PT
      INTEGER GZMUOT
      INTEGER GZMUCD
      INTEGER GZMUON
      EXTERNAL GZMUOT
      EXTERNAL GZMUCD
      EXTERNAL GZMUON
      INTEGER J,I,LMED,IFLG1,IFLG2,NTRK,NTRK1,NTRK2
      REAL DPHI,DTHE, NSD
      REAL X(6)
      REAL EPS, XX, YY, ZZ, RR, R
      DATA EPS /1.0E-10/
C----------------------------------------------------------------------
C
C     -- return error code...
      IERR=0
      IVF1=0
      NTRK = 0
      NTRK1 = 0
      NTRK2 = 0
C
C  LOOP OVER MUOT TRACKS.
C  ======================
C
 1000 CONTINUE
C
      NTRK = NTRK + 1
      LMUOT = GZMUOT(NTRK)
      IF(LMUOT .EQ. 0) GO TO 999
C
C     -- Get a MUOT track...
C
C
C-------- FILL MUON BANK (SA.)
C
      CALL MFL_MUON1(LMUOT,LMUON)
C
 1999 CONTINUE
      GO TO 1000
C
  999 RETURN
      END
