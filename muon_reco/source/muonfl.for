      SUBROUTINE MUONFL(NTRK, MOM, X, IV, LMUON)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To calculate and fill MUON bank
C-
C-   Inputs  :  NTRK     : track number
C-              MOM      : Muon momentum.
C-              X(1:3)   : Starting point coordinates. (vertex for now)
C-              X(4:6)   : Direction cosines of Muon.
C-              IV       : Vertex number chosen (if <0 then only save in
C-                         bank & do not propagate
C-
C-   Outputs :  LMUON    : Address of MUON bank
C-   Controls:
C-
C-   Created  23-MAY-1990   SHAHRIAR ABACHI
C-   D. Hedin 1-91 new MFIT format
C-   Updated  31-JAN-1991   SHAHRIAR ABACHI     Isolation added, format changed
C-   Updated  08-APR-1991   SHAHRIAR ABACHI     Angle between MU&CD introduced
C-   Updated  03-JUL-1991   SHAHRIAR ABACHI     Link to ZTRK added
C    DH 12-23-91  REMOVE PUTTING LMFIT INTO MUOT STRUCTIRAL LINK
C                 also IQ not LQ for NS definition
C-   Updated  21-FEB-1992   SHAHRIAR ABACHI     Link to MUOT added
C-   Updated  09-JUN-1992   SHAHRIAR ABACHI     IFLG upto 2 accepted
C-   Update   03-DEC-1992   Shuichi Kunori      new argument in MUCAL call
C-                                              and store EM energy in
C-                                              muon bank.
C-   Updated  06-JAN-1993   SHAHRIAR ABACHI     Vertex number added
C-   Updated  26-JAN-1993   SHAHRIAR ABACHI     more cal info added
C-   Updated  11-MAR-1993   Daria Zieminska   don't fill word 4
C-                          (it is done in the fitting routine)
C-   Updated  12-APR-1993   Daria Zieminska set Q(LMUON+31) to -1.
C-                          the fit result is filled in MCGLOBAL and MFGLOBAL
C-   Updated  19-apr-1993   SHAHRIAR ABACHI   If (IV <0) then do not propagate
C-   Updated  12-OCT-1993   Daria Zieminska   Call MUTFLT and fill the word
C-   Updated  24-JAN-1994   I.Mandrichenko    Do not recalculate elos for
C-					      SAMUS tracks
C-   Updated  11-FEB-1995   M. Fortner        Remove unused variables
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LMFIT,NTRK
      REAL MOM, X(6)
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:MUPHYS.INC/LIST'
      INCLUDE 'D0$INC:ZLINKA.INC/LIST'
      INTEGER I,NDOF,IRC
      REAL DECALM_EM(10),DECALM(10),DEERR,DELT,EDELT,CHISQ
      INTEGER LMUON, GZMFIT, GZMUON, GZVERT ,NS, IV, NV
      INTEGER IMUOT,LMUOT, NS2, GFIT, IER
      REAL PMUN(3),MUISOL(10),PT, PMUO, LVERT
      LOGICAL FIRST, MCOK
      DATA FIRST /.TRUE./
C
      IF(FIRST) THEN
        CALL EZGET('GFIT',GFIT,IER)
        FIRST = .FALSE.
      ENDIF
C
      LMUON = GZMUON(NTRK)
      NS = IQ(LMUON - 2)
      LMFIT = GZMFIT(NTRK)
      NS2 = IQ(LMFIT - 2)
      LMUOT = LQ(LMFIT - NS2 - 1)
      LQ(LMUON - NS - 1) = LMUOT
C---
      IF(IV .LT. 0) THEN
        NV = 1
      ELSE
        NV = IV
      ENDIF
C
      IF(NV .NE. 0) THEN
        LVERT = GZVERT(NV)
        LQ(LMUON - NS - 2) = LVERT
      ELSE
        LQ(LMUON - NS - 2) = 0
      ENDIF
      LQ(LMUON - NS - 3) = 0  ! Will be filled in mulink
C
      CALL MUCAL(MOM,X,LMFIT,DECALM_EM,DECALM,DEERR,PMUN,MUISOL,MCOK)
      IF( IQ(LMFIT+4).EQ.13 .OR. IQ(LMFIT+4).EQ.14 ) THEN
	PMUN(1) = X(4)*MOM
	PMUN(2) = X(5)*MOM
	PMUN(3) = X(6)*MOM
      END IF
C
      IQ(LMUON + 1) = 3   !Bank version
C
      IF(IQ(LMFIT + 10) .LT. 0) THEN
        IQ(LMUON + 2) = 14
      ELSE
        IQ(LMUON + 2) = -14
      ENDIF
C - dedx flag
      IQ(LMUON + 3) = MFLG
C - number of CD tracks
CC      IQ(LMUON + 5) = NMTR    ! Done inside MULINK for now
C - quality flag
      IQ(LMUON + 6) = IQ(LMUOT + 7)  ! To be defined more accurately later
C - Kink flag
      IQ(LMUON + 7) = 0
C - Vertex flag
      IF(IVRT .EQ. 1) THEN
        IQ(LMUON + 8) = 1
      ELSEIF(IVRT .EQ. 0) THEN
        IQ(LMUON + 8) = 2
      ELSE
        IQ(LMUON + 8) = 3
      ENDIF
C - Quad
      IQ(LMUON + 9) = IQ(LMFIT + 4)
C - Method of fit
      IF(IQ(LMFIT+7) .EQ. 3) THEN
        IQ(LMUON + 10) = 7
      ELSE
        IQ(LMUON + 10) = 2 * IQ(LMFIT + 7) + IQ(LMFIT + 5) - 1
      ENDIF
C
C - momentum and angles
      PMUO = SQRT(PMUN(1)**2 + PMUN(2)**2 + PMUN(3)**2 )
      DO I=1,3
        Q(LMUON + 10 + I) = PMUO * X(I + 3)
      ENDDO
      PT = SQRT((PMUO*X(4))**2 + (PMUO*X(5))**2)
      Q(LMUON + 14) = PMUO
      Q(LMUON + 15) = PT
      Q(LMUON + 16) = MUTHE
      Q(LMUON + 17) = MUETA
      Q(LMUON + 18) = MUPHI
C
      Q(LMUON + 19) = TRADLCA
      Q(LMUON + 20) = TRADLMU
      Q(LMUON + 21) = MSACA
      Q(LMUON + 22) = MSAMU
      IF(MCOK) THEN
        Q(LMUON + 23) = 100.   !will be filled in mulink
        Q(LMUON + 24)  = DECAL
        Q(LMUON + 25)  = DEMUO
      ENDIF
C
      DO I=1,5
        Q(LMUON + 25 + I) = Q(LMFIT + 24 + I)
      ENDDO
C      Q(LMUON + 31) = Q(LMFIT + 30)
C
C  Initialise chisq/df ( the fitting result will be filled after global
C  fitting is successfully performed)
C
      IF( IQ(LMFIT+4).EQ.13 .OR. IQ(LMFIT+4).EQ.14 ) THEN
	Q(LMUON + 31) = Q(LMFIT + 30)	! May be recalculated in Global Fit
      ELSE
	Q(LMUON + 31) = -1.
      ENDIF
C
      IF(MCOK) THEN
        DO I=1,5
          Q(LMUON + 31 + I) = MUISOL(I)
        ENDDO
      ENDIF
C
      DO I=1,3
        Q(LMUON + 36 + I) = Q(LMFIT + 10 + I)
      ENDDO
C
      Q(LMUON + 40) = 270.   !will be filled in mulink (with AGMUCD)
      Q(LMUON + 41) = 270.   !will be filled in mulink
      Q(LMUON + 42) = 270.   !will be filled in mulink
C - Energy in cones...
      DO I=1,3
        Q(LMUON + 42 + I)  = DECALM(I+2)
      ENDDO
      DO I=1,5
        Q(LMUON + 55 + I)  = DECALM_EM(I)
      ENDDO
      DO I=1,2
        Q(LMUON + 60 + I)  = DECALM(I)
      ENDDO
C - TOF
      IMUOT=IQ(LMUOT-5)
      CALL MUTFLT(IMUOT,DELT,EDELT,CHISQ,NDOF,IRC)
      Q(LMUON + 46) = DELT
C - Middle of iron
      DO I=1,3
        Q(LMUON + 46 + I) = Q(LMFIT + 13 + I)
      ENDDO
C - Direction cosines after magnet
      DO I=1,3
        Q(LMUON + 49 + I) = Q(LMFIT + 16 + I)
      ENDDO
C - Muon impact parameter
      Q(LMUON + 53) = -999.   !will be filled in mulink
      Q(LMUON + 54) = -999.   !will be filled in muonflg
C - B mean
      Q(LMUON + 55) = Q(LMFIT + 31)
C - Vertex number
      IQ(LMUON + 63) = IABS(IV)
C
  999 RETURN
      END
