      SUBROUTINE MUREFIT_MUON(NTRK, MOM, X, IV, LMUON)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To calculate and fill MUON bank after refit
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
C-   Created  15-mar-1993 D. Wood, based on MUONFL
C-   Updated   5-OCT-1994   Daria Zieminska   preset word 31 (chisq/ndf)
C-                          to -1. Will be filled with GFIT chisq later
C-   Updated  30-sep-1995 D. Wood, explicitly set IQ(LMUON+4)=0
C-   Updated  16-oct-1995 D. Wood, copy vertex logic of MUONFL and do call
C-                                 to MUCAL if CALREFIT.GT.0
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LMFIT, NTRK
      REAL MOM, X(6)
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:MUPHYS.INC/LIST'
      INTEGER I, NDMUON,ND_V3,NPUSH
      REAL DECALM_EM(10),DECALM(10),DEERR,MUISOL(10)
      PARAMETER(ND_V3=63)
      INTEGER LMUON, GZMFIT, GZMUON, GZVERT ,NS, IV, NV
      INTEGER LMUOT, NS2, GFIT, IER, CALREFIT
      REAL PMUN(3),PT, PMUO, LVERT
      REAL AMASS,DEEF,EM,PMN
      LOGICAL FIRST, MCOK
      DATA FIRST /.TRUE./
      DATA AMASS/.105/
C
      IF(FIRST) THEN
        CALL EZGET('GFIT',GFIT,IER)
        CALL EZGET('CALREFIT',CALREFIT,IER)
        FIRST = .FALSE.
      ENDIF
C
      LMUON = GZMUON(NTRK)
C check bank size, and push if necessary
      NDMUON = IQ(LMUON-1)
      IF(NDMUON.NE.ND_V3) THEN
        NPUSH = ND_V3 - NDMUON
        CALL MZPUSH(IXMAIN,LMUON,0,NPUSH,' ')
      ENDIF
      IQ(LMUON + 1) = 3   !Bank version
C
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
      IF(NV .GT. 0) THEN
        LVERT = GZVERT(NV)
        LQ(LMUON - NS - 2) = LVERT
      ELSE
        LQ(LMUON - NS - 2) = 0
      ENDIF
C
      MCOK = .FALSE.
      IF(CALREFIT.GT.0) THEN
        CALL MUCAL(MOM,X,LMFIT,DECALM_EM,DECALM,DEERR,PMUN,MUISOL,MCOK)
        IF( IQ(LMFIT+4).EQ.13 .OR. IQ(LMFIT+4).EQ.14 ) THEN
          PMUN(1) = X(4)*MOM
          PMUN(2) = X(5)*MOM
          PMUN(3) = X(6)*MOM
        END IF
      ELSE
CDRW normally, this stuff is done inside MUCAL
        IF(IQ(LMFIT + 7) .EQ. 3) THEN
          DEEF = DECAL + 0.5 * DEMUO
        ELSE
          DEEF = DECAL + DEMUO
        ENDIF
        EM = SQRT(MOM**2 + AMASS**2) + DEEF
        PMN = SQRT(EM**2 - AMASS**2)
        DO I=1,3
          PMUN(I) = PMN * X(I+3)
        ENDDO
      ENDIF
C
      IF(IQ(LMFIT + 10) .LT. 0) THEN
        IQ(LMUON + 2) = 14
      ELSE
        IQ(LMUON + 2) = -14
      ENDIF
C - dedx flag
      IQ(LMUON + 3) = MFLG
C - fit status (clear it; it will be set in MCGLOBAL or MFGLOBAL
      IQ(LMUON + 4) = 0
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
C        Q(LMUON + 23) = 100.   !will be filled in mulink
      Q(LMUON + 24)  = DECAL
      Q(LMUON + 25)  = DEMUO
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
        Q(LMUON + 31) = Q(LMFIT + 30)   ! May be recalculated in Global Fit
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
C - Energy in cones...
      IF(MCOK) THEN
        DO I=1,3
          Q(LMUON + 42 + I)  = DECALM(I+2)
        ENDDO
        DO I=1,5
          Q(LMUON + 55 + I)  = DECALM_EM(I)
        ENDDO
        DO I=1,2
          Q(LMUON + 60 + I)  = DECALM(I)
        ENDDO
      ENDIF
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
