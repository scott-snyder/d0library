      SUBROUTINE MUREFIT_MUON(NTRK, MOM, X, NV, LMUON)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To calculate and fill MUON bank after refit
C-
C-   Inputs  :  NTRK     : track number
C-              MOM      : Muon momentum.
C-              X(1:3)   : Starting point coordinates. (vertex for now)
C-              X(4:6)   : Direction cosines of Muon.
C-              NV       : Vertex number chosen (if any)
C-
C-   Outputs :  LMUON    : Address of MUON bank
C-   Controls:
C-
C-   Created  15-mar-1993 D. Wood, based on MUONFL
C-   Updated   5-OCT-1994   Daria Zieminska   preset word 31 (chisq/ndf)
C-                          to -1. Will be filled with GFIT chisq later
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LMFIT, IMFIT, NTRK
      REAL MOM, X(6)
      REAL PMU(3)
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:MUPHYS.INC/LIST'
      INTEGER I, LMED,NDMUON,ND_V3,NPUSH
      PARAMETER(ND_V3=63)
      INTEGER LMUON, GZMFIT, GZMUON, GZVERT ,NS, NV
      INTEGER LMUOT, NS2, GFIT, IER
      REAL PMUN(3),P, PT, PMUO, LVERT
      REAL AMASS,DEEF,EM,PMN
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      DATA AMASS/.105/
C
      IF(FIRST) THEN
        CALL EZGET('GFIT',GFIT,IER)
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
      IF(NV .GT. 0) THEN
        LVERT = GZVERT(NV)
        LQ(LMUON - NS - 2) = LVERT
      ELSE
        LQ(LMUON - NS - 2) = 0
      ENDIF
C
C      CALL MUCAL(MOM,X,LMFIT,DECALM_EM,DECALM,DEERR,PMUN,MUISOL,MCOK)
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
C        Q(LMUON + 23) = 100.   !will be filled in mulink
        Q(LMUON + 24)  = DECAL
        Q(LMUON + 25)  = DEMUO
C
      DO I=1,5
        Q(LMUON + 25 + I) = Q(LMFIT + 24 + I)
      ENDDO
C      Q(LMUON + 31) = Q(LMFIT + 30)
      Q(LMUON + 31) = -1.
C
      DO I=1,3
        Q(LMUON + 36 + I) = Q(LMFIT + 10 + I)
      ENDDO
C
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
      IQ(LMUON + 63) = NV
C
  999 RETURN
      END
