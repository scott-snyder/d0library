      SUBROUTINE ZVCFIT(NTRACK,ZV,EZV,CHISQ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Perform a z-vertex fit constraining all tracks to
C-                         originate from a single vertex point.
C-                     Tracks are accepted based on the flag set in DTRK bank
C-
C-   Inputs  : NTRACK = Num of tracks to fit.
C-   Outputs : ZV = Z vertex calculated
C-             EZV = Error in Z position
C-             CHISQ  = Chisquare of the global fit per DOF
C-   Controls: none
C-
C-   Created  12-MAY-1993   Srini Rajagopalan
C-   Updated   6-DEC-1993   Srini Rajagopalan  Rename subroutine name from ZVFIT
C-                          to ZVCFIT to avoid conflict with common block name.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INCLUDE 'D0$INC:ZVFIT.INC'
C
      INTEGER IER
      INTEGER LDTRK,GZDTRK,LZFIND
C
C Parameter definitions to define boundaries
C
      INTEGER MAX_DL
      PARAMETER (MAX_DL=8)            ! Maximum number of delay line hits
C
C Hit dependent parameters
C
      INTEGER NHIT,IHIT,JHIT,IDL,NZHIT,NREJ
      REAL    R(MAX_DL),Z(MAX_DL),EZ(MAX_DL)
C
C Track dependent parameters
C
      INTEGER NJTRK,ITRK,JTRK,NTRACK,NDOF
      REAL    TSXY(MAX_TRACK),TSXX(MAX_TRACK),TSX(MAX_TRACK)
      REAL    TSWT(MAX_TRACK),TSY(MAX_TRACK),WT,DEL,SSY
      REAL    MAT(MAX_TRACK*MAX_TRACK),FINDEX(MAX_TRACK)
      REAL    CONST(MAX_TRACK),ZFIT
      REAL    NUMER,DENOM
      REAL    COEF2
C
      REAL ZV,EZV,CHISQ
C
C----------------------------------------------------------------------
C
C init every event call...
C
      IER = 0
      ZV  = 999.0
      EZV = 0.0
C
      CALL VZERO(TSLOPE(1),MAX_TRACK)
      CALL VZERO(TERR_INT(1),MAX_TRACK)
      CALL VZERO(TERR_SLOPE(1),MAX_TRACK)
C
C Loop over all tracks.
C
      NJTRK = 0
      SSY = 0.0
      DEL = 0.0
C
      DO 10 ITRK = 1,NTRACK
        IF (INDEX(ITRK).EQ.0) THEN
          NJTRK = NJTRK + 1
          TPID(NJTRK) = PID(ITRK)
          TSXY(NJTRK) = SXY(ITRK)
          TSXX(NJTRK) = SXX(ITRK)
          TSX(NJTRK) = SX(ITRK)
          TSWT(NJTRK) = SWT(ITRK)
          TSY(NJTRK) = SY(ITRK)
          DEL = TSWT(NJTRK) + DEL
          SSY = TSY(NJTRK) + SSY
        ENDIF
   10 CONTINUE                          ! Loop over tracks
C
C
C Compute the Matrix elements
C
      DO ITRK = 1,NJTRK
        DO JTRK = 1,NJTRK
          IF (ITRK.EQ.JTRK) THEN
            MAT((ITRK-1)*NJTRK + JTRK) =
     &                      TSXX(JTRK) - (TSX(JTRK)*TSX(JTRK)/DEL)
          ELSE
            MAT((ITRK-1)*NJTRK + JTRK) = -1.0*TSX(JTRK)*TSX(ITRK)/DEL
          ENDIF
        ENDDO
      ENDDO
C
C Invert the Matrix elements
C
      CALL RINV(NJTRK,MAT,NJTRK,FINDEX,IER)
      IF (IER.NE.0) THEN
        CALL ERRMSG('Matrix Inversion Failed','ZMINIM',
     &    'Singular Matrix','W')
        IER = -2
        GO TO 999
      ENDIF
C
C Compute the slopes of all the tracks.
C
      DO JTRK = 1,NJTRK
        CONST(JTRK) = TSXY(JTRK) - SSY*TSX(JTRK)/DEL
        DO ITRK = 1,NJTRK
          TSLOPE(ITRK) = CONST(JTRK)*MAT(NJTRK*(ITRK-1)+JTRK) +
     &                  TSLOPE(ITRK)
        ENDDO
      ENDDO
C
C Compute the Z vertex position, using the equation of any (first) track.
C
      ZV = (TSXY(1) - TSLOPE(1)*TSXX(1))/TSX(1)
C
C Calculate the chisquare of all tracks with the constrained fit.
C
      JTRK = 0
      NDOF = 0
      CHISQ = 0.
C
      DO ITRK = 1,NJTRK
        LDTRK = LZFIND(IXCOM,GZDTRK(1),TPID(ITRK),-5)
        CALL DLINFO(LDTRK,NZHIT,R,Z,EZ,IER)
        DO IDL = 1,MAX_DL
          IF (EZ(IDL).GT.0.0) THEN
            ZFIT = TSLOPE(ITRK)*R(IDL) + ZV
            CHISQ = ((Z(IDL) - ZFIT)**2)*EZ(IDL)+CHISQ
            NDOF = NDOF + 1
          ENDIF
        ENDDO
      ENDDO
      NDOF = NDOF - 2*NJTRK + (NJTRK-1)
      CHISQ = CHISQ/FLOAT(NDOF)
C
C Compute the errors on the slopes and the Z vertex position
C
      DO JTRK = 1,NJTRK
C
C Error on slope
C
        NUMER = TSXX(JTRK) +
     &                      TSWT(JTRK)*((TSX(JTRK)/DEL)**2) -
     &                      2.*TSX(JTRK)**2/DEL
C
        DENOM = (TSXX(JTRK) - (TSX(JTRK)**2)/DEL)**2
C
        TERR_SLOPE(JTRK) = SQRT(ABS(NUMER)/DENOM)
C
C Error on intercept
C
        COEF2 = (DEL - TSX(JTRK)**2/TSXX(JTRK))**2
        TERR_INT(JTRK) = TSWT(JTRK) + (3.0*(TSX(JTRK)**2)/TSXX(JTRK))
        TERR_INT(JTRK) = TERR_INT(JTRK)/COEF2
        EZV = TERR_INT(JTRK) + EZV
      ENDDO
      EZV = SQRT(EZV)
C
  999 RETURN
      END
