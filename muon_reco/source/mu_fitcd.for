      SUBROUTINE MU_FITCD(LMUCD1,LZTRK1,ORENT,FITVTX,ERRVTX, 
     &  SLD,SLW,PFD,PFW,WCDD,WCDW,VTMUCD,TCHI_D,TCHI_W,ECD,ECW,NOFIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : KALMAN filter and fit CD hits
C-
C-   Inputs  : LZTRK1 - pointer to ZTRK,ORENT - orientation in muon system
C-             FITVTX-vertex point,ERRVTX-vertex errors                        
C-             TOLERCD,CW,RES_CD -tolerance and resolutions
C-             SLD,SLW - slopes
C-   Outputs : PFD-position and slope for drift cd,                            
C-             PFW-position and slope for WIRE cd,                             
C-             WCDD,WCDW -weight matrices for drift and wire,                  
C-             VTMUCD -vertex as a result of the cd fit
C-   Controls: 
C-
C-   Created   8-AUG-1991   A.Klatchko
C-   Updated  28-NOV-1991   A.Klatchko ERROR OPTIMIZATION AND OUTPUT MUCD 
C-   Updated   3-JAN-1992   A.Klatchko  ADD ERROR TO OUTPUT LIST 
C-   Updated   5-JAN-1992   A.Klatchko  ADD CHECK ON NUMBER OF CD HITS 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FIRST,KALMAN_CD,CD_ERROR,NOFIT
      DATA FIRST/.TRUE./
      INTEGER LZTRK1,ORENT,DETECTOR,NHITS,GOOD_HITS,IERZ,I
      REAL PFD(3),PFW(3),WCDD(2,2),WCDW(2,2),VTMUCD(3)
      REAL FITVTX(3),ERRVTX(3),HITX(65),HITY(65),HITZ(65), 
     & ERX(65),ERY(65),ERZ(65),HITD(65),HITW(65),HITP(65),ERD(65),      
     &  ERW(65),RES_CD(2),TOLERCW,TOLERCD,FACTOR_CD
      REAL TCHI_D,TCHI_W,ORIGIND,ORIGINW,SLD,SLW,CP,CD,CW,ECD,ECW
      REAL ORD,CHISD,ORW,CHISW,WCDD_I(2,2),WCDW_I(2,2),SUMP,SUMD,SUMW
      INTEGER FLAGD(65),FLAGW(65),LMUCD1
      DATA RES_CD/.25,0.02/
C----------------------------------------------------------------------
      NOFIT = .FALSE.
      IF(FIRST)THEN
        CALL EZGET_l('KALMAN_CD',KALMAN_CD,IERZ)
        CALL EZGET_l('CD_ERROR',CD_ERROR,IERZ)
        IF(KALMAN_CD)THEN
          CALL EZGET('TOLERCD',TOLERCD,IERZ)
          CALL EZGET('TOLERCW',TOLERCW,IERZ)
          CALL EZGET('FACTOR_CD',FACTOR_CD,IERZ)
        ENDIF
       FIRST = .FALSE.
      ENDIF
C  
      DO I=1,65
        FLAGD(I) = 0
        FLAGW(I) = 0
      ENDDO
C
      CALL CDTRHT(LZTRK1,DETECTOR,FITVTX,ERRVTX,HITX,HITY,HITZ, 
     &   ERX,ERY,ERZ,NHITS)       
      CALL ORDER_CDHITS(ORENT,NHITS,HITX,HITY,HITZ,ERX,ERY,ERZ,HITD,
     &  HITW,HITP,ERD,ERW,GOOD_HITS)
      IF(GOOD_HITS .LT. 3)THEN
        NOFIT = .TRUE.
        GO TO 999
      ENDIF
      IF(CD_ERROR)CALL CHECK_CDER(GOOD_HITS,RES_CD,ERD,ERW)
      CALL TRFITER(GOOD_HITS,RES_CD(1),ERD,HITP,HITD,ORD,SLD,
     &          CHISD,WCDD_I)
C
      WCDD_I(1,1) = WCDD_I(1,1)*FACTOR_CD 
      WCDD_I(1,2) = WCDD_I(1,1)*SQRT(FACTOR_CD) 
      WCDD_I(2,1) = WCDD_I(1,2) 
      WCDD_I(2,2) = WCDD_I(2,2)*FACTOR_CD 
C
      PFD(1) = HITD(GOOD_HITS)
      PFD(2) = SLD
      TCHI_D = CHISD
      IF(KALMAN_CD)THEN
        PFD(1) = HITD(1)
        CALL MUCD_KALMAN(WCDD_I,RES_CD(1),HITD,HITP,GOOD_HITS,ERD,
     &  TOLERCD,ORIGIND,TCHI_D,PFD,WCDD,FLAGD)  
      ENDIF
      CALL TRFITER(GOOD_HITS,RES_CD(2),ERW,HITP,HITW,ORW,SLW,
     &      CHISW,WCDW_I)
C
      WCDW_I(1,1) = WCDW_I(1,1)*FACTOR_CD 
      WCDW_I(1,2) = WCDW_I(1,1)*SQRT(FACTOR_CD) 
      WCDW_I(2,1) = WCDW_I(1,2) 
      WCDW_I(2,2) = WCDW_I(2,2)*FACTOR_CD 
C
      PFW(1) = HITW(GOOD_HITS)
      PFW(2) = SLW
      TCHI_W = CHISW
      IF(KALMAN_CD)THEN
        PFW(1) = HITW(1)
        CALL MUCD_KALMAN(WCDW_I,RES_CD(2),HITW,HITP,GOOD_HITS,ERW,
     &  TOLERCW,ORIGINW,TCHI_W,PFW,WCDW,FLAGW)
      ENDIF  
      CALL MUCDIN(ORENT,HITP,HITD,HITW,ERD,ERW,FLAGD,FLAGW,GOOD_HITS,
     &       LMUCD1,CP,CD,CW,ECD,ECW)
      IF(KALMAN_CD)THEN
        PFW(1) = CW
        PFW(3) = CP
        PFD(1) = CD
        PFD(3) = CP
      ELSE
        SUMP = 0.0
        SUMD = 0.0
        SUMW = 0.0
        DO I = 1,GOOD_HITS
          SUMP = SUMP + HITP(I)
          SUMD = SUMD + HITD(I)
          SUMW = SUMW + HITW(I)
        ENDDO
        SUMP=SUMP/GOOD_HITS
        SUMD=SUMD/GOOD_HITS
        SUMW=SUMW/GOOD_HITS
        PFW(1) = SUMW
        PFW(3) = SUMP
        PFD(1) = SUMD
        PFD(3) = SUMP
      ENDIF
  999 RETURN
      END
