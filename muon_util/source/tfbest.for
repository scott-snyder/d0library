      SUBROUTINE TFBEST(NPT,TFIN,CSIN,NDIN,TMIN,ERRT,CSMIN,NDOUT,IRC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Called by MUTFLT.  Finds best time shift
C-                         and its error given the chisquared values
C-                         at discreet points 
C-
C-   Inputs  : NPT = number of time points
C-             TFIN() = array of NPT time shifts
C-             CSIN() = array of chisquared values
C-             NDIN() = array of numbers of degrees of freedom
C-   Outputs : TMIN = best time shift value (interpolated)
C-             ERRT = one-sigma error on TMIN
C-             CSMIN = chisquared value at TMIN 
C-             NDOUT = number of degrees of freedom
C-             IRC = return code-- 0=good parabolic interpolation
C-                                 1=only two points used, error suspect
C-                                 2=only one point used, no error estimate
C-                                 3=ill-determined parabola, no error est
C-                                -1=FAILURE, all NDOUT<=0 (no fits)
C-              
C-   Controls: 
C-
C-   Created  20-AUG-1992   Darien R. Wood
C-   Revised  04-JUL-1995   DRW - add new IRC=-1
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NTMAX
      PARAMETER(NTMAX=40)
      REAL TFIN(NTMAX),CSIN(NTMAX),TMIN,ERRT,CSMIN
      INTEGER NPT,NDIN(NTMAX),IRC,NDOUT,IMIN,I,NDMAX,NTOSS
      REAL TFPAR(3),CSPAR(3),CSBEST,CPDOF,CPDOFBEST
      REAL DT12,DT32,DTQ12,DTQ32,DCS12,DCS32,DET0,DETA,DETB
      DATA NTOSS/0/
C----------------------------------------------------------------------
      IRC = 0
      CALL VZERO(CSPAR,3)
      CALL VZERO(TFPAR,3)
C first pass, find max ndof
      NDMAX = 0
      DO I=1,NPT
        IF(NDIN(I).GE.NDMAX) NDMAX = NDIN(I)
      ENDDO
      NDOUT = NDMAX
      IF(NDMAX.LE.0) THEN
C no real fits, results are nonsense.  Fill output variable 
C just for neatness and set failure return code
        TMIN = TFIN(1)
        ERRT = 0.
        CSMIN = CSIN(1)
        NDOUT = 0
        IRC = -1
        GOTO 999
      ENDIF
C second pass, find min chisq/dof for within 1 dof of max ndof
      CSBEST = 999999.
      CPDOFBEST = 999999.
      IMIN = 0
      DO I=1,NPT
        IF(NDIN(I).GE.NDMAX-NTOSS .AND. NDIN(I).GT.0) THEN
          CPDOF = CSIN(I)/NDIN(I)
          IF(CPDOF.LT.CPDOFBEST) THEN
            CPDOFBEST = CPDOF
            IMIN = I
          ENDIF
        ENDIF
      ENDDO
      IF(IMIN.LE.0) THEN
C no real fits, results are nonsense.  Fill output variable 
C just for neatness and set failure return code
        TMIN = TFIN(1)
        ERRT = 0.
        CSMIN = CSIN(1)
        NDOUT = 0
        IRC = -1
        GOTO 999
      ENDIF
      CSPAR(2) = CSIN(IMIN)/NDIN(IMIN)
      TFPAR(2) = TFIN(IMIN)
C examine points on either side
      IF(IMIN.GE.2) THEN
        IF(NDIN(IMIN-1).GE.NDMAX-NTOSS .AND. NDIN(IMIN-1).GE.1) THEN
          CSPAR(1) = CSIN(IMIN-1)/NDIN(IMIN-1)
          TFPAR(1) = TFIN(IMIN-1)
        ENDIF
      ENDIF
      IF(IMIN.LE.NPT-1) THEN
        IF(NDIN(IMIN+1).GE.NDMAX-NTOSS .AND. NDIN(IMIN+1).GE.1) THEN
          CSPAR(3) = CSIN(IMIN+1)/NDIN(IMIN+1)
          TFPAR(3) = TFIN(IMIN+1)
        ENDIF
      ENDIF
C handle anomalous cases (less than 3 points with NDMAX)
      IF(CSPAR(1).EQ.0.) THEN
        IF(CSPAR(3).EQ.0.) THEN
          IRC = 2
          CSMIN = CSPAR(2)*NDIN(IMIN)
          TMIN = TFPAR(2)
          ERRT = 0.
        ELSE
          IRC = 1
          CSMIN = CSPAR(2)*NDIN(IMIN)
C 2 points: try to do a crude interpolation
          IF(CSPAR(2)+CSPAR(3).GT.0.) THEN
            TMIN = (CSPAR(2)*TFPAR(3)+CSPAR(3)*TFPAR(2))/(CSPAR(2)+
     &        CSPAR(3))
          ELSE
            TMIN = 0.5*(TFPAR(2)+TFPAR(3))
          ENDIF  
          ERRT = 0.5*ABS(TFPAR(3)-TFPAR(2))
        ENDIF
      ELSEIF(CSPAR(3).EQ.0.) THEN  
        IRC = 1
        CSMIN = CSPAR(2)*NDIN(IMIN)
C 2 points: try to do a crude interpolation
        IF(CSPAR(1)+CSPAR(1).GT.0.) THEN
          TMIN = (CSPAR(1)*TFPAR(2)+CSPAR(2)*TFPAR(1))/(CSPAR(1)+
     &      CSPAR(2))
        ELSE
          TMIN = 0.5*(TFPAR(1)+TFPAR(2))
        ENDIF  
        ERRT = 0.5*ABS(TFPAR(2)-TFPAR(1))
      ENDIF
      IF(IRC.NE.0) GOTO 999
C 3 good points, do parabolic solution
      DCS12 = CSPAR(1) - CSPAR(2)
      DCS32 = CSPAR(3) - CSPAR(2)
      DT12 = TFPAR(1) - TFPAR(2)
      DT32 = TFPAR(3) - TFPAR(2)
      DTQ12 = TFPAR(1)**2 - TFPAR(2)**2
      DTQ32 = TFPAR(3)**2 - TFPAR(2)**2
      DET0 = DTQ12*DT32 - DTQ32*DT12
      DETA = DCS12*DT32 - DCS32*DT12
      DETB = DTQ12*DCS32 - DTQ32*DCS12
      IF(DETA.NE.0.) THEN
        IRC = 0
        TMIN = -0.5*DETB/DETA
        IF(DET0*DETA.GT.0.) THEN
          ERRT = SQRT(DET0/DETA)
          CSMIN = CSPAR(2) - ((TMIN-TFPAR(2))/ERRT)**2
          CSMIN = CSMIN*NDIN(IMIN)
        ELSE
          IRC = 3
          ERRT = 0.
        ENDIF
      ELSE
        IRC = 2
        TMIN = CSPAR(2)
        ERRT = 0.
      ENDIF  
C          
C
  
  999 RETURN
      END
