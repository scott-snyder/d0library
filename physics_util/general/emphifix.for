      LOGICAL FUNCTION EMPHIFIX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :   Package to "FIX" the Phi information 
C-
C-   Returned value  : Always returns true
C-   Inputs  :         None
C-   Outputs :         None
C-
C-   Created  29-AUG-1993   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER IER,I
      INTEGER LEVEL,  GZPELC, GZPPHO
      INTEGER LCLUS, LHSTR
      REAL    XBAR3(3),PHI
      INTEGER GZHSTR
      LOGICAL PHOTON,ELEC,OK
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C ****
C
      EMPHIFIX = .TRUE.
C
C
C ****  CHECK ON D0RECO VERSIONS, correct ONLY for D0RECO production
C ****  VERSIONS below 10.12
C
        LHSTR = GZHSTR()
        IF (IQ(LHSTR+3).GE.10) THEN
          IF (IQ(LHSTR+3).GE.12) THEN
            GOTO 999
          ENDIF
        ENDIF
C
C ****  Loop over all electrons/photons banks in the event
C
      LCLUS  = GZPELC()
      ELEC   = .TRUE.
      PHOTON = .FALSE.
      IF (LCLUS.EQ.0) THEN
        LCLUS  = GZPPHO()
        ELEC   = .FALSE.
        PHOTON = .TRUE.
      END IF

      DO WHILE(LCLUS .NE. 0)
C
C ****  Get PELC/PPHO shower centroid information
C
        IF (IQ(LCLUS+1).EQ.2) THEN               ! Bank version
          IF (PHOTON) THEN
            CALL UCOPY(Q(LCLUS+20),XBAR3,3)   !x,y,z
          ELSE
C
C **** Electrons
C
            CALL UCOPY(Q(LCLUS+23),XBAR3,3)   !x,y,z
          ENDIF
C
C **** Compute Phi
C
          PHI = ATAN2(XBAR3(2),XBAR3(1))    
          IF (PHI.LT.0) PHI = PHI + TWOPI
C
C ****  Fill Phi
C
          Q(LCLUS+10) = PHI
        ENDIF
        LCLUS = LQ(LCLUS)
        IF (LCLUS.EQ.0 .AND. .NOT. PHOTON) THEN
          LCLUS  = GZPPHO()
          ELEC   = .FALSE.
          PHOTON = .TRUE.
        END IF
C
      END DO
C
  999 RETURN
      END
