      SUBROUTINE GET_JETS1(JET_TYPE,NJETS_MAX,ET_CUT,JSIZ,P23,NJETS,JET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  gets kinematic quantities of jets selected using
C-   Particle_select.
C-
C-   Inputs  : JET_TYPE  [C]   Jet algorithm name as in particle_selct.rcp
C-                             'TOP_JETS' etc
C-             NJETS_MAX [I]   Maximum number of jets to output in P23(23,*)
C-             ET_CUT    [R]   ET cut for jets
C-             JSIZ            number of words buffered for each jet
C-   Outputs : P23(31,*) [R]
C-
C-              P(1..9)
C-                            PX, PY, PZ, E, ET, ETA, PHI, DET_ETA, QUALITY
C-                    I = 9
C-              P(I+1)    Sig**2(Ex)
C-              P(I+2)    Sig**2(Ey)
C-              P(I+3)    RMS Eta width
C-              P(I+4)    RMS Phi width
C-              P(I+5)    Fraction of EM Et
C-              P(I+6)    Flag for merging/splitting
C-              P(I+7)    Number of cells above threshold (1GeV def)
C-              P(I+8)    Fraction of ICD/MG Et(EtICD+EtMG/TOTAL_ET)
C-              P(I+9)    Fraction of CH Et (Et CH/TOTAL_ET)
C-              P(I+10)   Ratio of hottest to next-hottest cell
C-              P(I+11)   Number of CDC tracks in jet cone
C-              P(I+12)   Number of TOWERS comprising 90% of jet Et
C-              P(I+13)   ConeSize (-1.0 for Nearest Neighbor)
C-              P(I+14)   Phi Difference between MET and JET (PNUT(2))
C-              P(I+15)   Spare
C-              P(I+16)   Energy Correction Flag (I)
C-              P(I+17)   Sig**2(Ez)
C-              P(I+18)   dExdEy
C-              P(I+19)   dExdEz
C-              P(I+20)   dEydEz
C-              P(I+21)   Change in missing ET X component due to Jet energy
C-                        scale re-calibration.
C-              P(I+22)   Change in missing ET Y component due to Jet energy
C-                        scale re-calibration.
C-
C-
C-             NJETS     [I]   Total number of jets surviving ET cut,
C-                             but not exceeding MAXJETS
C-             JET       [L]   TRUE IF NJETS > 0
C-   Controls:
C-
C-   Created   7-NOV-1994   Rajendran Raja   
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NJETS_MAX,NJETS
      INTEGER JSIZ
      REAL    ET_CUT,P23(JSIZ,*)
      LOGICAL JET
      INTEGER NSIZE
      INTEGER I,IP
C
      CHARACTER*(*) JET_TYPE
C----------------------------------------------------------------------
      CALL NOBJ_JETS1(JET_TYPE,NJETS,NSIZE)
      IF ( NSIZE.GT.(JSIZ-2) ) THEN
        CALL ERRMSG('CALORIMETER','GET_JETS1',
     &    ' Space allocation too small in GET_JETS1','W')
      ENDIF
      NJETS = MIN(NJETS,NJETS_MAX)
      IP = 0
      DO I = 1 , NJETS
        IP = IP + 1
        CALL OBJECT_JET1(JET_TYPE,I,(JSIZ-2),P23(1,IP))
        IF ( P23(5,IP).LT.ET_CUT ) THEN
          IP = IP -1   !JET BELOW ET
        ENDIF
      ENDDO
      NJETS = IP
      IF (NJETS .GT. 0) JET = .TRUE.
  999 RETURN
      END
