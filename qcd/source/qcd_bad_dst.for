      SUBROUTINE QCD_BAD_DST( IJET, BAD_FLAG )
      ENTRY QCD_GOOD_JET( IJET, BAD_FLAG )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the BAD_FLAG for a jet from the DST
C-                         or MDST.  Warning! You must set the path with
C-                         SET_CAPH before you call this routine!
C-
C-   Inputs  : IJET     [I] Ith jet in loop over jets
C-   Outputs : BAD_FLAG [I] 0 = OK, Good jet
C-                         -1 = error 
C-                         >0 = Failed one or more good jet cut:
C-                          BAD_FLAG = Bit mask flag
C-                          bit 1 set = Failed CH fraction cut (CHF > .40 )
C-                          bit 2 set = Failed Hot cell cut    (HCF < .10 )
C-                          bit 3 set = not available in DST
C-                          bit 4 set = Failed EM fraction cut (EMF > .95 
C-                                                              or EMF < .05 )
C-   Controls:
C-
C-   Created  17-DEC-1992   Richard V. Astur
C-   Modified  5-JAN-1993   Andrew G. Brandt use GTJETS_ALL 
C-   Modified 13-FEB-1993   R. Astur (Disable if JETS is version 2 or less)
C-   Modified 25-JUN-1993   R. Astur EMFRAC .05-.95 instead of .1-1.
C-                          don't exclude ICD from EMFRAC cut
C-   Modified 29-NOV-1994   M. Bhattacharjee "Change so ICD cut matches
C-                          Daniel/Mrinmoy note, use detector eta"
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IJET,BAD_FLAG
C
      INTEGER IJ,IVERS,ISPL,NCELL,IER
      REAL    E(5),SIG(4),HOT(3)
      REAL    THETA,PHI,ETA,EMFRAC
      REAL    PETA_TO_DETA,ZVERT
      REAL    DETA
C
      REAL CH_FRACTION_MAX, EM_FRACTION_MIN, EM_FRACTION_MAX
      REAL HOT_FRACTION_MIN
      PARAMETER( CH_FRACTION_MAX = .4 )
      PARAMETER( EM_FRACTION_MIN = .05 )
      PARAMETER( EM_FRACTION_MAX = .95)
      PARAMETER( HOT_FRACTION_MIN= .1 )
C----------------------------------------------------------------------
C
C Get jet information necessary to use routine
C
      IJ=IJET
      CALL GTJETS_ALL(IJ,IVERS,E,THETA,PHI,ETA,SIG,EMFRAC,ISPL,
     +                NCELL,HOT,IER)
C
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('No jets bank','QCD_BAD_DST',
     &          'No jets bank found','W')
        BAD_FLAG = -1
        GOTO 999
      ELSE IF ( IVERS .LT. 3 ) THEN
        BAD_FLAG = -1
        CALL ERRMSG('Old version', 'QCD_BAD_DST',
     &    'JETS bank uses version 2 or less',
     &    'W')
        GOTO 999
      END IF
C
C: Set to OK
      BAD_FLAG = 0
C
C: Change Physics eta to Detector eta
      DETA=PETA_TO_DETA(ETA,ZVERT)
C
C: Apply CH cut
      IF ( HOT(2) .GT. CH_FRACTION_MAX ) BAD_FLAG =
     &  IBSET(BAD_FLAG,0)

C: Apply hot cell cut
      IF ( HOT(3) .GT. 1./HOT_FRACTION_MIN ) BAD_FLAG =
     &  IBSET(BAD_FLAG,1)

C: Apply EM cut(drop low EMF cut in ICR)      
      IF ((ABS(DETA).GE.1.0).AND.(ABS(DETA).LE.1.6)) THEN
        IF (EMFRAC.GT.EM_FRACTION_MAX) THEN
          BAD_FLAG = IBSET( BAD_FLAG, 3 )
        ENDIF
      ENDIF
      IF ((ABS(DETA).LT.1.0).OR.(ABS(DETA).GT.1.6)) THEN
      IF ( EMFRAC .LT. EM_FRACTION_MIN .OR. EMFRAC
     &  .GT. EM_FRACTION_MAX) THEN
          BAD_FLAG = IBSET( BAD_FLAG, 3 )
      ENDIF
      ENDIF

  999 RETURN
      END
