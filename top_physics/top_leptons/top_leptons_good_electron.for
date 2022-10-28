      LOGICAL FUNCTION TOP_LEPTONS_GOOD_ELECTRON(LPELC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check the Bit Pattern in Word 19 of
C-                         PELC to see if this is a good electron
C-                         track candidate.
C-
C-   Returned value  : 
C-              TOP_LEPTONS_GOOD_ELECTRON = True/False for Good/Poor Candidate
C-   Inputs  : 
C-              LPELC - PELC Bank pointer   
C-   Outputs : 
C-              None
C-   Controls: 
C-              None
C-
C-   Created  24-SEP-1992   Stephen J. Wimpenny
C-   Modified  4-Dec-1992   Changed for new PELC bank structure 
C-   Updated  30-JAN-1993   Meenakshi Narain   
C-                          use ELEMASK and the EM quality
C-                          check routine, which takes care of
C-                          all version dependencies... 
C-   Updated  16-Mar-1993   Routine renamed Top_Leptons_Good_Electron instead
C-                          Good_Electron for library compatibility
C-   Modified  4-Dec-1993   NCells cut disabled for MonteCarlo (shower
C-                          library)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      INTEGER LPELC
      INTEGER IER, I, N
      INTEGER ELEMASK,ELECTRON_MASK(32)
C
      LOGICAL FIRST,OK,DIAGNOSTIC_DUMPS,TOP_LEPTONS_UTIL_MONTECARLO
C
      DATA FIRST/.TRUE./
C
      TOP_LEPTONS_GOOD_ELECTRON=.FALSE.
C
      IF (FIRST) THEN
        CALL EZPICK('TOP_LEPTONS_RCP')
        CALL EZGETA_i('ELECTRON_MASK',0,0,0,N,IER)  
        IF (N.NE.32) 
     1     CALL ERRMSG('Error reading ELE MASK ARRAY',
     2      'TOP_LEPTONS_GOOD_ELECTRON',' ','F')
        IF(IER.EQ.0) THEN
          CALL EZGETA('ELECTRON_MASK',1,N,1,ELECTRON_MASK,IER)
C
C *** Turn off number of cells cut for Monte Carlo (showerlibrary).
C
          IF(TOP_LEPTONS_UTIL_MONTECARLO())THEN
            ELECTRON_MASK(13) = 0
          ENDIF
C
          ELEMASK = 0
          DO I = 1, N
            IF (ELECTRON_MASK(I).EQ.1) THEN
              ELEMASK = IOR(ELEMASK,2**(I-1))
            ENDIF
          ENDDO
        ENDIF
        IF(IER.EQ.0) CALL EZGET('DO_DIAGNOSTIC_DUMPS',
     1    DIAGNOSTIC_DUMPS,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_GOOD_ELECTRON',' ','F')
        FIRST = .FALSE.
      ENDIF
C
      IF(DIAGNOSTIC_DUMPS) THEN
        TOP_LEPTONS_GOOD_ELECTRON=.TRUE.
        GO TO 999
      ENDIF
C
      IF(LPELC.GT.0) THEN
C
C *** Check Cleanem Selection Word from PELC
C
        CALL CHECK_EM_QUALITY(LPELC,ELEMASK,OK)
        TOP_LEPTONS_GOOD_ELECTRON = OK
      ELSE
        GO TO 999
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
