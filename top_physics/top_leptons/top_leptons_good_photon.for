      LOGICAL FUNCTION TOP_LEPTONS_GOOD_PHOTON(LPPHO)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check the Bit Pattern in Word 19 of
C-                         PPHO to see if this is a good electron
C-                         track candidate.
C-
C-   Returned value  : 
C-              GOOD_PHOTON = True/False for Good/Poor Candidate
C-   Inputs  : 
C-              LPPHO - PPHO Bank pointer   
C-   Outputs : 
C-              None
C-   Controls: 
C-              None
C-
C-   Created  24-SEP-1992   Stephen J. Wimpenny
C-   Modified  4-Dec-1992   New PPHO Bank Structure
C-   Updated  30-JAN-1993   Meenakshi Narain   
C-                          use GAMMASK and the EM quality
C-                          check routine, which takes care of
C-                          all version dependencies...
C-   Modified 15-Mar-1993   Name change for library compatibility 
C-   Modified  4-Dec-1993   NCells cut disabled for MonteCarlo (shower
C-                          library)
C-
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      INTEGER LPPHO
      INTEGER GAMMASK,IER,I,N
      INTEGER PHOTON_MASK(32)
C
      LOGICAL FIRST,OK,DIAGNOSTIC_DUMPS,TOP_LEPTONS_UTIL_MONTECARLO
      DATA FIRST/.TRUE./
C
      IF (FIRST) THEN
        CALL EZPICK('TOP_LEPTONS_RCP')
        CALL EZGETA('PHOTON_MASK',0,0,0,N,IER)  
        IF (N.NE.32) 
     1    CALL ERRMSG('Error reading Photon Mask Array',
     &      'TOP_LEPTONS_GOOD_PHOTON',' ','F')
        IF(IER.EQ.0) THEN
          CALL EZGETA('PHOTON_MASK',1,N,1,PHOTON_MASK,IER)
C
C *** Turn off number of cells cut for Monte Carlo (showerlibrary).
C
          IF(TOP_LEPTONS_UTIL_MONTECARLO())THEN
            PHOTON_MASK(13) = 0
          ENDIF
C
          GAMMASK = 0
          DO I = 1, N
            IF (PHOTON_MASK(I).EQ.1) THEN
              GAMMASK = IOR(GAMMASK,2**(I-1))
            ENDIF
          ENDDO
        ENDIF
        IF(IER.EQ.0) CALL EZGET('DO_DIAGNOSTIC_DUMPS',
     1    DIAGNOSTIC_DUMPS,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_GOOD_PHOTON',' ','F')
        FIRST = .FALSE.
      ENDIF
C
      TOP_LEPTONS_GOOD_PHOTON=.FALSE.
C
      IF(DIAGNOSTIC_DUMPS) THEN
        TOP_LEPTONS_GOOD_PHOTON=.TRUE.
        GO TO 999
      ENDIF
C
      IF(LPPHO.GT.0) THEN
C
C *** Check id Word from PPHO
C
        CALL CHECK_EM_QUALITY(LPPHO,GAMMASK,OK)
        TOP_LEPTONS_GOOD_PHOTON = OK
      ELSE
        GO TO 999
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
