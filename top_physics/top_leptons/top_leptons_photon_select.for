      SUBROUTINE TOP_LEPTONS_PHOTON_SELECT(NOPH,NOPH_UNCUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Loop over PPHO banks and drop banks with
C-                         photon candidates which we do not want to
C-                         consider further. Also Et orders the
C-                         remaining banks
C-
C-   Inputs  : None
C-
C-   RCP input variables :
C-             ETA_MAX   - max eta
C-             PT_MIN    - min Pt
C-             PT_MAX    - max Pt
C-
C-   Outputs : NOPH       - No of PPHO candidates after cuts
C-             NOPH_UNCUT - No of PPHO banks before selection
C-
C-   Controls:
C-
C-   Created  15-JUL-1992   Stephen J. Wimpenny
C-   Modified  3-AUG-1992   Meena's Electron Quality Cuts added
C-   Updated  14-SEP-1992   Meenakshi Narain  CLEAN UP RCP variable
C-                                            handling mechanism
C-   Modified 28-Sep-1992   Fix error in raw Bank counting
C-   Modified  4-Dec-1992   New PPHO Bank structure
C-   Updated  30-JAN-1993   Meenakshi Narain   
C-                          change electron/photon selection to
C-                          D0 electron/photon selection 
C-                          a) update so that electron/photon selction 
C-                          is compatible with different versions of the 
C-                          pelc/ppho banks
C-                          b) change eta,et cutoff masks from bit 16 and 17
C-                             to be bits 14 and 15 (spare in cleanem)
C-                           ***NOTE*** this could lead to a potential
C-                           ***conflict if CLEANEM is updated to use them
C-                          c) use GAMMASK to count good photons
C-   Modified 16-Mar-1993   Change in Good_Photon logical name
C-   Modified  7-Jul-1993   Photon_Mask format in TOP_LEPTONS RCP changed
C-                          EM scale correction added
C-   Modified 12_Jul-1993   Missing Et correction due to scale correction
C-                          added
C-   Modified  1-Oct-1993   etamin cut added
C-   Modified 20-Nov-1993   MET EM correction logic removed
C-   Modified  4-Dec-1993   NCells cut disabled for MonteCarlo (shower
C-                          library)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      LOGICAL FIRST,EM_CORR
      LOGICAL TOP_LEPTONS_UTIL_MONTECARLO
C
      INTEGER NOPH,NOPH_UNCUT
      INTEGER GZPPHO,I,N
      INTEGER LPPHO,IER,IFLAG,JFLAG,IJUNK
      INTEGER GAMMASK, VERSION
      INTEGER PHOTON_MASK(32)
C
      REAL ETA_MIN,ETA_MAX,PT_MIN,PT_MAX
      REAL ET_PHOT,EM_FACT,TOP_LEPTONS_EM_CORRECTION
C
      DATA FIRST,IJUNK/.TRUE.,0/
C
      IF (FIRST) THEN
C
C *** Read Bank 'cuts' from RCP file
C
        IER = 0
C
C *** Get all latest parameter/Options Values
C
        CALL EZPICK('TOP_LEPTONS_RCP')
C
C *** Photons --- PHOT
C
        CALL EZGET('PPHO_ETAMIN',ETA_MIN,IER)       
        IF(IER.EQ.0) CALL EZGET('PPHO_ETAMAX',ETA_MAX,IER)
        IF(IER.EQ.0) CALL EZGET('PPHO_PTMIN',PT_MIN,IER)
        IF(IER.EQ.0) CALL EZGET('PPHO_PTMAX',PT_MAX,IER)
        IF(IER.EQ.0) CALL EZGETA('PHOTON_MASK',0,0,0,N,IER)  
        IF (IER.EQ.0) THEN
          IF (N.NE.32) THEN
            CALL ERRMSG('Error reading Photon Mask',
     &      'TOP_LEPTONS_PHOTON_SELECT',' ','F')
          ENDIF
          CALL EZGETA('PHOTON_MASK',1,N,1,PHOTON_MASK,IER)
          GAMMASK = 0
C
C *** Turn off number of cells cut for Monte Carlo (showerlibrary).
C
          IF(TOP_LEPTONS_UTIL_MONTECARLO())THEN
            PHOTON_MASK(13) = 0
          ENDIF
C
          DO I = 1, N
            IF (PHOTON_MASK(I).EQ.1) THEN
              GAMMASK = IOR(GAMMASK,2**(I-1))
            ENDIF
          ENDDO
        ENDIF
        IF(IER.EQ.0) CALL EZGET('EM_CORR',EM_CORR,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_PHOTON_SELECT',' ','F')
        FIRST = .FALSE.
      ENDIF
C
      NOPH=0
      NOPH_UNCUT=0
C
C *** Look for photon candidates
C
      LPPHO=GZPPHO()
      IF(LPPHO.NE.0) THEN
C
C *** Order Banks in Decreasing Pt
C
        CALL ZSORT(IXCOM,LPPHO,7)
        LPPHO=GZPPHO()
        CALL ZTOPSY(IXCOM,LPPHO)
        LPPHO=GZPPHO()
C
C *** count raw banks (Diagnostics)
C
        DO WHILE(LPPHO.GT.0)
          NOPH_UNCUT=NOPH_UNCUT+1
          LPPHO=LQ(LPPHO)
        ENDDO
C
C *** Sort thro and look for good photons
C *** Start by doing electron cleanup -> require 'golden' electron
C *** candidate based onquality criteria
C ***   MN 30-JAN-1993
C ****  comment out the call below, this now replaced by the
C ****  COMPUTE_EM_QUALITY package to be run before the 
C ****  top_leptons package for version 1 and 2 of pelc/ppho
C *** Now apply kinematic cuts
C
        LPPHO=GZPPHO()
        DO WHILE (LPPHO.GT.0)
          IFLAG=0
C
C ****  check on bank version number
C
          VERSION = IQ(LPPHO+1)
          IF (VERSION.EQ.1) THEN
            IFLAG = IQ(LPPHO+20)
          ELSE IF (VERSION.EQ.2) THEN
            IFLAG = IQ(LPPHO+23)
          ELSE
            IFLAG = IQ(LPPHO+30)
          ENDIF
C
C *** Flag PPHO banks with candidates which fail selection cuts
C *** start with max eta cut
C
          JFLAG=IFLAG
          IF(ABS(Q(LPPHO+9)).GT.ETA_MAX.OR.ABS(Q(LPPHO+9)).LT.ETA_MIN)
     1      THEN
            IFLAG=IBSET(IFLAG,30)
            IF(JFLAG.EQ.0) IJUNK=IJUNK+1
          ENDIF
C
C *** Pt cuts
C
          EM_FACT=1.0
          IF(EM_CORR) THEN
C
C *** Calculate absoulte scale correction
C
            EM_FACT=TOP_LEPTONS_EM_CORRECTION(LPPHO)
          ENDIF
          ET_PHOT=Q(LPPHO+7)*EM_FACT
C
          IF(ET_PHOT.LT.PT_MIN.OR.ET_PHOT.GT.PT_MAX) THEN
            IFLAG=IBSET(IFLAG,15)
            IF(JFLAG.EQ.0) IJUNK=IJUNK+1
          ENDIF
C
C ****  Copy IFLAG back into PPHO Bank
C
          IF (VERSION.EQ.1) THEN
            IQ(LPPHO+20) = IFLAG 
          ELSE IF (VERSION.EQ.2) THEN
            IQ(LPPHO+23) = IFLAG 
          ELSE
            IQ(LPPHO+30) = IFLAG 
          ENDIF
C
          IF (IAND(IFLAG,GAMMASK).NE.0) THEN
            GOTO 10
          ENDIF
C
C *** OK we have a possible good one !
C *** increment Bank counter
C
          NOPH=NOPH+1
   10     LPPHO=LQ(LPPHO)
        ENDDO
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
