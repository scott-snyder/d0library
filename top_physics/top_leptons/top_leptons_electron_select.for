      SUBROUTINE TOP_LEPTONS_ELECTRON_SELECT(NOEL,NOEL_UNCUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Loop over PELC banks and drop banks with
C-                         electron candidates which we do not want to
C-                         consider further. Also Pt orders the
C-                         remaining banks
C-
C-   Inputs  : None
C-   RCP input variables :
C-             ETA_MAX       - max eta
C-             PT_MIN        - min Pt
C-             PT_MAX        - max Pt
C-
C-   Outputs : NOEL       - No of PELC candidates after cuts
C-             NOEL_UNCUT - No of PELC banks before selection
C-   Controls:
C-
C-   Created  15-JUL-1992   Stephen J. Wimpenny
C-   Modified  3-AUG-1992   Meena's Electron Quality Cuts added
C-   Updated  14-SEP-1992   Meenakshi Narain  CLEAN UP RCP variable
C-                                            handling mechanism
C-   Modified 24-Sep-1992   Flags Banks instead of dropping them
C-   Modified 28-Sep-1992   Fix error in raw Bank counting
C-   Modified  4-Dec-1992   New PELC/PPHO Bank structure
C-   Updated  30-JAN-1993   Meenakshi Narain  change electron selection to 
C-                          D0 electron selection                          
C-                          a) update so that electron selection 
C-                          is compatible with different versions of the 
C-                          pelc/ppho banks
C-                          b) change eta,et cutoff masks from bit 16 and 17
C-                             to be bits 14 and 15 (spare in cleanem)
C-                           ***NOTE*** this could lead to a potential
C-                           ***conflict if CLEANEM is updated to use them
C-                          c) use ELEMASK to count good photons
C-   Modified 15-Mar-1993   Cosmetic changes to Warning messages
C-   Modified  7-Jul-1993   ELectron_Mask format in TOP_LEPTONS RCP changed
C-                          EM scale correction added
C-   Modified  1-Oct-1993   etamin cut added
C-   Modified 20-Nov-1993   ETM MET correction calcultion removed
C-   Modified  4-Dec-1993   Ncell cut disabled for MonteCarlo Data
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
      INTEGER NOEL,NOEL_UNCUT
      INTEGER GZPELC,I,N
      INTEGER LPELC,IER,IFLAG,JFLAG,IJUNK
      INTEGER ELEMASK,VERSION
      INTEGER ELECTRON_MASK(32)
C
      REAL ET_ELEC,EM_FACT,TOP_LEPTONS_EM_CORRECTION
      REAL ETA_MIN,ETA_MAX,PT_MIN,PT_MAX
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
C *** Electrons --- PELC
C
        CALL EZGET('PELC_ETAMIN',ETA_MIN,IER)
        IF(IER.EQ.0) CALL EZGET('PELC_ETAMAX',ETA_MAX,IER)
        IF(IER.EQ.0) CALL EZGET('PELC_PTMIN',PT_MIN,IER)
        IF(IER.EQ.0) CALL EZGET('PELC_PTMAX',PT_MAX,IER)
        IF(IER.EQ.0) CALL EZGETA('ELECTRON_MASK',0,0,0,N,IER)  
        IF (IER.EQ.0) THEN
          IF (N.NE.32) THEN
            CALL ERRMSG('Error reading Electron Mask',
     &    'TOP_LEPTONS_ELECTRON_SELECT',' ','F')
          ENDIF
          CALL EZGETA('ELECTRON_MASK',1,N,1,ELECTRON_MASK,IER)
          ELEMASK = 0
C
C *** Turn off number of cells cut for Monte Carlo (showerlibrary).
C
          IF(TOP_LEPTONS_UTIL_MONTECARLO())THEN
            ELECTRON_MASK(13) = 0
          ENDIF
C
          DO I = 1, N
            IF (ELECTRON_MASK(I).EQ.1) THEN
              ELEMASK = IOR(ELEMASK,2**(I-1))
            ENDIF
          ENDDO
        ENDIF
        CALL EZGET('EM_CORR',EM_CORR,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_ELECTRON_SELECT',' ','F')
        FIRST = .FALSE.
      ENDIF
C
      NOEL=0
      NOEL_UNCUT=0
C
C *** Look for electron candidates
C
      LPELC=GZPELC()
      IF(LPELC.NE.0) THEN
C
C *** Order Banks in Decreasing Pt
C
        CALL ZSORT(IXCOM,LPELC,7)
        LPELC=GZPELC()
        CALL ZTOPSY(IXCOM,LPELC)
        LPELC=GZPELC()
C
C *** count raw banks (Diagnostics)
C
        DO WHILE(LPELC.GT.0)
          NOEL_UNCUT=NOEL_UNCUT+1
          LPELC=LQ(LPELC)
        ENDDO
C
C *** Sort thro and look for good electrons
C *** Start by doing electron cleanup -> require 'golden' electron
C *** candidate based onquality criteria
C ***   MN 30-JAN-1993
C ****  comment out the call below, this now replaced by the
C ****  COMPUTE_EM_QUALITY package to be run before the 
C ****  top_leptons package for version 1 and 2 of pelc/ppho
C *** Now apply kinematic cuts
C
        LPELC=GZPELC()
        DO WHILE (LPELC.GT.0)
C
C ****  check on bank version number
C
          VERSION = IQ(LPELC+1)
          IF (VERSION.LT.3) THEN
            IFLAG = IQ(LPELC+20)
          ELSE
            IFLAG = IQ(LPELC+30)
          ENDIF
C
C *** Flag PELC banks with candidates which fail selection cuts
C *** start with max eta cut
C
          JFLAG=IFLAG
          IF(ABS(Q(LPELC+9)).GT.ETA_MAX.OR.ABS(Q(LPELC+9)).LT.ETA_MIN)
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
C *** Calculated absolute energy scale correction
C
            EM_FACT=TOP_LEPTONS_EM_CORRECTION(LPELC)
          ENDIF
          ET_ELEC=Q(LPELC+7)*EM_FACT
C
          IF(ET_ELEC.LT.PT_MIN.OR.ET_ELEC.GT.PT_MAX) THEN
            IFLAG=IBSET(IFLAG,15)
            IF(JFLAG.EQ.0) IJUNK=IJUNK+1
          ENDIF
C
C ****  Copy IFLAG back into bank
C
          IF (VERSION.LT.3) THEN
            IQ(LPELC+20) = IFLAG
          ELSE
            IQ(LPELC+30) = IFLAG
          ENDIF
          IF (IAND(IFLAG,ELEMASK).NE.0) THEN
            GOTO 10
          ENDIF
C
C *** OK we have a possible good one !
C
          NOEL=NOEL+1
   10     LPELC=LQ(LPELC)
        ENDDO
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
