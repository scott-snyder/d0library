      SUBROUTINE L2JETS_PARAMETERS(NEWPAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Init routine for L2JETS:
C-                         1) Read in NEWPAR parameter sets
C-                         2) Check these parameter sets and look to see
C-                            how many are algorithm independent.
C-                         3) Set INIT_OK from control common to .TRUE. if
C-                            no error.
C-
C-   Inputs  : NEWPAR : Number of parameters sets to read, this must be
C-             less than NPAR_MAX or we will error.
C-   Outputs :
C-   Controls:
C-
C-   Created  26-JAN-1990   Richard V. Astur
C-   Modified 30-APR-1991   Rich Astur "Remove L2JETS_TRGR bank calls"
C-                         + Add EZRSET
C-            15-MAY-1991  RA: Add jet size and em-fraction parameters
C-            24-JUN-1991  RA: Allow option to process all candidates.
C-             1-MAY-1991  RA: Skip if NEWPAR .le. 0
C-             6-MAY-1991  RA: Change Em fract cut from fraction to percent
C-            29-JUN-1992  RA: Disable any checking of # of parameter sets
C-            19-NOV-1993  RA: Disable checking of 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$PARAMS:L2JETS.PARAMS'
      INCLUDE 'D0$INC:L2JETS_FAKE_HOT.INC' ! COMMON for Fake Hot Towers
      INCLUDE 'D0$INC:L2JETS_PAR.INC'   ! COMMOM for JETS parameters
      INCLUDE 'D0$INC:L2JETS_CONT.INC'  ! COMMON for controlling
      INCLUDE 'D0$INC:L2JETS_CHARS.INC'         ! Character variables
C                                       ! parameters
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPN.LINK'
      INCLUDE 'D0$LINKS:IZSCAL.LINK'
      INCLUDE 'D0$LINKS:IZSL2H.LINK'

C&IF VAXVMS,VAXELN
      BYTE NEWPAR
C&ELSE
C&      INTEGER NEWPAR
C&ENDIF
      REAL CONE_CENTER( NPAR_MAX ), CONE_TOTAL( NPAR_MAX )
      REAL DUM_EM_CONV,DUM_HD_CONV,DUM_EM_OFF,DUM_HD_OFF
      REAL MAX_PERCENT( NPAR_MAX ), MIN_PERCENT( NPAR_MAX )
      INTEGER NEWPAR1,GZSL2H,LSL2H,GZJPAR,NPARAM_VALUE
      LOGICAL OK,L2J_UTIL_INIT
      INTEGER IER,I                       ! Error status variable
C----------------------------------------------------------------------
C
C---Is this begin run intended for this node type?
C
      IF(NEWPAR.LE.0) GO TO 999   !nothing downloaded this run

C
C---Need to setup some lookup arrays that we will be using.
C
      IF (.NOT. L2J_UTIL_INIT() ) THEN
        NOWSMESS = 'No L2JUTIL INIT'
        NOWERRMESS=' Couldnt init L2J_UTIL_INIT commons?'
        NOWERR = ERR_L2J_UTIL
        GOTO 900
      END IF
C
C---We will use this routine mainly to fill common blocks: both for
C---general JETS use and for Filter parameters. We read this information
C---into ZEBSTP either through RCP or directly. The STP structure
C---should have been created by L2JETS_INIT which is called by the L2 system.
C
      INIT_OK = .TRUE.                  ! Assume initialization is okay
      NOWERR  = ERR_NONE

C---Need to find our RCP banks on the STP tree and declare them to SRCP.
      LSL2H = GZSL2H()                  ! Get link to L2 STP header
      IF (LSL2H .LE. 0) THEN
        NOWERRMESS = ' Cant find SL2H bank'
        NOWSMESS = 'No SL2H bank'
        NOWERR = ERR_NO_SL2H
        GOTO 900
      ENDIF
C
C---We use SL2H to hold both SRCP banks and other banks we are interested in
C---downloading.  The Level2 framework will declare (EZNAME) all SRCP banks
C---that it finds under the SL2H bank.  Currently, L2JETS only uses SRCP banks
C---in downloading, so there is nothing else to check here.
C
  500 CONTINUE
C
C---Init CL2 so we can use the CL2 loopup arrays and the CL2 fast unpacking
C---routines.
C
      CALL CL2_INI
      CALL L2_VERT_PARAMETERS

C---10/4/90 We are going to a format where NEWPAR is NOT passed. Then we
C---would have to get NEWPAR from RCP itself. We do this when we read
C---in the parameters: see below.
C
C---Select RCP bank and read them in. Note I must use the tool name
C---JETS as my bank name because this is the only well defined name that
C---the program that makes the grand .RCP file will know for this tool.
C---Otherwise, I would give it the same name as the common block it fills.
C
C---Note that this routine may be called many times (begin of run) without
C---any new parameter sets for me.  We simply flag the error now, but do not
C---report it unless L2JETS should get called.
C
      CALL EZPICK('L2JETS')
      CALL EZERR(IER)
      IF ( IER .NE. 0 ) THEN  ! This might be okay, but flag it for now.
        NOWERRMESS =
     &    ' Cannot Select RCP bank:L2JETS'
        NOWSMESS   = 'No L2JETS RCP'
        NOWERR = ERR_NO_RCP
        GOTO 900
      ENDIF
C
C---Fill value of NEWPAR1 by  checking to see how long this array is.
C
      CALL EZGETA('ETMIN',0,0,0,NEWPAR1,IER)
      IF (IER .NE. 0) THEN
        NOWERRMESS = ' Cant get size of parameter array from EZGETA '
        NOWSMESS   = 'L2JETS Read fail'
        NOWERR     = ERR_READ_RCP
        GOTO 900
      END IF
C
C---Check to make sure we are dimensioned to handle this many parameters
C---sets. If not, we can NOT proceed.
C
      IF (NEWPAR1 .GT. NPAR_MAX ) THEN
        NOWERRMESS = ' # of parameter sets is too large '
        NOWSMESS   = ' Param overflow'
        NOWERR     = ERR_PARAM_OVERFLOW
        GOTO 900
      END IF
C
C---Try and read all the parameters we need
C
      NOWERRMESS =
     &    ' Cannot read in all parameters from L2JETS RCP bank'
      NOWSMESS   = 'L2JETS Read fail'
      NOWERR     = ERR_READ_RCP
C---Fill in number of parameter sets
      NUM_PARAMS = NEWPAR1
C---Now get the arrays out normally:
      NPARAM_VALUE = 0
      CALL EZGET('ETMIN',ETMIN,IER)
      IF (IER .NE. 0) GOTO 900
      NPARAM_VALUE = NPARAM_VALUE + 1   ! count # of parameter values
      CALL EZGET('CORE_DELTA_R',CONE_CENTER,IER)
      IF (IER .NE. 0) GOTO 900
      NPARAM_VALUE = NPARAM_VALUE + 1   ! count # of parameter values
C      CALL EZGET('JET_DELTA_R',CONE_TOTAL,IER)
C      IF (IER .NE. 0) GOTO 900
C      NPARAM_VALUE = NPARAM_VALUE + 1   ! count # of parameter values
      CALL EZGET('SIZE_MAX',MAXRAD,IER)
      IF (IER .NE. 0) GOTO 900
      NPARAM_VALUE = NPARAM_VALUE + 1   ! count # of parameter values
      CALL EZGET('SIZE_MIN',MINRAD,IER)
      IF (IER .NE. 0) GOTO 900
      NPARAM_VALUE = NPARAM_VALUE + 1   ! count # of parameter values
      CALL EZGET('EMPCT_MAX',MAX_PERCENT,IER)
      IF (IER .NE. 0) GOTO 900
      NPARAM_VALUE = NPARAM_VALUE + 1   ! count # of parameter values
      CALL EZGET('EMPCT_MIN',MIN_PERCENT,IER)
      IF (IER .NE. 0) GOTO 900
      NPARAM_VALUE = NPARAM_VALUE + 1   ! count # of parameter values
      CALL EZGET('NUM_JETS',NJET_COUNT,IER)
      IF (IER .NE. 0) GOTO 900
      NPARAM_VALUE = NPARAM_VALUE + 1   ! count # of parameter values
      CALL EZGET('VETO',VETO_THIS,IER)
      IF (IER .NE. 0) THEN
        CALL ERRMSG('No VETO?','L2JETS_PARAMETERS',
     &    'VETO parameter set not found?','W')
      ENDIF
      NPARAM_VALUE = NPARAM_VALUE + 1   ! count # of parameter values
      CALL EZRSET
C
C---Use the read in parameters to set the parameters used by L2JETS
C
      DO I = 1, NUM_PARAMS
        EMFRACT_MIN( I ) = MIN_PERCENT( I )/100.
        EMFRACT_MAX( I ) = MAX_PERCENT( I )/100.
        ICON_ENG( I ) = MAX(ABS(CONE_CENTER( I ) - .1),0.)/.199
c                                        ! Convert to trigger tower radius
        ICON_CEN( I ) = MAX(ABS(CONE_CENTER( I ) - .1),0.)/.199
c                                        ! Convert to trigger tower radius
      ENDDO
C
C---Check number of parameters used.
C
      IF (NPARAM_VALUE .NE. NPAR_VAL_MAX) THEN
        NOWERRMESS = '# of parameter values not consistent'
        NOWSMESS   = 'Bad param count'
        NOWERR     = ERR_BAD_PAR_COUNT
        GOTO 900
      END IF
C
C---Read in some more from the control bank
C
      NOWERRMESS =
     &    ' Cannot read in all parameters from L2JETS_CONTROL RCP bank'
      NOWSMESS   = 'RCP fail'
      NOWERR     = ERR_READ_RCP

      CALL EZPICK('L2JETS_CONTROL')
      CALL EZERR(IER)
      IF (IER .NE. 0) GOTO 900
      CALL EZGET('DUMP_JAUX',DUMP_JAUX,IER)
      IF (IER .NE. 0) GOTO 900
      CALL EZGET('DO_ALL_CAND',DO_ALL_CAND,IER)
      IF (IER .NE. 0) GOTO 900
      CALL EZGET('RUN_ELN',RUN_ELN,IER)
      IF (IER .NE. 0) GOTO 900
      CALL EZGET('USE_CL2',USE_CL2,IER)
      IF (IER .NE. 0) GOTO 900
      CALL EZGET('TRGR_READY',TRGR_READY,IER)
      IF (IER .NE. 0) GOTO 900
      CALL EZGET('NJTHOT_FAKE',NJTHOT_FAKE,IER)
      IF (IER .NE. 0) GOTO 900
      CALL EZGET('JT_COMP_FAKE',JT_COMP_FAKE,IER)
      IF (IER .NE. 0) GOTO 900
      CALL EZGET('IHOT_MSK_JT_FAKE',IHOT_MSK_JT_FAKE,IER)
      IF (IER .NE. 0) GOTO 900
      CALL EZGET('IHOT_ADR_JT_FAKE',IHOT_ADR_JT_FAKE,IER)
      IF (IER .NE. 0) GOTO 900
C
C--- Get calibration numbers:
C
      CALL EZGET('L2JCAL_CEN',L2JCAL_CEN,IER)
      IF (IER .NE. 0) GOTO 900
      CALL EZGET('L2JCAL_CRA',L2JCAL_CRA,IER)
      IF (IER .NE. 0) GOTO 900
      CALL EZGET('L2JCAL_END',L2JCAL_END,IER)
      IF (IER .NE. 0) GOTO 900
      CALL EZGET('L2JCAL_CEN_CL2',L2JCAL_CEN_CL2,IER)
      IF (IER .NE. 0) GOTO 900
      CALL EZGET('L2JCAL_CRA_CL2',L2JCAL_CRA_CL2,IER)
      IF (IER .NE. 0) GOTO 900
      CALL EZGET('L2JCAL_END_CL2',L2JCAL_END_CL2,IER)
      IF (IER .NE. 0) GOTO 900
C
C--- Overide default TRGR unpacking numbers ONLY if they are here!
C--- Else, make the values negative and the TRGR unpacking routine will
C--- use the current defaults instead.
C
      CALL EZGET('CTS_TO_GEV_EM',DUM_EM_CONV,IER)
      IF (IER .NE. 0) DUM_EM_CONV = -1.
      CALL EZGET('CTS_TO_GEV_HD',DUM_HD_CONV,IER)
      IF (IER .NE. 0) DUM_HD_CONV = -1.
      CALL EZGET('CTS_EM_OFF',DUM_EM_OFF,IER)
      IF (IER .NE. 0) DUM_EM_OFF = -1.
      CALL EZGET('CTS_HD_OFF',DUM_HD_OFF,IER)
      IF (IER .NE. 0) DUM_HD_OFF = -1.
      CALL CL1PAR_SET(DUM_EM_CONV,DUM_HD_CONV,DUM_EM_OFF,DUM_HD_OFF)


C--- Done with EZGETS
      CALL EZRSET

      NOWERRMESS = ' No message. RCP reads are complete'
      NOWSMESS   = ' No error '
      NOWERR     = ERR_NONE

C
C--- At this point we have read in our parameter sets. But we are further
C--- interested in knowing how many of these parameter sets are ALGORITHM
C--- independent. Call a routine to test that. Let it change value of
C--- INIT_OK if it has trouble.
C
      CALL L2JETS_TEST_PARAMS
      IF ( NOWERR .NE. ERR_NONE ) GOTO 900
C
C---Enter parameters used in JPAR bank
C
      NOW_NEW_PARAMS = .TRUE.         ! Tell L2JETS we have new parameter set
      CALL L2JETS_JPARFL
      IF ( NOWERR .NE. ERR_NONE ) GOTO 900


      GOTO 999                        ! OK if we got here
C
  900 CONTINUE                        ! Error
      INIT_OK = .FALSE.
  999 RETURN
      END
