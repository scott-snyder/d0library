      SUBROUTINE CAJNEP_ANL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-         histograms for CAJNEP package
C-
C-   Created  28-NOV-1991   Dhiman Chakraborty
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INTEGER LEP,GZJETS,GZJNEP,ADDR_J(50)
      INTEGER NCELL_EP,NCELL_J,NCELL_OVRLAP,NEP
      INTEGER USER_ID,REC_LEN,NUM_VAR,IERR,STATUS
      INTEGER NTUPLE_ID
      INTEGER I,J
      REAL    IER
      REAL    CONE_RAD
      REAL    XTUPLE(31)
      CHARACTER*8 CHTAGS(31)
      CHARACTER*16 LOGNAM
      CHARACTER*32 FILNAM,TITLE
      LOGICAL NEWFILE
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
      DATA USER_ID/777/,REC_LEN/8191/,NUM_VAR/31/
      DATA LOGNAM/'CAJNEP_ANL'/,FILNAM/'CAJNEP_ANL.NT'/,
     &  TITLE/'TEST_JNEP'/
      DATA CHTAGS/'CONE_RAD','EX','EY','EZ','E','ET','THETA','PHI',
     &  'ETA','SIG2EX','SIG2EY','FR_ETEM','DEX','DEY','DEZ','DE',
     &  'DET','DTHET','DPHI','DETA','DSIG2EX','DSIG2EY','JFR_ETEM',
     &  'EP_ETA','EP_PHI','JET_ETA','JET_PHI','NEP','NCELL_EP',
     &  'NCELL_J','DNCELL'/
      COMMON /ANALYZE/CONE_RAD,ADDR_J,J,NCELL_EP,
     &  NCELL_J,NEP,NCELL_OVRLAP
C----------------------------------------------------------------------
C
      CALL DHDIR('CAJNEP_RCP','HBOOK_DIRECTORY',IER,' ')
c         ! Create/Set HBOOK directory for CAJNEP
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('CALORIMETER','CAJNEP',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      IF(FIRST)THEN
        FIRST = .FALSE.
C $$$$$$$$$$$$$$$$$$$$$$$$$$
C $$$$  BOOK NTUPLE HERE
C $$$$$$$$$$$$$$$$$$$$$$$$$$
C
        CALL EZPICK('CAJNEP_RCP')
        CALL EZGET('NEWFILE',NEWFILE,IER)
        CALL NTUPLE_FILE_OPEN(USER_ID,NEWFILE,FILNAM,REC_LEN,LOGNAM,
     &    IERR)
        CALL NTUPLE_BOOK(LOGNAM,NUM_VAR,CHTAGS,TITLE,NTUPLE_ID,IERR)

C
      ENDIF
C
C $$$$$$$$$$$$$$$$$$$$$$$$$$
C $$$$  FILL NTUPLE HERE
C $$$$$$$$$$$$$$$$$$$$$$$$$$
C
      LJETS = LSLINK(ADDR_J(J))
C
      LJNEP = LQ(LJETS-2)
      IF(LJNEP.GT.0) THEN
      XTUPLE(1) = CONE_RAD
      CALL UCOPY(Q(LJNEP+2),XTUPLE(2),10)
      XTUPLE(12) = Q(LJNEP+14)
      CALL VSUB(Q(LJETS+2),Q(LJNEP+2),XTUPLE(13),10)
      XTUPLE(23) = Q(LJETS+14)
      LEP = LQ(LJETS-3)
      XTUPLE(24) = Q(LEP+9)
      XTUPLE(25) = Q(LEP+10)
      XTUPLE(26) = Q(LJETS+9)
      XTUPLE(27) = Q(LJETS+8)
      XTUPLE(28) = FLOAT(NEP)
      XTUPLE(29) = FLOAT(NCELL_EP)
      XTUPLE(30) = FLOAT(NCELL_J)
      XTUPLE(31) = FLOAT(NCELL_OVRLAP)
C
      CALL NTUPLE_FILL(LOGNAM,NTUPLE_ID,XTUPLE,STATUS)
      ENDIF
C
  999 RETURN
      END
