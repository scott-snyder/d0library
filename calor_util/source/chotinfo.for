      LOGICAL FUNCTION CHOTINFO_EVENT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  29-MAR-1993   sFahey
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
C----------------------------------------------------------------------
      LOGICAL CHOTINFO_EOP
C
      INTEGER IETA,IPHI,ILYR,NREP,NHITS,I,IUSER,IER,U
      INTEGER LCAEP,GZCAEP,PTR,ADDR,RUNNUM,N,IP,IDX
      LOGICAL OK
      REAL ENERGY
      CHARACTER*70 DIRECTORY,FILENAME,FIL1,FIL2,FIL3,FIL4
      BYTE B(4)
      EQUIVALENCE (B,ADDR)
      CHARACTER*70 EVENT_DATA,FILE
C
      INTEGER NUM_EVTS
      INTEGER NUM_HITS(-NETAL:NETAL,1:NPHIL,1:NLYRL)
      REAL NRG_SUM(-NETAL:NETAL,1:NPHIL,1:NLYRL)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        NUM_EVTS = 0
        CALL VZERO(NUM_HITS,NLYRL*NPHIL*(2*NETAL+1))
        CALL VZERO(NRG_SUM,NLYRL*NPHIL*(2*NETAL+1))
        RUNNUM = IQ(LHEAD+6)
      ENDIF
      CHOTINFO_EVENT = .TRUE.
      LCAEP = GZCAEP()
      IF ( LCAEP .LE. 0 ) RETURN        ! no CAEP -- can't work
      NUM_EVTS = NUM_EVTS + 1
      NREP = IQ(LCAEP+2)
      NHITS = IQ(LCAEP+3)
C
      DO I = 1, NHITS
        PTR = LCAEP+(I-1)*NREP

        ADDR = IQ(PTR+4)
        IETA = B(BYTE4)
        IPHI = B(BYTE3)
        ILYR = B(BYTE2)
        ENERGY = Q(PTR+5)
C
        NUM_HITS(IETA,IPHI,ILYR) = NUM_HITS(IETA,IPHI,ILYR) + 1
        NRG_SUM(IETA,IPHI,ILYR)  = NRG_SUM(IETA,IPHI,ILYR)  + ENERGY
C
      ENDDO                             ! i = 1, nhits
C
  999 RETURN
C
C#####################################################################
      ENTRY CHOTINFO_EOP
C
C   WRITE OUT CHANNEL INFO AT END OF PROCESSING
C
      CALL INRCP('CAHITS_RCP',IER)       ! read in RCP file
      IF(IER.NE.0) GOTO 888             ! failed
      CALL EZPICK('CAHITS_RCP')
      CALL EZERR(IER)
      IF (IER.EQ.0) THEN
        CALL EZGET('CALHOT_USER',IUSER,IER)
        CALL EZGETS('TMPCHOT',1,DIRECTORY,N,IER)
      ELSE
        CALL ERRMSG('CHOTINFO','CHOTINFO_EOP',
     &      'CAHITS_RCP file does not have a CHOT bank.','W')
      ENDIF
      CALL EZRSET

C
C Make filename here
C                    Format: Express_runno_partno.chot
      EVENT_DATA='EVENT_DATA'
      CALL TRNLNM(EVENT_DATA,FILE,N)
      IDX=INDEX(FILE,'.')
      READ(FILE(IDX-2:IDX-1),100)IP
  100 FORMAT(I2)
      CALL ADDSTR(DIRECTORY,'EXPRESS_',FIL1,N)
      CALL STRINT(FIL1(1:N),RUNNUM,FIL2,N)
      CALL ADDSTR(FIL2,'_',FIL3,N)
      CALL STRINT(FIL3(1:N),IP,FIL4,N)
      CALL ADDSTR(FIL4,'.CHOT',FILENAME,N)
C
      CALL GTUNIT(IUSER,U,IER)
      CALL D0OPEN(U,FILENAME,'UO',OK)
      IF (.NOT.OK) THEN
        CALL ERRMSG('CALHOT','CHOTINFO_EOP',
     &      'Could not open output file','W')
          GOTO 999
      ENDIF
C
      WRITE(U) RUNNUM, IP, NUM_EVTS
      WRITE(U) (((NUM_HITS(IETA,IPHI,ILYR),NRG_SUM(IETA,IPHI,ILYR),
     &            ILYR=1,NLYRL),IPHI=1,NPHIL),IETA=-NETAL,NETAL)
C
      CALL D0CLOSE(U,' ',OK)
      CALL RLUNIT(IUSER,U,IER)
  888 RETURN
      END
