      LOGICAL FUNCTION MULIB()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : MAIN PACKAGE HOOK for MUON HIT LIBRARY BUILDING
C-
C-   Inputs  :  NONE
C-   Outputs :  NONE
C-   Created    5-AUG-1993   Jasbir Singh, Chip Stewart   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER, IETA,IPHI,ILYR
      INTEGER K,NCHT,NEMT,LDCATE,NR,NFOUND
      REAL    E(4),ET,WT,ISA_WEIGHT,X
      LOGICAL FIRST,LMONTE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PAWC.INC'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INCLUDE 'D0$LINKS:IZISV1.LINK'
      INCLUDE 'D0$INC:ZLINKC.INC'
C
      LOGICAL START,SINGLE_TRACKS
      INTEGER LISV1,GZISV1,LSUPP,LISP1,LISAE,GZISAE,ID,NTUPLE_SIZE
      INTEGER LSUPV,IDABS
      REAL IDPAR,PX,PY,PZ,PT,ETA,PHI,TH,VTX(3),PP(4),THETA
C
      REAL XTUPLE(7)
      PARAMETER(NTUPLE_SIZE=7)
      CHARACTER*50 MSG,LABELS(NTUPLE_SIZE)
      DATA FIRST/.TRUE./
      DATA LABELS /'IDPAR','PX','PY','PZ','PT','ETA','PHI'/
C----------------------------------------------------------------------
C
      MULIB = .TRUE.
      IF(FIRST)THEN
        FIRST = .FALSE.
        call hbook1(1,'idpar',100,0.,100.,0.)
        call hbook1(2,'px',100,0.,200.,0.)
        call hbook1(3,'py',100,0.,200.,0.)
        call hbook1(4,'pz',100,0.,200.,0.)
        call hbook1(5,'pt',100,0.,200.,0.)
        call hbook1(6,'eta',100,0.,200.,0.)
        call hbook1(7,'phi',100,0.,200.,0.)
        CALL HBOOKN(900,'SINGLE_TRACK NTUPLE',
     &    NTUPLE_SIZE,' ',1024,LABELS)
      ENDIF
C
	call make_mulib
C
C=======   BRANCH TO ISAJET TRACKS==========
C
      LISAE= GZISAE  ()     ! find ISAJET main bank
      LSUPV = LISAE-IZISV1   ! find pointer to first bank
C      NP = 0
   10 CALL GTISV1(LSUPV,LISV1,ID,PP,VTX(1),VTX(2),VTX(3))
      IF( LISV1 .GT. 0 ) THEN
        LSUPP = LISV1 - IZISP1
   11   CALL GTISP1(LSUPP,LISP1,ID,PP,PHI,TH,ETA)
        IF( LISP1 .GT. 0 ) THEN
          IDABS=IABS(IQ(LISP1+1))
C NEUTRINOS
          IF(IDABS.EQ.11.OR.IDABS.EQ.13.OR.IDABS.EQ.15) GO TO 100
C MUONS AND ELECTRON
          IF(IDABS.EQ.14) GO TO 100
C HIGH ETA
          IF(IDABS.NE.120) GO TO 100
          PX = PP(1)
          PY = PP(2)
          PZ = PP(3)
          XTUPLE(1) = ID
          XTUPLE(2) = PP(1)
          XTUPLE(3) = PP(2)
          XTUPLE(4) = PP(3)
          PT        = SQRT(PP(1)**2+PP(2)**2)
          XTUPLE(5) = PT
          XTUPLE(6) = ETA
          XTUPLE(7) = PHI
          CALL HF1(1,float(id),1.)
          CALL HF1(2,PX,1.)
          CALL HF1(3,PY,1.)
          CALL HF1(4,PZ,1.)
          CALL HF1(5,PT,1.)
          CALL HF1(6,ETA,1.)
          CALL HF1(7,PHI,1.)
          CALL HFN(900,XTUPLE)
 100  CONTINUE
          LSUPP = LISP1
          GOTO 11
        END IF
        LSUPV = LISV1
        GOTO 10
      END IF
  999 RETURN
      END
