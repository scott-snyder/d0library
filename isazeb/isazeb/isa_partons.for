      SUBROUTINE ISA_PARTONS(OK,DONE,USE_CONE_LIM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      read partons from a file
C-   Outputs : 
C-    OK = succesful read
C-    DONE= finished reading file
C-
C-  ENTRY ISA_PARTONS_INI(IN_UNIT,W_DECAY)
C-         initialize ISAJET 
C-   Inputs:
C-         IN_UNIT=unit number for input
C-         W_DECAY= select W mode decay
C-                  ALL  (all decays)
C-                  QUARKS (only quarks)
C-                  LEPTONS (leptonic decays only,including taus)
C-                  ELECTRON (electron decays only)
C-                  MUON     (muon decays only)
C-                  E_AND_MU  (electron+muon decays)
C-                  TAUS     (tau decays only)
C-     USE_CONE_LIM= if false RMIN is set to 2.0 and ISAJET won't use it
C-
C- ENTRY ISA_PARTONS_WEIGHT(SIG,WT,DO)
C-      give weight and cross section to ISAEFL
C-  Outputs:
C-      SIG= cross section
C-      WT = weight
C-      DO = WT, SIG not set if DO is false
C-
C-   Created   3-MAY-1991   Serban D. Protopopescu
C-   Updated   1-OCT-1994   sss - Rearrange to set DONE properly on last evt.
C-                                Protect against overrunning local arrays.
C-                                Delete decls of unused locals.
C-                                Update SAVE.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER MAXN
      PARAMETER (MAXN = 10)
C
      LOGICAL OK,DONE,USE_CONE_LIM
      REAL    SIG,WT
      LOGICAL DO,DOIT
      INTEGER IN_UNIT,INDATA
      INTEGER I,J,IDS(MAXN),NPRTNS,NDCAYS,NEVENT,IW,IDQ(2)
      CHARACTER*76 TITLE
      REAL    PRTNS(4,MAXN),WEIGHT,CMSE,ETMIN,RMIN,CROSS_SECTION
      CHARACTER*8 W_DECAY,REAC
      CHARACTER*1 WZ
      CHARACTER*2 BEAMS(2)
      CHARACTER*4 DCAYS(20)
      LOGICAL FIRST,INIOK,IWDK
      DATA FIRST/.TRUE./
      DATA NEVENT/0/
      DATA DOIT/.FALSE./
C
      LOGICAL HAVE_EVENT
      DATA    HAVE_EVENT/.FALSE./
      SAVE    HAVE_EVENT
C
      SAVE FIRST, NEVENT, DOIT
      SAVE INDATA, CROSS_SECTION, IWDK
      SAVE NPRTNS, WEIGHT, PRTNS, IDS
C----------------------------------------------------------------------
C
C            read partons
      OK=.TRUE.
      DONE=.FALSE.
      DOIT=.TRUE.
c
      IF (.NOT. HAVE_EVENT) THEN
        OK = .FALSE.
        DONE = .TRUE.
        RETURN
      ENDIF
C
C         fill ISAJET information
C
      CALL IPARTNS(NPRTNS,IDS,PRTNS,IDQ,WEIGHT,IWDK)
c
      FIRST=.FALSE.
      NEVENT=NEVENT+1

C
C             read events after 1st
c
      HAVE_EVENT = .FALSE.
      READ(INDATA,ERR=100) NPRTNS,WEIGHT
      IF (NPRTNS .GT. MAXN) THEN
        CALL ERRMSG ('too many partons', 'isa_partons', ' ', 'E')
        NPRTNS = MAXN
      ENDIF
      READ(INDATA,ERR=100) ((PRTNS(I,J),I=1,4),IDS(J),J=1,NPRTNS)
      HAVE_EVENT = .TRUE.

      GOTO 999
  100 DONE=.TRUE.    
      OK = .TRUE.
      CALL STNVRN(NEVENT)
      GOTO 999
C
C
      ENTRY ISA_PARTONS_INI(IN_UNIT,W_DECAY,USE_CONE_LIM)
C
      IF(.NOT.FIRST) GOTO 999
      INDATA=IN_UNIT
      have_event = .false.
C
C              read initial information and first event
      READ(INDATA,ERR=200) TITLE
      READ(INDATA,ERR=200) CMSE,ETMIN,RMIN,CROSS_SECTION
      READ(INDATA,ERR=200) REAC
      READ(INDATA,ERR=100) NPRTNS,WEIGHT
      IF (NPRTNS .GT. MAXN) THEN
        CALL ERRMSG ('too many partons', 'isa_partons', ' ', 'E')
        NPRTNS = MAXN
      ENDIF
      READ(INDATA,ERR=100) ((PRTNS(I,J),I=1,4),IDS(J),J=1,NPRTNS)
      HAVE_EVENT = .TRUE.
C
      IF(.NOT.USE_CONE_LIM) THEN
        IF(REAC.EQ.'DRELLYAN') 
     &    call evol_cuts(rmin,etmin,.true.)  ! provide limits to radiation
        RMIN=2.0
      ENDIF
C              default initial partons are gluons
      IDQ(1)=9
      IDQ(2)=9
C
C        find if there is a W(Z) and if its decays are provided
      WZ=' '
      IWDK=.FALSE.
      IW=NPRTNS
      DO 1 I=1,NPRTNS
        IF(IABS(IDS(I)).EQ.80) THEN
          WZ='W'
          IW=I
        ENDIF
        IF(IABS(IDS(I)).EQ.90) THEN
          WZ='Z'
          IW=I
        ENDIF
   1  CONTINUE
      IWDK=IW.EQ.(NPRTNS-2)
C
C         specified decay options
      IF(REAC.EQ.'DRELLYAN') THEN
        IDQ(1)=1
        IDQ(2)=-2
        IF(W_DECAY.EQ.'ELECTRON') THEN
          NDCAYS=2
          DCAYS(1)='E+'
          DCAYS(2)='E-'
        ELSE IF(W_DECAY(1:4).EQ.'MUON') THEN
          NDCAYS=2
          DCAYS(1)='MU+'
          DCAYS(2)='MU-'
        ELSE IF(W_DECAY.EQ.'E_AND_MU') THEN
          NDCAYS=4
          DCAYS(1)='E+'
          DCAYS(2)='E-'
          DCAYS(3)='MU+'
          DCAYS(4)='MU-'
        ELSE IF(W_DECAY(1:4).EQ.'TAUS') THEN
          NDCAYS=2
          DCAYS(1)='TAU+'
          DCAYS(2)='TAU-'
        ELSE
          NDCAYS=1
          DCAYS(1)=W_DECAY(1:4)
        ENDIF
      ENDIF
C
C         fill ISAJET information
      BEAMS(1)='P '
      BEAMS(2)='AP'
      CALL INISAP(CMSE,REAC,BEAMS,WZ,NDCAYS,DCAYS,ETMIN,RMIN,INIOK)
C
      IF(.NOT.INIOK) THEN
        CALL ERRMSG('No valid reaction in parton file',
     &      'ISA_PARTONS_INI', ' ','F')
      ENDIF
      GOTO 999
C
C
      ENTRY ISA_PARTONS_WEIGHT(SIG,WT,DO)
      DO=DOIT
      SIG=CROSS_SECTION
      WT=WEIGHT
      GOTO 999
C
  200 CALL ERRMSG(' Cannot read parton file','ISA_PARTONS_INI',' ','F')
C
  999 RETURN
      END
