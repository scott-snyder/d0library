      SUBROUTINE DBANK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : General purpose Zebra Browser.
C-
C-
C-   Created   28-JUL-1988   Michael Peters
C-   Updated  10-APR-1989   Rajendran Raja
C-   Updated  15-Aug-1991   Herbert Greenlee
C-       Added machine-dependent includes
C-       Added missing arguments in smg calls
C-   Updated  19-AUG-1992   sss - eliminate jump into block (for ibm)
C-   Updated   3-JUN-1993   James T. Linnemann  new linear chain operations
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C

C&IF VAXVMS
      INCLUDE '($SSDEF)'
      INCLUDE '($SMGDEF)'
C&ELSE
C&      INCLUDE 'D0$INC:SSDEF.DEF'
C&      INCLUDE 'D0$INC:SMGDEF.DEF'
C&ENDIF

C-
C-    The constant SMG$_PASALREXI cannot be found at each
C-    D0 subscribing nodes because of the versions of VMS and
C-    FORTRAN (and sys$library:forsysdef.tlb) installed there.
C-    For the time being, we declare a dummy constant in order
C-    to compile everywhere.
C-
C-    THIS FIX IS TEMPORARY AND SHOULD BE REMOVED WHENEVER ALL
C-    SUBSCRIBING NODES WILL BE RUNNING AT LEAST VMS/FORTRAN V5.
C-
C-                4/21/89      Kamal Hammoutene. BNLD0::KAMAL
C-
      INTEGER DUMMY_SMG$_PASALREXI
      PARAMETER (DUMMY_SMG$_PASALREXI = '00128031'X)
C-
C-
C-
      INTEGER SMG$CREATE_PASTEBOARD,SMG$DELETE_PASTEBOARD
      INTEGER SMG$REPAINT_SCREEN
      INTEGER PBID
      INTEGER ISTATUS
      LOGICAL STATUS
      INTEGER NROWS, NCOLS              ! Screen size
      INTEGER DUMMY                     ! Dummy argument
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C--------------The bank below has had its variables renamed so
C--------------as not to conflict with ZEBCOM.
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(9000)
      EQUIVALENCE (QG(1),IQG(1),LQG(9)),(LQG(1),LMAIN)
C
      INTEGER   NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,IQG(1),LQG(8000)
      REAL      GVERSN,ZVERSN,FENDQ,WS,QG(1)
C
      INCLUDE 'D0$INC:QUEST.INC'
C-------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBQ.INC'         ! ZEBRA COMMON BLOCKS
      INCLUDE 'D0$INC:MZCA.INC'
      INCLUDE 'D0$INC:MZCB.INC'
      INTEGER IOK,GTCHFR,DRPFRM
      INTEGER IER
      LOGICAL CHOOSE_LIN,LINEAR
C
      CHARACTER*10 BANK
      INTEGER MWV
      PARAMETER (MWV=200)               ! MAXIMUM NUMBER OF LINES IN WRITEUP
      CHARACTER*80 WRUPV(MWV)
C
      INTEGER LUN,LWV,J,LEN3,HBANK,LNEXT,NLIN,ILIN,NZBANK
      INTEGER NMOVE,NMOVED,IMOVE,ID
      INTEGER LZFIDH,MSBYT,IDIV,IANS,ZCOM,IERR,JBYT,LDUM,ZDIV

      CHARACTER*6 CLIN
C
      INCLUDE 'D0$INC:AUTOF.INC'
C
      LOGICAL DOBANK,YES
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C-------------------------------------------------------------------
C

      DOBANK=.TRUE.
      GO TO 10

      ENTRY DWRUP
      DOBANK=.FALSE.
   10 CONTINUE
      ISTATUS=SMG$CREATE_PASTEBOARD(PBID, 'sys$output', NROWS, NCOLS,
     &  0)
      CALL VBROWSE_SCREEN(NROWS,NCOLS,NROWS)
      IF(ISTATUS.NE.SS$_NORMAL.AND.ISTATUS.NE.DUMMY_SMG$_PASALREXI)
     &  GO TO 999
C&IF VAXVMS
C&ELSE
C&C-
C&C-  If the pasteboard did not already exist and this is the first call,
C&C-  we must initialize COMPACK.  This will force COMPACK routines (such as
C&C-  GETPAR) to use SMG I/O.  COMPACK won't mind that we have already
C&C-  created the pasteboard.
C&C-
C&      IF(FIRST)THEN
C&        FIRST = .FALSE.
C&        IF(ISTATUS.EQ.SS$_NORMAL)CALL SETCOM
C&      ENDIF
C&ENDIF
C
      CALL INPTRM     ! reset COMPACK to read commands from terminal
      CALL GETPAR(1,'TYPE IN BANK NAME>','C',BANK)     ! BROWSE TO THIS
      CALL UPCASE(BANK,BANK)
      LBANK=0
      LFRMT = 0 !no format bank booked
      IF(BANK.EQ.'HELP') GO TO 130 !display dbank.doc
C
      IF(DOBANK) THEN
        CHOOSE_LIN = .TRUE.             ! CHOOSE AMONG LINEAR STRUCTURES
C
  120   CALL GET_STORE_DIV(ISTOR,IDIV,IER)
        IF(IER.NE.0)GO TO 120
C
        IF (JBYT(ISTOR,27,6).NE.JQSTOR)  CALL MZSDIV (ISTOR,-7)
     &    ! ZEBRA INTERNAL ROUTINE TO SWITCH STORES
C
C        CALL MZLINT(ISTOR,'/AUTOF/',LBANK,LBANKL,LBANK)   !DECLARE LINK AREA
C
        CALL UCTOH(BANK,HBANK,4,4)
C
        LBANK=LZFIDH(IDIV,HBANK,0)
C
        IF(LBANK.EQ.0) THEN
          CALL GETPAR(1,BANK//' BANK not found. Show .ZEB anyway? [N]',
     &        'L',YES)
          IF(YES) GO TO 130  
          GO TO 999 !exit if .ZEB not wanted
        ENDIF
C
C
C See if bank a linear structure.
C
        LINEAR = .FALSE.
        IF (LBANK.NE.0) THEN
          LINEAR = (LQQ(LBANK+KQS).NE.0)   !try moving on next link
          IF (.NOT.LINEAR) THEN
C
C...try moving against next link to head of chain
            CALL MOVE_ON_LINEAR_CHAIN(LBANK,-10000000,LBANK,NMOVED,IER)
            LINEAR = (NMOVED.NE.0)          !was move successful?
          ENDIF
        ENDIF
        IF( LINEAR.AND.CHOOSE_LIN)THEN
C
C BANK IS A LINEAR STRUCTURE
C
          NLIN = NZBANK(ISTOR,LBANK)  ! Number of banks in linear structure
          WRITE(CLIN,'(I6)')NLIN
          CALL OUTMSG
     &      (' '//BANK//' is a linear structure with '//CLIN//
     &      ' members.')
          CALL GETPAR(1,
     &      'Pick one: (1 is head of chain, probably last booked)',
     &      'I',ILIN)
          CALL MOVE_ON_LINEAR_CHAIN(LBANK,-10000000,LBANK,NMOVED,IER) !to head
          ILIN = ILIN - 1   !now MOVE 1 less than requested
          CALL MOVE_ON_LINEAR_CHAIN(LBANK,ILIN,LBANK,NMOVED,IER)
        ENDIF
      ENDIF
C
  130 CONTINUE
C
      CALL GETZEB(BANK,MWV,WRUPV,LWV,IERR)   ! get .ZEB file
C      IF(IERR.NE.0)GO TO 999
C
  131 CONTINUE  !come here if using same ZEB file as last time
      STATUS = SMG$REPAINT_SCREEN(PBID)
      IF(.NOT.STATUS)CALL LIB$SIGNAL(%VAL(STATUS))
C
C The philosophy here is to be able to display Zero length banks which
C have link areas but not to display any bank if BNKLEN is negative.
C I.e DWRUP called or HELP or Bank not found
      BNKLEN = -999       !Flag for VBROWSE that not looking at a bank
      IF (LBANK.NE.0) THEN
        BNKLEN = IQQ(KQS+LBANK-1)
        IOK = GTCHFR() ! Get bank format for this chain member(used by Auto_fmt)
      ENDIF
      CALL VBROWSE(PBID,WRUPV,LWV,'(I4,A76)',
     &  IQQ(KQS+LBANK+1),BNKLEN,'AUTO',LNEXT,NMOVE,ID)
      IOK = DRPFRM() ! always reget:linear structures may have differing lengths
C
C...see if wish to simply move along linear chain: same format, .ZEB file
      IF (NMOVE.NE.0) THEN
        CALL MOVE_ON_LINEAR_CHAIN(LBANK,NMOVE,LBANK,NMOVED,IER)
        IF (NMOVED.NE.NMOVE) CALL OUTMSG(
     &    ' Could not do full move on linear chain')
        GO TO 131
      ENDIF
C
C...see if wish to search linear chain for ID
      IF (ID.NE.0) THEN
        DO IMOVE = 1,-1,-2  !try in both directions
          LNEXT = LBANK
          NMOVED = IMOVE
          DO WHILE (NMOVED.EQ.IMOVE)  !as long as can move in that direction
            CALL MOVE_ON_LINEAR_CHAIN(LNEXT,IMOVE,LNEXT,NMOVED,IER)
            IF (IQQ(LNEXT+KQS-5).EQ.ID) THEN
              LBANK = LNEXT
              GO TO 131
            ENDIF
          ENDDO
        ENDDO
        CALL OUTMSG('No bank with requested ID found')
        GO TO 131 !go back even if move fails
      ENDIF
C
      IF(LNEXT.GT.0)THEN
        LBANK = LNEXT                  ! address of bank to be found
        HBANK = IQQ(KQS+LBANK-4)
        CALL UHTOC(HBANK,4,BANK,4)
        GO TO 130
      ENDIF
      IF(NEWBNK.EQ.'HELP') THEN
        CALL GETZEB('HELP',MWV,WRUPV,LWV,IERR)   ! get DBANK.DOC file
        STATUS = SMG$REPAINT_SCREEN(PBID)
        IF(.NOT.STATUS)CALL LIB$SIGNAL(%VAL(STATUS))
        BNKLEN = -999 !FLAG to say not looking at a bank
        CALL VBROWSE(PBID,WRUPV,LWV,'(I4,A76)',
     &    IQQ(KQS+LBANK+1),BNKLEN,'AUTO',LNEXT,NMOVE,ID)
        GO TO 130 !Go back to original bank
      ENDIF
      IF(NEWBNK.NE.' ')THEN
C browse to another bank
        GO TO 10
      ENDIF
C
  999 CONTINUE
C- Istatus is still defined from calling smg$create_pasteboard.
C      IF(ISTATUS.NE.DUMMY_SMG$_PASALREXI) THEN
C        STATUS=SMG$DELETE_PASTEBOARD(PBID, 1)
C      ENDIF
      CALL INPCMD      ! reset COMPACK to read commands from command file
C&IF VAXVMS
C&ELSE
C&C-
C&C-  Drop out of visual mode.  In curses, this is necessary for non-visual
C&C-  programs and harmless for visual programs (such as COMPACK).
C&C-
C&      CALL SMG_RESET_TERM
C&ENDIF

      RETURN
      ENTRY DADDR
      CALL GETPAR(1,'Address of Bank> ','I',LBANK)
C MAKE SURE THAT THE STORE HAS BEEN SET CORRECTLY
      HBANK = IQQ(KQS+LBANK-4)
      CALL UHTOC(HBANK,4,BANK,4)
      CHOOSE_LIN = .FALSE.              ! IF LINEAR STRUCTURE DO NOT
                                        ! PERMIT CHOICE
      DOBANK = .TRUE.
      GO TO 130
      END
