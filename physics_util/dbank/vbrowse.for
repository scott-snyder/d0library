      SUBROUTINE VBROWSE(PBID,VA,NA,CHFA,VB,NB,CHFB,LNEXT,NMOVE,ID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Opens one or two windows and allows displays
C-                         one or two vectors of values, allowing the
C-                         user to browse up or down in either window
C-                         using arrow keys. A CR terminates the session.
C-
C-   Inputs  : PBID = pasteboard ID
C-             VA/B = Data to be displayed on window 1/2
C-             NA/B = Lengths of the vectors. NA= 0 or NB=-999
C-                    means no display on that window.
C-             CHFA/B = Character strings with formats for the variables
C-                        or Hollerith strings with a preceeding Int
C-                        or 'AUTO' for automatic formatting
C-   Outputs : LNEXT    = address of next bank to be browsed 
C-             NMOVE    = number of slots to move along linear chain 
C-             ID       = Bank Numerical ID to search for on linear chain
C-         The outputs are 0 if no such request has been made
C-
C-   Created  20-JUL-1988   Michael Peters
C-   Updated  11-APR-1989   Rajendran Raja
C-   Updated  15-Aug-1991   Herbert Greenlee
C-       Added machine-dependent includes.
C-       Added missing arguments in smg calls
C-   Updated  20-AUG-1992   sss - compile on ibm
C-                
C-   Updated   4-JUN-1993   James T. Linnemann  add LI and ID commands 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
C&IF VAXVMS
      INCLUDE '($SMGDEF)'
      INCLUDE '($TRMDEF)'
C&ELSE
C&      INCLUDE 'D0$INC:SMGDEF.DEF'
C&      INCLUDE 'D0$INC:TRMDEF.DEF'
C&ENDIF
      INTEGER MAXROW1,NSCROL1,NCOL1(2),PCOL1(2),MAXSCROLL,MAXCOL
      INTEGER PAGE,WIDTH,SCROLL
      INTEGER MAXROW,NSCROL,MINROW,ILO,IHI,RLEN
      PARAMETER (MAXROW1=22,NSCROL1=15,MINROW=5)
      INTEGER SMG$REPAINT_SCREEN,SMG$PUT_LINE
      INTEGER SMG$REPASTE_VIRTUAL_DISPLAY
      INTEGER SMG$CREATE_VIRTUAL_DISPLAY,SMG$PASTE_VIRTUAL_DISPLAY
      INTEGER SMG$SAVE_PHYSICAL_SCREEN,SMG$RESTORE_PHYSICAL_SCREEN
      INTEGER SMG$PUT_CHARS,SMG$CURSOR_ROW,SMG$CREATE_VIRTUAL_KEYBOARD
      INTEGER SMG$CHANGE_RENDITION,SMG$READ_STRING,SMG$SET_CURSOR_ABS
      INTEGER SMG$DELETE_VIRTUAL_DISPLAY,SMG$DELETE_VIRTUAL_KEYBOARD
      INTEGER SMG$SCROLL_DISPLAY_AREA,SMG$ERASE_DISPLAY
      INTEGER SMG$MOVE_VIRTUAL_DISPLAY
      INTEGER IW1
      INTEGER IDUM
      REAL VB(*),VROW,VALUEX
      CHARACTER*(*) VA(*)
      INTEGER I,NA,NB,PBID,KBID,COL,MODD,MOD
      logical status
      integer*2 term
      INTEGER IW,ROW,J,JL,JH,IS,FIRST,TYPE,I1,I2
      INTEGER N(2),NROW(2),NCOL(2),WIND(2),PCOL(2),IFRST(2),DIR(2)
      INTEGER NLO(2),NHI(2)
      INTEGER LNEXT,NMOVE,ID
      LOGICAL AUTOB
      CHARACTER*(*) CHFA,CHFB
      CHARACTER*20 FMTA,FMTB,AUTO_FMT
      INTEGER MXT
      PARAMETER( MXT = 80 )
      CHARACTER*(MXT) TCHR
      CHARACTER*100 LINE
      INTEGER RC
      PARAMETER( RC = 2 )
C
      REAL FNEXT
      INTEGER INEXT,NL
      EQUIVALENCE (FNEXT,INEXT)
C
      INCLUDE 'D0$INC:AUTOF.INC'
C
      DATA NCOL1/78,23/,PCOL1/2,57/
C&IF IBMAIX
C&      integer vec1(3)
C&      pointer (ptvec1, vec1)
C&ENDIF
C
      NEWBNK = ' '                      ! no new bank to go to from here.
      LNEXT = 0                         ! No bank to be browsed after this
      NMOVE = 0                         ! No movement on linear chain requested
      ID = 0                            ! No ID to search for on linear chain
C&IF IBMAIX   ! (*sigh*)
C&      ptvec1 = loc (vb(1)) - 4*(loc (vb(2)) - loc (vb(1)))
C&      inext = vec1(1)
C&ELSE
      FNEXT = VB(-3)
C&ENDIF
      NL = INEXT                        ! Number of links in this bank
      IF(NA.EQ.0)THEN
        DO I = 1 , MINROW
          VA(I) = ' '                   ! PADDING IT.
        ENDDO
        NA = MINROW                     ! DO 5 ANYWAY
      ENDIF
C
      N(1)=NA
      N(2)=NB+NL+9
      IF(NB.EQ.-999)N(2)=0              ! DWRUP
C
      IF(N(1).LE.0.AND.N(2).LE.0) RETURN
C
      NLO(1) = 1                        ! scroll limits
      NHI(1) = NA
C
      NLO(2) = -NL-8
      NHI(2) = NB
C
      FMTA=CHFA   !No AUTO_FMT for Display A
      FMTB = CHFB                       ! Default
C
      IF(CHFB.EQ.'AUTO'.OR.CHFB.EQ.'auto') THEN
        AUTOB=.TRUE.
      ELSE
        AUTOB=.FALSE.
        FMTB=CHFB
      ENDIF
C
      MODD=TRM$M_TM_ESCAPE.OR.TRM$M_TM_NOECHO.OR.TRM$M_TM_PURGE
      STATUS=SMG$CREATE_VIRTUAL_KEYBOARD(KBID)
C
      DO 300 IW=1,2
C        IF(N(IW).LE.0.AND.IW.EQ.1) GO TO 300    ! PERMIT ZERO DATA ZEBRA
C                                        ! BANKS TO BE DISPLAYED
C     BIG SCREENS
        MAXROW =MAX(MAXROW,MAXROW1)
        NROW(IW)=MIN(MAXROW,N(IW))
        NCOL(1)   = MAX(NCOL1(1),MAXCOL-2)
        PCOL(1)   = 2
        NCOL(2)   = 23
        PCOL(2)   = MAX(PCOL1(2),MAXCOL-23)
        NSCROL    = MAX(NSCROL1,MAXSCROLL)
C        
        IF(NROW(IW).LT.MINROW.AND.NB.NE.-999)NROW(IW)=MINROW
        STATUS=SMG$CREATE_VIRTUAL_DISPLAY(NROW(IW),NCOL(IW),
     &                                      WIND(IW),SMG$M_BORDER,
     &                                      %VAL(0),%VAL(0))
        STATUS=SMG$PASTE_VIRTUAL_DISPLAY(WIND(IW),PBID,2,PCOL(IW),
     &                                   %VAL(0))
        IFRST(IW) = NHI(IW)-NROW(IW)+1
        IF(IW.EQ.1)THEN
          IFRST(IW) = MIN(IFRST(IW),1)
        ENDIF
        DIR(IW) = 1
        IF(IW.EQ.2)THEN
          IFRST(IW) = MIN(IFRST(IW),-8) ! START BANK DISPLAY FROM 1ST LINK.
        ENDIF
C
        IF(IW.EQ.1)
     &    CALL ZB_SMG_PN1(WIND(IW),VA(1),FMTA,1,IFRST(IW),NROW(IW))
C
        IF(IW.EQ.2)
     &    CALL ZB_SMG_PN2(WIND(IW),VB(1),FMTB,1,IFRST(IW),NROW(IW))
C
  300 CONTINUE
      IW=2
      IF(N(2).LE.0) IW=1                ! DEFAULT CURSOR ON ZEBRA BANK.
      FIRST = IFRST(IW)
C
      STATUS=SMG$SET_CURSOR_ABS(WIND(IW),1,1)
C
 1000 TCHR = ' '
      STATUS=SMG$READ_STRING(KBID,TCHR,%VAL(0),MXT,MODD,
     &                       %VAL(0),%VAL(0),%VAL(0),
     &                       TERM,WIND(IW),%VAL(0),%VAL(0),%VAL(0))
      IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
      CALL UPCASE(TCHR,TCHR)
      IF(TCHR.EQ.' '.AND.TERM.EQ.SMG$K_TRM_CR) GO TO 3000
C
      IF(TERM.EQ.SMG$K_TRM_UP.OR.TCHR(1:RC).EQ.'U ') THEN
        DIR(IW) = -1
        GO TO 1001
      ELSEIF(TERM.EQ.SMG$K_TRM_DOWN.OR.TCHR(1:RC).EQ.'D ') THEN
        DIR(IW) = +1
        GO TO 1001
      ELSEIF(TERM.EQ.SMG$K_TRM_RIGHT.OR.TCHR(1:RC).EQ.'R ') THEN
        IF(NB.NE.-999)THEN
          IW=2
          STATUS=SMG$REPASTE_VIRTUAL_DISPLAY(WIND(IW),PBID,2,PCOL(IW),
     &                                       %VAL(0))
C Bring to front
        ENDIF
        GO TO 1300
      ELSEIF(TERM.EQ.SMG$K_TRM_LEFT.OR.TCHR(1:RC).EQ.'L ')THEN
        IF(N(1).GT.0)THEN
          IW=1
          STATUS=SMG$REPASTE_VIRTUAL_DISPLAY(WIND(IW),PBID,2,PCOL(IW),
     &                                       %VAL(0))
C Bring to front
        ENDIF
        GO TO 1300
      ELSEIF(TCHR(1:RC).EQ.'F '.AND.IW.EQ.2) THEN
        FMTB = '(I5,G10.2)'
        GO TO 1299
      ELSEIF(TCHR(1:RC).EQ.'C '.AND.IW.EQ.2) THEN
        FMTB = '(I5,2X,A8)'
        GO TO 1299
      ELSEIF(TCHR(1:RC).EQ.'I '.AND.IW.EQ.2) THEN
        FMTB = '(I5,I10)'
        GO TO 1299
      ELSEIF(TCHR(1:RC).EQ.'X '.AND.IW.EQ.2) THEN
        FMTB = '(I5,Z9,1X,''X'')'
        GO TO 1299
      ELSEIF(TCHR(1:RC).EQ.'O '.AND.IW.EQ.2) THEN
        FMTB = '(I5,O12,1X,''O'')'
        GO TO 1299
      ELSEIF(TCHR(1:RC).EQ.'A '.AND.IW.EQ.2) THEN
        FMTB = 'AUTO'
        GO TO 1299
      ELSEIF(TCHR(1:RC).EQ.'BK') THEN        ! Get another bank
        NEWBNK = 'NEW'
        GO TO 3000
      ELSEIF((TCHR(1:RC).EQ.'HE').OR.(TCHR(1:RC).EQ.'H ')) THEN ! Get help file
        NEWBNK = 'HELP'
        GO TO 3000
C
      ELSEIF(TCHR(1:RC).EQ.'CH') THEN        ! display CHFORM
C
        IW = 1
        STATUS = SMG$ERASE_DISPLAY(WIND(IW),
     &                             %VAL(0),%VAL(0),%VAL(0),%VAL(0))
        STATUS = SMG$SET_CURSOR_ABS(WIND(IW),10,1)
C
        STATUS=SMG$PUT_LINE(WIND(IW),
     &  'The CHFORM block for this bank is  '
     &  ,%VAL(0),%VAL(0),%VAL(0),%VAL(0),%VAL(0),SMG$M_UP)
        STATUS=SMG$PUT_LINE(WIND(IW),
     &  CHFORM
     &  ,%VAL(0),%VAL(0),%VAL(0),%VAL(0),%VAL(0),SMG$M_UP)
        STATUS=SMG$PUT_LINE(WIND(IW),
     &  'Hit Return to Proceed: '
     &  ,%VAL(0),%VAL(0),%VAL(0),%VAL(0),%VAL(0),SMG$M_UP)
C
        STATUS=SMG$READ_STRING(KBID,TCHR,%VAL(0),MXT,MODD,
     &                         %VAL(0),%VAL(0),%VAL(0),TERM,WIND(IW),
     &                         %VAL(0),%VAL(0),%VAL(0))
        IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
        CALL UPCASE(TCHR,TCHR)
        IF(TCHR.EQ.' '.AND.TERM.EQ.SMG$K_TRM_CR) THEN
C
          STATUS = SMG$SET_CURSOR_ABS(WIND(IW),1,1)
          IF(IW.EQ.1)THEN
            CALL ZB_SMG_PN1(WIND(IW),VA(1),FMTA,1,IFRST(IW),NROW(IW))
          ENDIF
C
C RESTORE WINDOW
C
          GO TO 1300
        ENDIF
        GOTO 1001
      ELSEIF(TCHR(1:RC).EQ.'P ') THEN        ! Print Bank to a file
C
        IW = 1
        STATUS = SMG$ERASE_DISPLAY(WIND(IW),
     &                             %VAL(0),%VAL(0),%VAL(0),%VAL(0))

        STATUS = SMG$SET_CURSOR_ABS(WIND(IW),10,1)
C
        STATUS=SMG$READ_STRING(KBID,TCHR,'Name of File: ',
     &    MXT,0,%VAL(0),%VAL(0),RLEN,TERM,WIND(IW),
     &    %VAL(0),%VAL(0),%VAL(0))
        IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
        CALL UPCASE(TCHR,TCHR)
C
        CALL PRNT_BANK(TCHR,VB(1),FMTB) ! Print bank to file
C
        STATUS = SMG$SET_CURSOR_ABS(WIND(IW),1,1)
        IF(IW.EQ.1)THEN
          CALL ZB_SMG_PN1(WIND(IW),VA(1),FMTA,1,IFRST(IW),NROW(IW))
        ENDIF
C
C RESTORE WINDOW
C
        GO TO 1300
      ELSEIF(TCHR(1:RC).EQ.'LQ'.AND.IW.EQ.2) THEN        ! find another bank
        LNEXT = VALUEX(TCHR(3:),I1,I2,TYPE)
        IF(LNEXT.GT.1.OR.LNEXT.LT.-NL)GO TO 1000
C
C only permit going to the bank hanging off, if it is a genuine bank.
C THE ORIGIN link at LQ=2  points to the supporting link and not the bank
C So browsing to this link is not allowed.
C Also below NL there are no more links
C
        FNEXT = VB(LNEXT-8)               ! to avoid conversion. VB is real
C 8 OFF SET TO GET INTO LQ SPACE. (LQ(0) = IQ(-8))
        LNEXT = INEXT
        GO TO 3000
      ELSEIF ((TCHR(1:RC).EQ.'LI').AND.(IW.EQ.2)) THEN !move on linear chain
        NMOVE = VALUEX(TCHR(3:),I1,I2,TYPE)
        IF(NMOVE.EQ.0) GO TO 1000
        GO TO 3000
      ELSEIF ((TCHR(1:RC).EQ.'ID').AND.(IW.EQ.2)) THEN !search on linear chain
        ID = VALUEX(TCHR(3:),I1,I2,TYPE)
        IF (ID.EQ.0) GO TO 1000
        GO TO 3000
      ELSE
C test if TCHR is an integer
        VROW=VALUEX(TCHR,I1,I2,TYPE)
        IF(TYPE.NE.1)GO TO 1300
C
C INTEGER TYPED IN.
C
        FIRST = VROW
        FIRST = MAX(FIRST,NLO(IW))
        FIRST = MIN(FIRST,NHI(IW)-NROW(IW)+1)       ! Scroll limits
C
        IFRST(IW)=FIRST
        DIR(IW) = -1
C
        IF(IW.EQ.1)
     &    CALL ZB_SMG_PN1(WIND(IW),VA(1),FMTA,1,IFRST(IW),NROW(IW))
      ENDIF
 1299 IF(IW.EQ.2)
     &  CALL ZB_SMG_PN2(WIND(IW),VB(1),FMTB,1,IFRST(IW),NROW(IW))
      GO TO 1300
C
 1001 CONTINUE
      IF(DIR(IW).EQ.-1)THEN                 ! SCROLL DOWN.
        STATUS = SMG$SET_CURSOR_ABS(WIND(IW),1,1)
        ILO = IFRST(IW)
        IHI = IFRST(IW)-NSCROL
C
        IHI = MAX(IHI,NLO(IW))
        ILO = MAX(ILO,NLO(IW))          ! Scroll limits
C
        IFRST(IW) = IHI               ! AFTER SCROLL
      ELSEIF(DIR(IW).EQ.1)THEN
        STATUS = SMG$SET_CURSOR_ABS(WIND(IW),NROW(IW),1)  ! SET IT ON
C                                        ! BOTTOM LINE
        ILO = IFRST(IW)+NROW(IW)-1
        IHI = IFRST(IW)+NROW(IW)+NSCROL-1
C
        IHI = MIN(IHI,NHI(IW))
        ILO = MIN(ILO,NHI(IW))          ! SCROLL limits
C
        IFRST(IW) = IHI-NROW(IW)+1      ! AFTER SCROLL
C
      ENDIF
      DO 1002 IS = ILO,IHI,DIR(IW)
        IF(IW.EQ.1)
     &    CALL ZB_SMG_PN1(WIND(IW),VA(1),FMTA,DIR(IW),IS,1)
C
        IF(IW.EQ.2)
     &    CALL ZB_SMG_PN2(WIND(IW),VB(1),FMTB,DIR(IW),IS,1)
 1002 CONTINUE
 1300 CONTINUE
C
      STATUS=SMG$SET_CURSOR_ABS(WIND(IW),1,1)
      GO TO 1000
 3000 CONTINUE
      STATUS=SMG$DELETE_VIRTUAL_DISPLAY(WIND(2))
      IF(N(1).GT.0) STATUS=SMG$DELETE_VIRTUAL_DISPLAY(WIND(1))
      STATUS=SMG$DELETE_VIRTUAL_KEYBOARD(KBID)
      RETURN
C#######################################################################
      ENTRY VBROWSE_SCREEN(PAGE,WIDTH,SCROLL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   9-SEP-1992   Chip Stewart
C-
C----------------------------------------------------------------------
      MAXROW    = PAGE-2
      MAXCOL    = WIDTH
      MAXSCROLL = SCROLL-5
      END
