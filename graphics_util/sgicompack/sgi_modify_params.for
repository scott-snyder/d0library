      SUBROUTINE MODIFY_PARAMS(NVALS,NAMES,VALUES,PHELP,MODFLG)
C----------------------------------------------------------------------
C-  Purpose and Methods : Displays a list of parameters names and values
C-  and allows the user to modify them.  The display will let you select
C-  the parameter you want to modify by using the mouse.
C   The array MODFLG will be set on the elements that were modified.
C-       
C-   Inputs  : NVALS [ I ]:
C-             NAMES [ C*]: Names of the parameters to be displayed
C-             VALUES[ C*]: Values of the parameters
C-             PHELP [ C*]: String array with comments (help) about parameters
C-             
C-   Outputs : VALUES[ C*]: Values of the parameters
C-             MODFLG[ L*]: Logical array that will indicate what element has
C-                          been modified.
C-                          
C-   Created  25-JUN-1991   Michael Shupe
C-   Updated  13-AUG-1992   Lupe Howell  Redraw the menu if window damage
C- 				ocurrs
C----------------------------------------------------------------------
C
      INCLUDE 'fgl.h'
      INCLUDE 'fdevice.h'
C
      CHARACTER*(*) NAMES(*)
      CHARACTER*(*) VALUES(*)
      CHARACTER*(*) PHELP(*)
      LOGICAL MODFLG(*)
      INTEGER NVALS
C
      INTEGER ISCRX,ISCRY,IWOLD,IDY,IDYL,NPITM,MYWD,MYHT
      INTEGER IY0,IX0,IXMAX,IYMAX,IXL,IXL2,IWDID,I,IYBASE,IYLINE
      INTEGER MYL,IDEV,IRY,ICHO,NCHARS,IXM,IXMV,IYT
      INTEGER*2 QVAL
      CHARACTER*17 TITLE
      CHARACTER*80 CHARBF
      CHARACTER*1 CHR,CHRBUF(80)
      EQUIVALENCE (CHARBF,CHRBUF)
C
      DATA TITLE/'Modify Parameters'/
      DATA ISCRX,ISCRY/1280,1024/
C---------------------------------------------------------------------
C
      ISCRX=GETGDE(GDXPMA)
      ISCRY=GETGDE(GDYPMA)
      IWOLD=WINGET()
C
C *** Define the size of the WDID window
C
      IDY=20
c      IDY=1.5*FLOAT(GETHEI())
      IDYL=.2*IDY
      NPITM=NVALS
      MYWD=.9*FLOAT(ISCRX)
      MYHT=(NPITM+3)*IDY
      IY0=3*IDY
      IX0=3*IDY
      IXMAX=IX0+MYWD
      IYMAX=IY0+MYHT
      IXL=IX0+IDY
      IXL2=IX0+.35*FLOAT(MYWD)
      CALL PREFPO(IX0,IXMAX,IY0,IYMAX)
C
C *** Create window
C
      IWDID=WINOPE(TITLE,17)
      CALL WINTIT(TITLE,17)
C
C *** Set current window
C
      CALL WINSET(IWDID)
      CALL FRONTB(.TRUE.)
      CALL ORTHO2(FLOAT(IX0),FLOAT(IXMAX),FLOAT(IY0),FLOAT(IYMAX))
C
C *** Set up the mouse
C
      CALL QDEVIC(LEFTMO)
      CALL QDEVIC(KEYBD)
      CALL QRESET()

C
C *** Initialize the modfiy flags to FALSE 
C
      DO 5 I=1,NVALS
        MODFLG(I)=.FALSE.
    5 CONTINUE
C
C *** Write items and item box boundaries
C
    8 CALL COLOR(WHITE)
      CALL CLEAR
      DO 10 I=1,NPITM+1
        CALL COLOR(RED)
        IYBASE=IY0+IDY*(NPITM-I+3)       
        IYLINE=IYBASE+IDYL               
        CALL CMOV2(FLOAT(IXL),FLOAT(IYLINE))
        IF(I.LE.NPITM) THEN
          CALL CHARST(NAMES(I),MYL(NAMES(I)))
          CALL CMOV2(FLOAT(IXL2+5),FLOAT(IYLINE))
          CALL CHARST(VALUES(I),MYL(VALUES(I)))
        ELSE
          CALL CHARST('>>>>>DONE. EXIT.',16)
        ENDIF
        CALL COLOR(BLUE)
        CALL MOVE2(FLOAT(IX0),FLOAT(IYBASE))
        CALL DRAW2(FLOAT(IXMAX),FLOAT(IYBASE))
   10 CONTINUE
C
      CALL MOVE2(FLOAT(IXL2),FLOAT(IYLINE))
      CALL DRAW2(FLOAT(IXL2),FLOAT(IYMAX))
C
C *** Look for mouse activity
C
   15 CONTINUE
      IDEV=QREAD(QVAL)
C 
C *** If window needs to be redrawn do ir
C
      IF ( IDEV .EQ. REDRAW ) THEN
	CALL RESHAP()
	GO TO 8
      ENDIF
      IF(IDEV.NE.LEFTMO) GO TO 15
      IF(QVAL.NE.1) GO TO 15
      IRY=GETVAL(MOUSEY)
      ICHO=NPITM-((IRY-IY0)/IDY)+3
      IF(ICHO.EQ.NPITM+1) GO TO 300
      IF((ICHO.GT.0).AND.(ICHO.LE.NPITM)) GO TO 18
      GO TO 15
C
C  ***** Modifying the item *****
C  ***** Ask for modified value
C
   18 CONTINUE
      NCHARS=0
      IXM=IX0+.60*FLOAT(MYWD)
      IXMV=IXM+8*IDY
      IYT=IY0+IDY*(NPITM-ICHO+3)+IDYL
   22 CALL CMOV2(FLOAT(IXM),FLOAT(IYT))
      CALL COLOR(RED)
      CALL CHARST('NEW VAL:',8)
      CALL COLOR(BLACK)
      IF(NCHARS.GT.0) THEN
        CALL CMOV2(FLOAT(IXMV),FLOAT(IYT))
        CALL CHARST(CHARBF(1:NCHARS),NCHARS)
      ENDIF
C
C *** Look for input
C
   20 CONTINUE
      IDEV=QREAD(QVAL)
      IF(IDEV.EQ.LEFTMO.AND.QVAL.EQ.1) GO TO 200
      IF(IDEV.NE.KEYBD) GO TO 20
      NCHARS=NCHARS+1
      CHR=CHAR(QVAL)
      CHRBUF(NCHARS)=CHR
      CALL CHARST(CHR,1)
      GO TO 20
C
C *** TEST FOR CARRIAGE RETURN
C
  100 IF(QVAL.EQ.13) GO TO 200
C
C *** TEST FOR BACKSPACE
C
      IF(QVAL.EQ.127) THEN
        NCHARS=NCHARS-1
        GO TO 22
      ENDIF
C
C *** TEST FOR CONTROL-U (KILL LINE)
C
      IF(QVAL.EQ.21) THEN
        NCHARS=0
        GO TO 22
      ENDIF
      CALL QRESET()
      GO TO 15
C
C *** ITEM MODIFICATION DONE, STORE IT
C
  200 MODFLG(ICHO)=.TRUE.
      VALUES(ICHO)= CHARBF(1:NCHARS)
      CALL QRESET()
      GO TO 15
C
  300 CONTINUE
      CALL WINCLO(IWDID)
      IF(IWOLD.GE.0) CALL WINSET(IWOLD)
      RETURN
      END
