      SUBROUTINE ZB_SMG_PN1(WIND,VECT,CHF,DIR,IFRST,NROW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Puts out a page of information
C-                         on the .ZEB file on SMG window 1
C-
C-   Inputs  : WIND = WINDOW address
C-             VECT = Vectors to be displayed
C-             CHF  = Character strings with formats for the variables
C-                    or Hollerith strings with a preceeding Int
C-                    or 'AUTO' for automatic formatting
C-             DIR = DIRECTION OF SCROLLING. +1 = SMG$M_UP,-1=SMG$M_DOWN
C-             IFRST= 1st row number to be plotted
C-             NROW = Lengths of the vectors.  0 means no display
C-
C-   Outputs : Display
C-   Created  10-APR-1989   Rajendran Raja
C-                          based on code by M.W. peters
C-   Updated  15-Aug-1991   Herbert Greenlee
C-                          Added machine-dependent includes.
C-      		    Added missing arguments in smg calls.
C-   Updated  20-AUG-1992   sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C&IF VAXVMS
      INCLUDE '($SMGDEF)'
      INCLUDE '($TRMDEF)'
C&ELSE
C&      INCLUDE 'D0$INC:SMGDEF.DEF'
C&      INCLUDE 'D0$INC:TRMDEF.DEF'
C&ENDIF
      INTEGER SMG$CREATE_VIRTUAL_DISPLAY,SMG$PASTE_VIRTUAL_DISPLAY
      INTEGER SMG$PUT_LINE,SMG$CURSOR_ROW,SMG$CREATE_VIRTUAL_KEYBOARD
      INTEGER SMG$CHANGE_RENDITION,SMG$READ_STRING,SMG$SET_CURSOR_ABS
      INTEGER SMG$DELETE_VIRTUAL_DISPLAY,SMG$DELETE_VIRTUAL_KEYBOARD
      INTEGER SMG$SCROLL_DISPLAY_AREA,SMG$ERASE_DISPLAY
C      INTEGER SMG$M_UP,SMG$M_DOWN
      CHARACTER*(*) VECT(*)
      INTEGER I,DIR,PBID,STATUS,NROW,IFRST,WIND
C
      CHARACTER*20 FMT,AUTO_FMT
      CHARACTER*(*) CHF
      CHARACTER*100 LINE
C
      INCLUDE 'D0$INC:AUTOF.INC'
C
C----------------------------------------------------------------------
      IF(NROW.EQ.0)RETURN
      DO 100 I=IFRST,IFRST-1+NROW
        LINE=' '
        IF(CHF.EQ.'AUTO'.OR.CHF.EQ.'auto')THEN
          IDATA = I
          FMT = AUTO_FMT()
        ELSE
          FMT = CHF
        ENDIF
        WRITE(LINE,FMT,ERR=80)I,VECT(I)
   80   IF(DIR.EQ.1)THEN
          STATUS=SMG$PUT_LINE(WIND,LINE,
     &                        %VAL(0),%VAL(0),%VAL(0),%VAL(0),%VAL(0),
     &                        SMG$M_UP)
        ELSEIF(DIR.EQ.-1)THEN
          STATUS=SMG$PUT_LINE(WIND,LINE,%
     &                        VAL(0),%VAL(0),%VAL(0),%VAL(0),%VAL(0),
     &                        SMG$M_DOWN)
        ENDIF
  100 CONTINUE
  999 RETURN
      END
