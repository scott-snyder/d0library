      SUBROUTINE ZB_SMG_PN2(WIND,VECT,CHF,DIR,IFRST,NROW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Puts out a page of information
C-                         of the Zebra bank SMG window 2
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
C-                          Added machine-dependent includes
C-                          Added missing arguments in smg calls
C-   Updated   9-JAN-1995   Chip Stewart  
C-                          Added integer values for alpha 
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
      REAL VECT(*),V
      INTEGER I,DIR,PBID,STATUS,NROW,IFRST,WIND,LEN3
      INTEGER II,IV
      EQUIVALENCE (V,IV)
C
      CHARACTER*60 FMT,AUTO_FMT,FMT1,FMT2
      CHARACTER*(*) CHF
      CHARACTER*100 LINE
C
      INCLUDE 'D0$INC:AUTOF.INC'
C
      CHARACTER*6 ZFLG(-8:0)            ! ZEBRA LINK AREA DEFNS
      DATA ZFLG/'NXL LQ','UPL LQ','ORG LQ','IDN IQ','IDH IQ',
     &  'NL  IQ','NS  IQ','ND  IQ','STA IQ'/
      CHARACTER*6 ZFLG1,ZFLG2,ZFLG3
      DATA ZFLG1/'DTA IQ'/,ZFLG2/'STR LQ'/,ZFLG3/'REF LQ'/
C
      REAL QNL,QNS
      INTEGER NL,NS
      EQUIVALENCE (QNL,NL),(QNS,NS)
C
      DOUBLE PRECISION DVECT
      REAL    RVECT(2)
      EQUIVALENCE (RVECT,DVECT)
      CHARACTER*22 DOUBLE
C&IF IBMAIX
C&      integer vec1(3)
C&      pointer (ptvec1, vec1)
C&ENDIF
C----------------------------------------------------------------------
      IF(NROW.EQ.0)RETURN
C&IF IBMAIX   ! wimpy ibm compiler complains about out-of-bounds indices...
C&      ptvec1 = loc (vect(1)) - 4*(loc (vect(2)) - loc (vect(1)))
C&      nl = vec1(1)
C&      ns = vec1(2)
C&ELSE
      QNL = VECT(-3)                    ! Number of links
      QNS = VECT(-2)                ! Number of structural links
C&ENDIF
      DO 100 I=IFRST,IFRST-1+NROW
        LINE=' '
        FMT = ' '
        FMT2 = ' '
        IF(I.LE.0)THEN                  ! BANK LINK AREA. Special
          II = I
          IF(I.LT.-5)II=II+8            ! in LQ space
          IF(I.GE.-8.AND.I.NE.-4)THEN               ! Fixed link area
            V = VECT(I)
            WRITE(LINE,'(A6,I5,I10)',ERR=81)ZFLG(I),II,IV
          ELSEIF(I.EQ.-4)THEN           ! Hollerith data
            V = VECT(I)
            WRITE(LINE,'(A6,I5,6X,A4)',ERR=81)ZFLG(I),II,IV
          ELSEIF(II.GE.-NS)THEN         ! Structural link area
            V = VECT(I)
            WRITE(LINE,'(A6,I5,I10)',ERR=81)ZFLG2,II,IV
          ELSEIF(II.LT.-NS.AND.II.GE.-NL)THEN         ! Reference link area
            V = VECT(I)
            WRITE(LINE,'(A6,I5,I10)',ERR=81)ZFLG3,II,IV
          ENDIF
        ELSE
          IF(CHF.EQ.'AUTO'.OR.CHF.EQ.'auto')THEN
            IDATA = I
            FMT = AUTO_FMT()
            FMT1 = '(A6,'//FMT
            CALL ADDSTR(FMT1,')',FMT2,LEN3)
          ELSE
            FMT = CHF
            FMT1 = '(A6,'//FMT
            CALL ADDSTR(FMT1,')',FMT2,LEN3)
          ENDIF
          IF(FMT.EQ.'D1')THEN
C
C ****  DOUBLE PRECISION FLOATING POINT HERE.
C
            RVECT(1) = VECT(I)
            RVECT(2) = VECT(I+1)
            WRITE(DOUBLE,75)DVECT
   75       FORMAT(D22.11)
            WRITE(LINE,74)ZFLG1,I,DOUBLE(1:11)
   74       FORMAT(A6,I5,1X,A11)
          ELSEIF(FMT.EQ.'D2')THEN
            RVECT(2) = VECT(I)
            RVECT(1) = VECT(I-1)
            WRITE(DOUBLE,75)DVECT
            WRITE(LINE,74)ZFLG1,I,DOUBLE(12:22)
          ELSE
            IF(FMT(5:7).EQ.'I10') THEN
              V = VECT(I)
              WRITE(LINE,FMT2(1:LEN3),ERR=81) ZFLG1,I,IV
            ELSE
              WRITE(LINE,FMT2(1:LEN3),ERR=81) ZFLG1,I,VECT(I)
            ENDIF
          ENDIF
        ENDIF
   81   CONTINUE
   80   IF(DIR.EQ.1)THEN
          STATUS=SMG$PUT_LINE(WIND,LINE,
     &                        %VAL(0),%VAL(0),%VAL(0),%VAL(0),%VAL(0),
     &                        SMG$M_UP)
        ELSEIF(DIR.EQ.-1)THEN
          STATUS=SMG$PUT_LINE(WIND,LINE,
     &                        %VAL(0),%VAL(0),%VAL(0),%VAL(0),%VAL(0),
     &                        SMG$M_DOWN)
        ENDIF
  100 CONTINUE
  999 RETURN
      END
