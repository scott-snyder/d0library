      SUBROUTINE DEV_OPEN_WINDOW(
     &             IDEV,TITLE,WX,WY,WDX,WDY,ICOL,IORE,IERR)
C  Open a window on graphics device IDEV.
C       IDEV   = 0 - Current terminal or workstation (GL, X, UIS)
C                1 - Postscript output file
C                2 - Tektronix output file
C                3 - Tektronix terminal
C                4 - QUIC (QMS, Talaris) output file
C       WX,WY,WDX,WDY  -  Window origin and widths in screen fractions
C       ICOL   = 0 - Black and white, even if a color device
C                1 - Use color if possible
C       IORE   = 0 - PORTRAIT, 1 - LANDSCAPE
C       IERR   = 0 For error free return.
C
      CHARACTER*(*) TITLE
      CHARACTER*8 STATST(2),STATU
      LOGICAL FIRST
C
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
C
      CHARACTER*20 FILNAM(6)
      DATA FILNAM/
     &  'POSTSCRIPT.DAT      ',
     &  'POSTCOLOR.DAT       ',
     &  'TEKTRONIX.DAT       ',
     &  'TEKCOLOR.DAT        ',
     &  'TALARIS.DAT         ',
     &  'QMSCOLOR.DAT        '/
C
      DATA STATST/'NEW     ','UNKNOWN '/
      DATA FIRST/.FALSE./
      SAVE
C------------------------------------------------------------------
C
      STATU=STATST(2)                    ! 'UNKNOWN'
C
      IERR=0
      IDV=IDEV
      IGSYNC=1
      WNX=WX
      WNY=WY
      WNDX=WDX
      WNDY=WDY
C  Initialize the modelling matrix
C      CALL D_MATUNI(TMODEL)
C  Initialize base and plane vectors.
C      CALL STBASE(1.,0.,0.)
C      CALL STPLAN(0.,1.,0.)
C      CALL STJUST(1,1)
C
      IF(IDEV.LT.0.OR.IDEV.GT.4) THEN
        IERR=-1
        TYPE *,' ***** BAD DEVICE NUMBER. '
        RETURN
      ENDIF
      IF(ICOL.LT.0.OR.ICOL.GT.1) ICOL=0
      ICLR=ICOL
C
      IF(IDEV.EQ.3) THEN
        CALL OPEN_TKWIND(TITLE)
        RETURN
      ENDIF
C
C  Open the file if needed
      IDRUNI=62
      IFL=(IDEV-1)*2+ICOL+1
      IF(IDEV.EQ.4) IFL=5+ICOL
      IF(IDEV.EQ.1.OR.IDEV.EQ.4) THEN
C!!!
        IHNAME=1
        IF(IHNAME.EQ.0) THEN
          OPEN(IDRUNI,FILE=FILNAM(IFL),STATUS=STATU,
     &       FORM='FORMATTED',ERR=900)
          TYPE *,' HARDCOPY TO FILE: ',FILNAM(IFL)
        ELSE
          OPEN(IDRUNI,FILE=HCFILNAM,STATUS=STATU,
     &       FORM='FORMATTED',ERR=900)
          TYPE *,' HARDCOPY TO FILE: ',HCFILNAM
        ENDIF
        IF(IDEV.EQ.1) THEN
          CALL PS_INIT
        ELSE
          CALL QM_INIT
        ENDIF
      ELSEIF(IDEV.EQ.2) THEN
        IF(IHNAME.EQ.0) THEN
          OPEN(IDRUNI,FILE=FILNAM(IFL),STATUS=STATU,
     &       FORM='UNFORMATTED',ERR=900)
          TYPE *,' HARDCOPY TO FILE: ',FILNAM(IFL)
        ELSE
          OPEN(IDRUNI,FILE=HFILNAME,STATUS=STATU,
     &       FORM='UNFORMATTED',ERR=900)
          TYPE *,' HARDCOPY TO FILE: ',HFILNAME
        ENDIF
C!!!IF NEEDED      CALL TK_INIT
      ENDIF
      RETURN
C
  900 TYPE *,' ***** UNABLE TO OPEN HARDCOPY FILE. '
      IERR=-1
      RETURN
      END
