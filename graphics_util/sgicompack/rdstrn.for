      SUBROUTINE RDSTRN(STRNG,NMAX,NREAD,ICOL,IBCKCL)
C-------------------------------------------------------------------
C-
C-  Read from keyboard stream of characters terminated by CR
C-  and/or LF.  Characters are echoed as they are typed in.
C-
C-  INPUT:    NMAX   [I]: Max length of string to be read (<= 80)
C-            ICOL   [I]: Color for the characters entered
C-            IBCKCL [I]: Background colour
C-
C-  OUTPUT:   STRING[C*]: String containgn the values entered by the user
C-            NREAD  [I]: No of characters read up to (but not
C-                       including CR/LF) or NMAX
C-
C-   Created             Mike Shupe
C-   Updated 12-OCT-1992 Lupe Howell STRNG value set at the end
C-
C-------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'fgl.h'
      INCLUDE 'fdevice.h'
C
      INTEGER NMAX,ICOL,IBCKCL,NREAD
      CHARACTER*(*) STRNG
      
      CHARACTER*1 CHOLD
      INTEGER*2 ICX,ICY,QVAL
      INTEGER ICR,ILF,IDEL,IBS,IX1,IY1,IDEV
      REAL WID,CX4,CY4
      CHARACTER*80 TEMP
C
      DATA ICR,ILF/13,10/
      DATA IDEL,IBS/127,8/
C-------------------------------------------------------------------
      NREAD = 0
      TEMP = ' '
      CALL GETORI(IX1,IY1)
      CALL COLOR(ICOL)
C
C *** Read keyboard input character by character util CR or LF
C *** or until someone clicks right mouse
C
  100 CONTINUE
      IDEV = QREAD(QVAL)
      IF (IDEV.EQ.RIGHTM .AND. QVAL.EQ.1) GOTO 999
      IF (IDEV.NE.KEYBD) GOTO 100
C
C *** Echo typed characters
C
      IF (QVAL.EQ.ICR.OR.QVAL.EQ.ILF) THEN
        GOTO 999
      ELSEIF (QVAL.EQ.IDEL.OR.QVAL.EQ.IBS) THEN
        IF (NREAD.GT.0) THEN
          CHOLD = STRNG(NREAD:NREAD)
          NREAD = NREAD-1
          WID = FLOAT( STRWID(CHOLD,1) )
          CALL GETCPO(ICX,ICY)
          CX4 = FLOAT(ICX-IX1) - WID
          CY4 = FLOAT(ICY-IY1)
          CALL CMOV2(CX4,CY4)
          ICOL = GETCOL()
          CALL COLOR(IBCKCL)
          CALL CHARST(CHOLD,1)
          CALL COLOR(ICOL)
          CALL CMOV2(CX4,CY4)
        ENDIF
      ELSE
        NREAD = NREAD+1
        IF (NREAD.GT.NMAX) THEN
          NREAD = NMAX
          GOTO 999
        ENDIF
        TEMP(NREAD:NREAD) = CHAR(QVAL)
        CALL CHARST(TEMP(NREAD:NREAD),1)
      ENDIF
C
      GOTO 100
  999 CONTINUE
      STRNG = TEMP(1:NREAD)
      RETURN
      END
