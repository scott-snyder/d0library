      SUBROUTINE ISP1_LINK(LGCAH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Takes GEANT PARENT_TRK OR ISAJET_TRK track number -
C-   from current GCAH BANK and finds link
C-   to the corresponding ISP1 bank. Then, if GCAH is already referenced,
C-   follows chain of links to last GCAH and adds a refernce link from
C-   last GCAH to current GCAH
C-
C-   Inputs  : LGCAH - ADDRESS TO GCAH BANK
C-   Outputs : none
C-   Controls: none
C-
C-   Created  22-FEB-1990   Chip Stewart
C-   Updated  12-MAY-1992   K. Wyatt Merritt  Protection and error message
C-                          for infinite loop in link search added 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISAE.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISV1.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISP1.LINK/LIST'
C
      REAL VERT(3),PVERT(4),UBUF(10)
      INTEGER PARENT_TRK,ISAJET_TRK
      INTEGER IPART,NVERT,NUBUF
      INTEGER I,LZFIND,IZLINK
      INTEGER ITRA,NL
      REAL VTMP(3,3),PTMP(4,2),TOTAL(4)
      INTEGER IDATA(8),NPOINT

C
      INTEGER LISAE,LISV1,LISP1,LGCAH,LINK,LNEXT
      INTEGER LSAVE
      INTEGER ITRAK,NTRAK
      CHARACTER*50 MSG
      INCLUDE 'D0$INC:GCNUM.INC'
      INTEGER NCHAIN
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
      IF(LGCAH.LE.0) GOTO 999
      ITRAK = IQ(LGCAH-5)
      IF(ITRAK.LE.0) GOTO 999
      NCHAIN = 0
C
C ****  get this track's parentage from GCAH
C
      CALL GTGCAH(ITRAK,ITRA,VTMP,PTMP,IDATA,TOTAL,NPOINT)
      IF(ITRA.NE.ITRAK) GOTO 999
      ISAJET_TRK = IDATA(7)
      PARENT_TRK = IDATA(4)

      IF (ISAJET_TRK.EQ. 0) THEN
        NTRAK = PARENT_TRK
        IF( NTRAK .EQ.0) THEN
          WRITE(MSG,'(A,2I5)') ' NO PARENT TRACK ' ,
     &      PARENT_TRK,ISAJET_TRK
          CALL ERRMSG('D0GEANT','ISP1_LINK',MSG,'W')
          GOTO 999
        END IF
C
C ****  get parent track's parentage from GEANT JKINE bank
C
    5   CALL GFKINE(NTRAK,VERT,PVERT,IPART,NVERT,UBUF,NUBUF)
        IF(UBUF(5).GT.0.0) THEN
          NTRAK = UBUF(5)
          GOTO 10
        ELSE IF(UBUF(2).GT.0.0) THEN
          NTRAK = UBUF(2)
          GOTO 5
        ELSE
          WRITE(MSG,'(A,2I5)')
     &      'UNEXPECTED UBUF FROM GFKINE ' ,NINT(UBUF(2)),NINT(UBUF(5))
          CALL ERRMSG('D0GEANT','ISP1_LINK',MSG,'W')
          GOTO 999
        END IF
      ELSE IF (PARENT_TRK .EQ. 0) THEN
        NTRAK = ISAJET_TRK
      ELSE
        WRITE(MSG,'(A,2I5)') 'no PARENT or ISP1 track' ,
     &    PARENT_TRK,ISAJET_TRK
        CALL ERRMSG('D0GEANT','ISP1_LINK',MSG,'W')
        GOTO 999
      END IF
   10 CONTINUE
C
C ****  found original ISP1 source track number - now find address
C
      LISAE = LQ(LHEAD-IZISAE)
      IF ( LISAE.LE.0 ) THEN
        WRITE(MSG,'(A,I5)')
     &    ' POINTER TO ISAE ' ,LISAE
        CALL ERRMSG('D0GEANT','ISP1_LINK',MSG,'W')
        GOTO 999
      ENDIF
      LISV1 = LQ(LISAE-IZISV1)
      IF ( LISV1.LE.0 ) THEN
        WRITE(MSG,'(A,I5)')
     &    ' POINTER TO ISV1 ' ,LISV1
        CALL ERRMSG('D0GEANT','ISP1_LINK',MSG,'W')
        GOTO 999
      ENDIF
C
  100 LISP1 = LQ(LISV1-IZISP1)
  200 IF ( LISP1.EQ.0 ) GO TO 110
      LINK = LZFIND(IXCOM,LISP1,NTRAK,-5)
      IF (LINK.GT.0) THEN
        GOTO 899
      END IF
C ****  loop over particles...
  110 CONTINUE
      LISV1 = LQ(LISV1)
C ****  loop over vertices...
      IF ( LISV1.LE.0 ) THEN
        WRITE(MSG,'(A,2I5)')
     &    ' NO ISP1 FOUND  IN ISP1_LINK ' ,
     &    PARENT_TRK,ISAJET_TRK
        CALL ERRMSG('D0GEANT','ISP1_LINK',MSG,'W')
        GOTO 999
      ELSE
        GO TO 100
      END IF
C
  899 CONTINUE
C
C ****  CHECK FOR PREVIOUS GCAH BANKS LINKED TO THIS ISP1
C
      IZLINK = 5
      NL = IQ(LINK-3)
      IF(NL.LT.IZLINK) THEN
        WRITE(MSG,'(A)')
     &    ' NO REFERENCE LINK IN ISP1 BANK '
        CALL ERRMSG('D0GEANT','ISP1_LINK',MSG,'W')
        GOTO 999
      END IF
  300 LNEXT = LQ(LINK-IZLINK)
      LSAVE = LQ(LNEXT-IZLINK)
      IF (LSAVE .EQ. LINK) THEN
        WRITE (6,8010) LINK,IZLINK,LNEXT,LSAVE
        LQ(LINK-IZLINK) = LGCAH
        CALL PRGCAH(6,0,0,'ALL',0)
        CALL PRTEVZ(6)
        GO TO 999
      ENDIF
 8010 FORMAT(5X,'Infinite loop in GCAH chain: ',/,
     &  ' LINK ',I10,' IZLINK ',I3,' LNEXT ',I10,' LSAVE ',I10)
      NCHAIN = NCHAIN + 1
      IF (NCHAIN .GT. 1000) THEN
        WRITE (MSG,'(A,2I10)')
     &    ' GCAH CHAIN TOO LONG: LNEXT ',LNEXT,NCHAIN
        CALL ERRMSG('GCAH ERR1','ISP1_LINK',MSG,'W')
        WRITE (6,8000) ITRAK,ISAJET_TRK,PARENT_TRK,NTRAK,NTRACK
 8000   FORMAT(5X,'ITRAK',I5,' ISA TRK ',I5,' PAR TRK ',I5,'NTRAK',
     &    I5,' # IN JKINE ',I5)
        CALL PRGCAH(6,0,0,'ALL',0)
        CALL PRTEVZ(6)
        GO TO 999
      ENDIF
      IF ( LNEXT.GT.0) THEN
        LINK = LNEXT
        IZLINK = 1
        GOTO 300
      ELSE
        LQ(LINK-IZLINK) = LGCAH
      END IF
  999 CONTINUE
      RETURN
      END
