      SUBROUTINE GMUH_ISP1_LINK(LGMUH1)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Takes GEANT PARENT_TRK OR ISAJET_TRK track number -
C-   from current GMUH BANK and finds link
C-   to the corresponding ISP1 bank. Then, if GMUH is already referenced,
C-   follows chain of links to last GMUH and adds a refernce link from
C-   last GMUH to current GMUH
C-
C-   Inputs  : LGMUH - ADDRESS TO GMUH BANK
C-   Outputs : none
C-   Controls: none
C-
C-   Created  22-MAY-1993   Jasbir Singh & Chip Stewart
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
      INTEGER PARENT_TRK,ISAJET_TRK,GZGMUH,ISAJET_VTX,NVTX
      INTEGER IPART,NVERT,NUBUF
      INTEGER I,LZFIND,IZLINK
      INTEGER ITRA,NL,HBANK
      REAL VTMP(3,3),PTMP(4,2),TOTAL(4)

C
      INTEGER LISAE,LISV1,LISP1,LGMUH,LINK,LNEXT,LGMUH1
      INTEGER LSAVE
      INTEGER ITRAK,NTRAK
      CHARACTER MSG*50,CBANK*4
      INCLUDE 'D0$INC:GCNUM.INC'
      INTEGER NCHAIN
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
      IF(LGMUH1.LE.0) THEN
        LGMUH = GZGMUH(0)
        IF(LGMUH.LE.0) GOTO 999
      ELSE
        LGMUH = LGMUH1
      END IF
    1 ITRAK = IQ(LGMUH-5)
      IF(ITRAK.LE.0) GOTO 999
      IF(ITRAK.GT.10000) GOTO 999
      NCHAIN = 0
C
C ****  get this track's parentage from GMUH
C
      ITRA = IQ(LGMUH-5)                ! TRACK NUMBER
      ISAJET_TRK = IQ(LGMUH+17)
      ISAJET_VTX = IQ(LGMUH+16)
      IF (ISAJET_TRK.EQ. 0) THEN
        WRITE(MSG,'(A,2I5)') ' NO PARENT TRACK ' ,
     &      PARENT_TRK,ISAJET_TRK
        CALL ERRMSG('D0GEANT','GMUH_ISP1_LINK',MSG,'W')
        GOTO 999
      END IF
C
      NVTX  = ISAJET_VTX
      NTRAK = ISAJET_TRK
   10 CONTINUE
C
C ****  found original ISP1 source track number - now find address
C
      LISAE = LQ(LHEAD-IZISAE)
      IF ( LISAE.LE.0 ) THEN
        WRITE(MSG,'(A,I5)')
     &    ' POINTER TO ISAE ' ,LISAE
        CALL ERRMSG('MUONLIB','GMUH_ISP1_LINK',MSG,'W')
        GOTO 999
      ENDIF
      LISV1 = LQ(LISAE-IZISV1)
      LISV1 = LZFIND(IXCOM,LISV1,NVTX,-5)
      IF ( LISV1.LE.0 ) THEN
        WRITE(MSG,'(A,I5)')
     &    ' POINTER TO ISV1 ' ,NVTX
        CALL ERRMSG('MULIB','GMUH_ISP1_LINK',MSG,'W')
        GOTO 999
      ENDIF
C
  100 LISP1 = LQ(LISV1-IZISP1)
  200 IF ( LISP1.GT.0 ) LISP1 = LZFIND(IXCOM,LISP1,NTRAK,-5)
      IF ( LISP1.LE.0 ) THEN
        WRITE(MSG,'(A,2I5)')
     &    ' POINTER TO ISP1 NVTX NTRAK ' ,NVTX,NTRAK
        CALL ERRMSG('MULIB','GMUH_ISP1_LINK',MSG,'W')
        GOTO 999
      ENDIF
      LINK = LISP1
C
C ****  CHECK FOR PREVIOUS GMUH BANKS LINKED TO THIS ISP1
C
      IZLINK = 5
      NL = IQ(LINK-3)
      IF(NL.LT.IZLINK) THEN
        WRITE(MSG,'(A)')
     &    ' NO REFERENCE LINK IN ISP1 BANK '
        CALL ERRMSG('MAKE_MUONLIB','GMUH_ISP1_LINK',MSG,'W')
        GOTO 999
      END IF
  300 LNEXT = LQ(LINK-IZLINK)
      IF(LNEXT.GT.0) THEN
        HBANK = IQ(LNEXT-4)
        CALL DHTOC(4,HBANK,CBANK)
        IF(CBANK.EQ.'GCAH') LNEXT = 0
      END IF
      LSAVE = 0
      IF(LNEXT.GT.0) LSAVE = LQ(LNEXT-IZLINK)
      IF (LSAVE .EQ. LINK) THEN
        WRITE (MSG,8010) LINK,IZLINK,LNEXT,LSAVE
        CALL ERRMSG('GMUH ERR2','GMUH_ISP1_LINK',MSG,'W')
        LQ(LINK-IZLINK) = LGMUH
        GO TO 999
      ENDIF

 8010 FORMAT('Inf LINK ',I10,' IZLINK ',I3,' LNEXT ',I10,' LSAVE ',I10)
      NCHAIN = NCHAIN + 1
      IF (NCHAIN .GT. 1000) THEN
        WRITE (MSG,'(A,2I10)')
     &    ' GMUH CHAIN TOO LONG: LNEXT ',LNEXT,NCHAIN
        CALL ERRMSG('GMUH ERR1','GMUH_ISP1_LINK',MSG,'W')
        WRITE (6,8000) ITRAK,ISAJET_TRK,PARENT_TRK,NTRAK,NTRACK
 8000   FORMAT(5X,'ITRAK',I5,' ISA TRK ',I5,' PAR TRK ',I5,'NTRAK',
     &    I5,' # IN JKINE ',I5)
        GO TO 999
      ENDIF
      IF ( LNEXT.GT.0) THEN
        LINK = LNEXT
        IZLINK = 1
        GOTO 300
      ELSE
        LQ(LINK-IZLINK) = LGMUH
      END IF
C
C ****  CHECK for next GMUH and loop back if exists
C
      LGMUH = LQ(LGMUH)
      IF(LGMUH.GT.0) GOTO 1
  999 CONTINUE
      RETURN
      END
