      SUBROUTINE FCPUT(ILUN,CFNAM,CGNAM,CTAP,CCOM,IFILE,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write a data record to a file catalog.
C-     The last six characters of the CCOM field are overwritten by
C-     the current date
C-
C-   Inputs  : ILUN   - file logical unit
C-             CFNAM - Zebra file name
C-             CGNAM - Generic name
C-             CTAP  - Tape information 
C-             IFILE - File to write
C-   Outputs : 
C-   Controls: 
C-
C-   Created   8-NOV-1993   John D Hobbs
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      INTEGER ILUN,IFILE,IERR,ISTART,ILAST,I,IDATE,ITIME,IRFILE
      CHARACTER*(*) CFNAM,CGNAM,CTAP,CCOM,CRLF*4
      INTEGER IREC(JRECFC)
      REAL    QREC(JRECFC)
      EQUIVALENCE(IREC,QREC)
C----------------------------------------------------------------------
C
      CALL FCLSET(ILUN,IERR)
      IF( IERR.NE.0 ) THEN
        IERR = -4
        GOTO 999
      ENDIF
C
      CRLF=CHAR(13)//CHAR(10)//'  '
      CALL DATIME(IDATE,ITIME)
      I=LEN(CCOM)
      WRITE(CCOM(I-5:I),'(I6)') IDATE
C
      CALL FCHRD(ILUN,IRFILE,IERR)
      IF( IFILE.LE.0 ) THEN
         IF( IERR.NE.0 ) GOTO 901
         IFILE=IRFILE+1
      ENDIF
      DO I=1,JRECFC
         CALL UCTOH('    ',IREC(I),4,4)
      ENDDO
C
      CALL UCTOH(CFNAM,IREC,4,NFCFN)
      ISTART=NFCFN
      CALL UCTOH(CGNAM,IREC(ISTART/4+1),4,NFCGN)
      ISTART=ISTART+NFCGN
      CALL UCTOH(CTAP,IREC(ISTART/4+1),4,NFCTAP)
      ISTART=ISTART+NFCTAP
      CALL UCTOH(CCOM,IREC(ISTART/4+1),4,NFCCOM)
      CALL UCTOH(CRLF,IREC(JRECFC),4,4)
      WRITE(ILUN,REC=IFILE+1,ERR=902) IREC
      IF( IFILE.GT.IRFILE ) CALL FCHWRT(ILUN,IFILE,IERR)
C
  999 CONTINUE
      IERR=0
      RETURN
C
 901  CONTINUE
      IERR = -1
      RETURN
C
 902  CONTINUE
      IERR = -2
      RETURN
      END
