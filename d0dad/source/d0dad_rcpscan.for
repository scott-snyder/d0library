      SUBROUTINE D0DAD_RCPSCAN(FNRCP,FINUE,UINECOPT,IERR)
C-------------------------------------------------------------------------
C  Scan a given input file to create the unsorted event catalog used in 
C  direct access zebra I/O. The input file information is read from
C  the RCP file FNRCP
C
C     NB:  This code is highly D0 specific and requires linking with:
C
C
C  Author:  John D. Hobbs
C  Date:     8-DEC-1993
C
C  INPUTS: 
C     FNAME  - C - Name (set) of input Zebra files to scan)
C     FUENAM - C - Name of output unsorted event catalog.
C     UECOPT - C - Options for unsorted event catalog OPEN STATUS
C     
C  OUTPUTS: 
C     IERR   - I - Error return, 0 ==> All is OK.
C-------------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) FNRCP,FINUE,UINECOPT*1
      INTEGER   IERR
C
      INCLUDE  'D0$INC:zebcom.inc'
      INCLUDE  'D0$INC:quest.inc'
      INCLUDE  'D0$INC:d0dadcom.inc'
      INCLUDE  'D0$INC:d0dad.inc'
C
      INTEGER     NEVTS,IFSN,ICNTXT,ILEN,IZEVT1,IZEVTN,IDATE,ITIME
      CHARACTER   RCPFNAM*128,CSTRM*6,CDT*20,CFX*4,FZTYPE*4
      CHARACTER*6 CVSN,CITL
      LOGICAL LIB$FIND_FILE
      INTEGER LENOCC
      EXTERNAL LENOCC,LIB$FIND_FILE
C-------------------------------------------------------------------------
C
      RCPFNAM=FNRCP
      FUENAM=FINUE
      UECOPT=UINECOPT
      FZTYPE='XXXX'
      CVSN='XXXXXX'
      CITL='XXXXXX'
C
      IF( LDDBG.GT.3 ) THEN
         WRITE(*,*) ' '
         WRITE(*,*) ' *** Entry into d0dad_rcpscan ***'
         WRITE(*,9901) FNRCP(1:LENOCC(FNRCP))
         WRITE(*,9902) 'UEvent ',FUENAM(1:LENOCC(FUENAM)),UECOPT(1:1)
 9901    FORMAT(' Input RCP file: ',A)
 9902    FORMAT(' ',A7,' filename: ',A,', Option: ',A1)
      ENDIF
C
C  Open input RCP file and scan for fields
C
      IERR=0
      CALL INRCP(RCPFNAM,IERR)
      IF( IERR.NE.0 ) GOTO 998
C    General fields
      CALL EZGET('OUTPUT_TABLE_NAME',FZTYPE,IERR) 
      CALL EZGET('FILE_FORMAT',CFX,IERR)
      CALL EZGET('OFFLINE_STREAM_NAME',CSTRM,IERR)
      CALL EZGET('OUTPUT_FILENAME',FNTMP,IERR)
      CALL EZGET('GENERIC_NAME',CGNAME,IERR)
      CALL EZGET_i('RECORD_LENGTH',LRECL,IERR)
      CALL EZGET_i('FIRST_EVENT',IZEVT1,IERR)
      CALL EZGET_i('LAST_EVENT',IZEVTN,IERR)
      CALL EZGET_i('EVENTS',NEVTS,IERR)
C    Output-type-specific fields
      IF( FZTYPE(1:3).EQ.'STA' ) THEN
         CALL EZGET('VISUAL_TAPE_LABEL',CVSN,IERR)
         CALL EZGET('INTERNAL_TAPE_LABEL',CITL,IERR)
         CALL EZGET_i('FILE_SEQUENCE_NUMBER',IFSN,IERR)
         CALL EZGET('CREATION_DATE',CDT,IERR)
      ELSEIF( FZTYPE(1:3).EQ.'DST' ) THEN
         CALL EZGET('PRODUCTION_DATE',CDT,IERR)
      ELSE
         IF( LDDBG.GT.2 ) WRITE(*,8001) FZTYPE
 8001    FORMAT(' D0DAD_RCPSCAN: Unknown file type: ',I10)
      ENDIF
C
C  Check for input file type errors
C
      IF( FZTYPE.NE.CZBTYP .AND. CZBTYP.NE.'XXXX' ) GOTO 997
      IF( CFX.NE.'FX' ) GOTO 996
C
C  Setup the control variables...
C
      CALL DATIME(IDATE,ITIME)
      WRITE(CTAPE,2001) CVSN,CITL,IFSN
 2001 FORMAT(A6,1X,A6,1X,I6)
      WRITE(CFCCOM,2002) CDT,CSTRM(1:LENOCC(CSTRM)),FZTYPE,IDATE
 2002 FORMAT(A20,' ',A4,' ',A4,'    ',I6)
C
C  Do the scan...
C
      CALL D0DAD_SCAN(FNTMP,FUENAM,UECOPT,IERR)
      IF( IERR.NE.0 ) GOTO 995
 999  CONTINUE
      RETURN
C
 994  CONTINUE
      IERR = -4
      RETURN
C
 995  CONTINUE
      IERR = IERR-100
      RETURN
C
 996  CONTINUE
      IERR = -1
      RETURN
C
 997  CONTINUE
      IERR = -2
      RETURN
C
 998  CONTINUE
      IERR = -3
      RETURN
C
      END
