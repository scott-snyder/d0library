      SUBROUTINE D0DAD_INIT(CMDLIN,IDOPT,IERR)
C-------------------------------------------------------------------------
C  Initialize ZEBRA.
C
C  Author:  John D. Hobbs
C  Date:    8-DEC-1993
C-------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:zebcom.inc'
      INCLUDE  'D0$INC:quest.inc'
      INCLUDE  'D0$INC:d0dadcom.inc'
      INCLUDE  'D0$INC:d0dad.inc'
      INTEGER  IXWIPE,I,IDOPT,IFLAG,IERR,IKEY
      CHARACTER*1024 CMDLIN
      LOGICAL  LFIRST
      SAVE     LFIRST
      DATA     LFIRST/.TRUE./
C-------------------------------------------------------------------------
C
      IF( .NOT.LFIRST ) RETURN
      LFIRST=.FALSE.
C
C  Define defaults for standalone operation...
C
      ITEST=0
      IDOPT=IMUSER
      LDDBG=4
      CALL VZERO(DATTIM_STAMP,2)
C
      ISELR(1)=0
      ISELR(2)=0
      ISELE(1)=0
      ISELE(2)=0
      ISTBIT=-1
      ISTTYP=1
      IROFF=0
      IEOFF=0
C
C  Default file extensions and access modes...
C
      FZBNAM='file_names'
      FUENAM=' '
      FECNAM=' '
      FDFNAM=' '
      FFCNAM=' '
      FNTEXT='       '
      UECOPT='A'
      ECCOPT='A'
      FCCOPT='A'
      DFCOPT='W'
C
      LECDEV=.FALSE.
      LBLIND=.FALSE.
      LEVT_CHECK=.FALSE.
      HEADER_ONLY=.FALSE.
      PREFIX=' '
C
      CALL ERRMAX('UTAG_LOST',0,0)
C
C  Initialize a few character constants...
C
      DO I=1,LEN(CFNAME)
         CFNAME(I:I)=' '
      ENDDO
      DO I=1,LEN(CGNAME)
         CGNAME(I:I)=' '
      ENDDO
      DO I=1,LEN(CECTAG)
         CECTAG(I:I)=' '
         CDFTAG(I:I)=' '
         CFCTAG(I:I)=' '
      ENDDO
      CZBTYP='XXXX'
      CFNAME(1:13)='DUMMYFILENAME'
      CGNAME(1:16)='DUMMYGENERICNAME'
      CTAPE='XXXXXX XXXXXX 000000'
      CFCCOM='/* This is a dummy comment */'
C
C  Call initialization normally carried out by D0DAD_FOPEN when used in
C  frameworks
C
      CALL D0DAD_GZBINIT
C
C  Initialize general I/O unit for d0dad
C
      CALL D0DAD_GTUNIT(IDADOK,IKEY,IERR)
      IF( IERR.NE.0 ) THEN
         IF( LDDBG.GT.0 ) WRITE(*,1001) IERR
 1001    FORMAT(' D0DAD_INIT: Error ',I5,' getting generic input unit.')
         IERR = -1
         RETURN
      ENDIF
C
C  Parse command line and determine operating mode...
C
      CALL D0DAD_CPARSE(CMDLIN,IDOPT)
C
 999  CONTINUE
      IERR=0
      RETURN
      END
