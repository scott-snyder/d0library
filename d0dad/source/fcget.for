      SUBROUTINE FCGET(ILUN,IFILE,FNAME,GNAME,TAPE,COMMENT,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get a data record from an unsorted d0dad
C-      event catalog.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   8-NOV-1993   John D Hobbs
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:d0dadcom.inc/NOLIST'
      INTEGER  ILUN,IFILE,IERR,IMAX,LENOCC
      EXTERNAL LENOCC
      CHARACTER*(*) FNAME,GNAME,TAPE,COMMENT
C----------------------------------------------------------------------C
C
      CALL FCLSET(ILUN,IERR)
      IF( IERR.NE.0 ) THEN
        IERR = -4
        GOTO 999
      ENDIF
C
      READ(ILUN,REC=IFILE+1,ERR=998) CLINE
      FNAME=CLINE(1:NFCFN)
      GNAME=CLINE(NFCFN+1:NFCFN+NFCGN)
      TAPE=CLINE(NFCFN+NFCGN+1:NFCFN+NFCGN+NFCTAP)
      COMMENT=CLINE(NFCFN+NFCGN+NFCTAP+1:JNCHFC-4)
C
CJDH      IMAX=MIN(LEN(FNAME),LENOCC(CFNAME))
CJDH      FNAME=CFNAME(1:IMAX)
CJDH      IMAX=MIN(LEN(GNAME),LENOCC(CGNAME))
CJDH      GNAME=CGNAME(1:IMAX)
CJDH      IMAX=MIN(LEN(TAPE),LENOCC(CTAPE))
CJDH      TAPE=CTAPE(1:IMAX)
CJDH      IMAX=MIN(LEN(COMMENT),LENOCC(CFCCOM))
CJDH      COMMENT=CFCCOM(1:IMAX)
C
 999  CONTINUE
      IERR=0
      RETURN
C
 998  CONTINUE
      WRITE(*,*) ' FCGET: Error reading record',IFILE+1,' from FC'
      IERR = -1
      RETURN
      END
