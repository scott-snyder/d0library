      SUBROUTINE PFLABL(PF1,PF2,PF3,PF4)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Put PF-key labels on bottom line of display
C-
C-   Inputs  : PF1..PF4: Labels for the four PF-keys respectively
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PF1,PF2,PF3,PF4
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER ISTAT,LIBCUR,K,LIBBIG,I,J,LIBGET,TRULEN,LABLEN
      CHARACTER*16 LABL
      CHARACTER BLNK*16
      DATA BLNK/' '/
C----------------------------------------------------------------------
      IF(FULSCR) THEN
        LABLEN=PBCOLS/8-2
        ISTAT=LIBGET(I,J)
        K=MAX(LABLEN/2-TRULEN(PF1)/2,0)
        LABL=BLNK(1:K)//PF1//'        '
        ISTAT=LIBBIG(LABL(1:LABLEN),PBROWS,1,2)
        K=MAX(LABLEN/2-TRULEN(PF2)/2,0)
        LABL=BLNK(1:K)//PF2//'        '
        ISTAT=LIBBIG (LABL(1:LABLEN),PBROWS,(LABLEN+2)*2+2,2)
        K=MAX(LABLEN/2-TRULEN(PF3)/2,0)
        LABL=BLNK(1:K)//PF3//'        '
        ISTAT=LIBBIG (LABL(1:LABLEN),PBROWS,(LABLEN+2)*4+2,2)
        K=MAX(LABLEN/2-TRULEN(PF4)/2,0)
        LABL=BLNK(1:K)//PF4//'        '
        ISTAT=LIBBIG (LABL(1:LABLEN),PBROWS,(LABLEN+2)*6+2,2)
        ISTAT=LIBCUR(I,J)
      ENDIF
      RETURN
      END
