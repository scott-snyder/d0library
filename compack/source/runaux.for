      SUBROUTINE RUNAUX(INDIR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Let user select and run another program in 
C-                         a subprocess. Will really only work under 
C-                         VAX/VMS currently.
C-
C-   Inputs  : INDIR: Directory to look for EXE files in
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) INDIR
C
C     Get definitions for COMPACK
C
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
C
      INTEGER MAXITM
      PARAMETER (MAXITM=50)
      CHARACTER*40 TOPS,OUTDIR
      CHARACTER*32 ITEMS(1:MAXITM)
      INTEGER ISTAT,LSPAWN,LIBERA,TRALOG
      INTEGER POSI,I
C----------------------------------------------------------------------
C&IF VAXVMS
      ISTAT=TRALOG(INDIR(1:LEN(INDIR)-1),OUTDIR,I)
      IF(ISTAT.EQ.1) THEN
         TOPS='  Programs in '//OUTDIR(1:I)
      ELSE
         TOPS='  Programs in '//INDIR
         OUTDIR=INDIR
      ENDIF
C
C        Find the files available to be run from directory INDIR
C
100   CONTINUE
      IF(COMNUM.GT.0) THEN
         PF=1
         POSI=1
         ITEMS(1)=COMPRT(1)
      ELSE
         CALL FNDFIL(POSI,MAXITM,OUTDIR//'*.EXE',TOPS,ITEMS)
         IF(PF.EQ.0) THEN
            GOTO 100
         ENDIF
      ENDIF
      GOTO (1,2,3,4),PF
1     CONTINUE
      IF(SETUP) THEN
         IF(COMNUM.LT.1) WRITE(COMUNI,101) ITEMS(POSI)
101      FORMAT(A30)
      ELSE
         ISTAT=LIBERA(1,1)
         ISTAT=LSPAWN('RUN '//INDIR(1:LEN(INDIR))//ITEMS(POSI),' ')
         CALL PFWAIT
      ENDIF
      PF=0
      GOTO 5
2     CONTINUE
      ISTAT=LIBERA(1,1)
      ISTAT=LSPAWN('HELP '//ITEMS(POSI),' ')
      CALL PFWAIT
      PF=0                     
      GOTO 100
3     PF=0 
      GOTO 100
4     CONTINUE
5     CONTINUE
C&ELSE
C&      CALL OUTMSG('0NOT implemented on this machine!')
C&ENDIF
      RETURN
      END
