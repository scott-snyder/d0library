      SUBROUTINE FILENAME_PARSE(INNAME,CFIELD,FOUT,LFOUT)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Parse the filename given in FNAME according
C-    the fields requested in CFIELD.  This routine is roughly system
C-    independent.  It is a replacement for D0DAD_FNPARSE.
C-
C-   Inputs  : INNAME  - The filename to parse
C-             CFIELD  - The field descriptor (see above)
C-   Outputs : FOUT    - The output (parsed) filename
C-             LFOUT   - The length of FOUT up to first blank. If<0,
C-                 an error occurred.
C-   Controls:
C-
C-   Created  10-Jan-1995   John D. Hobbs
C-
C-   =================================================================
C-
C-    VMS filenames are of the form:
C-  
C-             NODE::DEVICE:[DIRS]NAME.TYPE;VERSION
C-
C-    Unix filenames are of the form:
C-
C-             (user@)NODE:/DIRS/NAME.TYPE
C-
C-    The desired field(s) are passed in CFIELD with the following
C-    convention (3 characters/field requested):
C-
C-            NOD = NODE NAME
C-            DEV = DEVICE NAME
C-            DIR = DIRECTORY
C-            NAM = NAME
C-            EXT = TYPE
C-            VER = VERSION
C-
C-    Thus, the descriptor to retrieve the file name is CFIELD='NAM'.
C-    A more complete parse is provided if (eg) CFIELD='NAM+EXT' which
C-    would (e.g.) return both the name and type.
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) INNAME,CFIELD,FOUT
      INTEGER LFOUT
C
      CHARACTER*132 CTFLD,FNAME
      INTEGER INODE,IDEV,IDIR,INAME,IEXT,IVERS
      INTEGER ICNOD,ICDEV,ICDIR(2),ICNAM,ICEXT,ICVER
      INTEGER I,ILEN,ISTART,ILAST
      CHARACTER NODSEP*(*),DEVSEP*(*),DIRSEP1*1,DIRSEP2*1,EXTSEP*1
      CHARACTER VERSEP*(*)
      INTEGER LENOCC,ICFILA,ICFIND,ICLOC,ICFNBL
C&IF VAXVMS      
      PARAMETER(NODSEP='::')
      PARAMETER(DEVSEP=':')
      PARAMETER(DIRSEP1='[',DIRSEP2=']')
      PARAMETER(EXTSEP='.')
      PARAMETER(VERSEP=';')
C&ELSE
C&      PARAMETER(NODSEP=':')
C&      PARAMETER(DEVSEP=' ')
C&      PARAMETER(DIRSEP1='/',DIRSEP2='/')
C&      PARAMETER(EXTSEP='.')
C&      PARAMETER(VERSEP=' ')
C&ENDIF
C-----------------------------------------------------------------------
C
      IF( LENOCC(INNAME).GT.LEN(FNAME) ) THEN
        LFOUT = -1
        GOTO 999
      ENDIF
C
      ISTART=ICFNBL(INNAME,1,LENOCC(INNAME))
      ILAST=LENOCC(INNAME)
      FNAME=INNAME(ISTART:ILAST)
      ISTART=ICFNBL(CFIELD,1,LENOCC(CFIELD))
      CTFLD=CFIELD(ISTART:LENOCC(CFIELD))
      CALL CLTOU(CTFLD)
C
      INODE=0
      IDEV=0
      IDIR=0
      INAME=0
      IEXT=0
      IVERS=0
      DO I=1,LENOCC(CTFLD)-2,4
C
        IF( (I+3).LE.LENOCC(CTFLD) .AND. CTFLD(I+3:I+3).NE.'+' ) THEN
          LFOUT = -2
          GOTO 999
C
        ENDIF
        IF( CTFLD(I:I+2).EQ.'NOD' ) INODE=1
C&IF VAXVMS
        IF( CTFLD(I:I+2).EQ.'DEV' ) IDEV=1
C&ENDIF
        IF( CTFLD(I:I+2).EQ.'DIR' ) IDIR=1
        IF( CTFLD(I:I+2).EQ.'NAM' ) INAME=1
        IF( CTFLD(I:I+2).EQ.'EXT' ) IEXT=1
C&IF VAXVMS
        IF( CTFLD(I:I+2).EQ.'VER' ) IVERS=1
C&ENDIF
      ENDDO
C
C  Get positions of field separators in FNAME. The positions of missing
C  fields are set to 0.
C
C  --> Node name
      ICNOD=ICLOC(NODSEP,LEN(NODSEP),FNAME,1,LENOCC(FNAME))
C&IF VAXVMS
C  --> Device name (VAX only)
      ISTART=ICNOD
      IF( ISTART.NE.0 ) ISTART=ISTART+LEN(NODSEP)
      ICDEV=ICFIND(DEVSEP,FNAME,MAX(ISTART,1),LENOCC(FNAME))
      IF( ICDEV.GT.LENOCC(FNAME) ) ICDEV=0
C&ELSE
C&      ICDEV=0
C&ENDIF
C  --> Directory name
      ICDIR(1)=ICFIND(DIRSEP1,FNAME,MAX(ICNOD,ICDEV,1),LENOCC(FNAME))
      ICDIR(2)=ICFILA(DIRSEP2,FNAME,MAX(ICNOD,ICDEV,1),LENOCC(FNAME))
C&IF VAXVMS
C&ELSE
C&      IF(ICDIR(1).NE.(ICNOD+LEN(NODSEP))) ICDIR(1)=ICNOD+LEN(NODSEP)
C&ENDIF
      IF( ICDIR(2).GT.LENOCC(FNAME) ) ICDIR(2)=0
C  --> Name field (delimited by end of directory and start of extension)
C  --> Extension field
      ISTART=MAX(ICNOD,ICDEV,ICDIR(2),1)
      ICEXT=ICFILA(EXTSEP,FNAME,ISTART,LENOCC(FNAME))
C&IF VAXVMS
C  --> Version field
      ISTART=MAX(ICNOD,ICDEV,ICDIR(2),ICEXT,1)
      ICVER=ICFILA(VERSEP,FNAME,ISTART,LENOCC(FNAME))
C&ELSE
C&      ICVER=LENOCC(FNAME)+1
C&ENDIF
C
C  Parse requested fields
C
      LFOUT=0
      FOUT=' '
      IF( INODE.EQ.1 .AND. ICNOD.GT.0 ) THEN
        ILAST=ICNOD+LEN(NODSEP)-1
        FOUT=FNAME(1:ILAST)
        LFOUT=LENOCC(FOUT)
      ENDIF
C&IF VAXVMS
      IF( IDEV.EQ.1 .AND. ICDEV.GT.0 ) THEN
        ISTART=0
        IF( ICNOD.GT.0 ) ISTART=ICNOD+LEN(NODSEP)
        FOUT=FOUT(1:LFOUT)//FNAME(MAX(ISTART,1):ICDEV)
        LFOUT=LENOCC(FOUT)
      ENDIF
C&ENDIF
      IF( IDIR.EQ.1 .AND. ICDIR(1).GT.0 .AND. ICDIR(2).GT.0 ) THEN
        FOUT=FOUT(1:LFOUT)//FNAME(ICDIR(1):ICDIR(2))
        LFOUT=LENOCC(FOUT)
      ENDIF
      IF( INAME.EQ.1 ) THEN
        ISTART=ICNOD
        IF( ICNOD.GT.0 ) ISTART=ICNOD+LEN(NODSEP)-1
        ISTART=MAX(ISTART,ICDEV,ICDIR(2))+1
        FOUT=FOUT(1:LFOUT)//FNAME(ISTART:ICEXT-1)
        LFOUT=LENOCC(FOUT)
      ENDIF
      IF( IEXT.EQ.1 .AND. ICEXT.LE.LENOCC(FNAME) ) THEN
         FOUT=FOUT(1:LFOUT)//FNAME(ICEXT:ICVER-1)
         LFOUT=LENOCC(FOUT)
      ENDIF
C&IF VAXVMS
      IF( IVERS.EQ.1 .AND. ICVER.LE.LENOCC(FNAME) ) THEN
        FOUT=FOUT(1:LFOUT)//FNAME(ICVER:LENOCC(FNAME))
        LFOUT=LENOCC(FOUT)
      ENDIF
C&ENDIF
C
 999  CONTINUE
      RETURN
      END

