      SUBROUTINE JBEGIN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
CD   Subroutine JBEGIN initializes internal variables to predefined values.
CD   The values cannot be intialized in a common block since this routine
CD   can be called several times from a single program.
CD   There are NO parameters to this subroutine.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  21-AUG-1990   A. Virgo
C-   UPDATED  19-JUNE-1990  S. ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GENRL.INC'
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:DEVSTS.INC'
      INCLUDE 'D0$INC:SEGINF.INC'
      INCLUDE 'D0$INC:PRIMVR.INC'
      INCLUDE 'D0$INC:LINATT.INC'
      INCLUDE 'D0$INC:TEXATT.INC'
      INCLUDE 'D0$INC:PLGATT.INC'
      INCLUDE 'D0$INC:NEWDI3.INC/LIST'
      INTEGER I
      LOGICAL FIRST
      DATA    FIRST /.TRUE./
C----------------------------------------------------------------------
C-
      IREND = 0             ! was set in NEWDI3.INC
      NUDI3 = .FALSE.       ! was set in NEWDI3.INC
      NOROT = .FALSE.       ! was set in NEWDI3.INC
      FMAT  = .FALSE.       ! was set in NEWDI3.INC
      EAS_CONN = .FALSE.    ! was set in GRFPAR.INC
C-
      IF (BEGIN) THEN
        CALL ERROR('DI-3000 EMULATOR ALREADY INITIALIZED')
      ENDIF
C
C    INITIALIZE NEWDI3 BLOCK FOR NEW STRACTURE
C
      IJKSG = 0
CC      NUDI3 = .FALSE.
      IREND = 0
C
C    INITIALIZE GENRL COMMON BLOCK
C
      BEGIN = .TRUE.
      EMVERS = 0.2
      DSPDEV = 0
C
C    INITIALIZE GRFPAR COMMON BLOCK
C
      DINIT  = .FALSE.
      EMDISP = 'FRAME"'
C-
C--- KEEP OLD VALUES AFTER THE SECOND CALL
C-
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        DO 5 I=1,3
          VUPNT(I) = 0.0
          NORML(I) = 0.0
          UPVEC(I) = 0.0
          SCV(I) = 1.0
          TRV(I) = 0.0
    5   CONTINUE
        NORML(3) = 1.0
        UPVEC(2) = 1.0
        WCLIP = .FALSE.
        HCLIP = .FALSE.
        YCLIP = .FALSE.
        RIGHT = 1.0
        KPTRN = 0
        MODEL = .FALSE.
        CALL KMTID(MODMAT)
        DO 7 I=1,MMAT
          NTRN(I) = 0
    7   CONTINUE
        NMAT = 0
        PRJTYP = PTPARA
      ENDIF
C
C    INITIALIZE DEVSTS COMMON BLOCK
C
      DEVON = .FALSE.
C
C    INITIALIZE SEGINF COMMON BLOCK
C
      NSEGS  = 0
      NPRIM  = 0
      SEGOPN = .FALSE.
      SEGNUM = -1
      SEGNAM = ' '
      SEGSOP = .FALSE.
      NTSEG  = 0
      NRSEG  = 0
      CTTYPE = 0
      DVISIB = 1
      DHILIT = 1
      DSGPRI = 0
      DDETEC = 0
      DPIKID = 0
C
C    INITIALIZE PRIMVR COMMON BLOCK
C
      CPX = 0.0
      CPY = 0.0
      CPZ = 0.0
C
C    INITIALIZE LINATT COMMON BLOCK
C
      CURCOL = 0
      DEFCOL = 0
      CPIDCO = 0
      DPIDCO = 0
      MINTEN = 500
      CINTEN = 32767
      DINTEN = 32767
      UVIEW(1) = -1.0
      UVIEW(2) = 1.0
      UVIEW(3) = -1.0
      UVIEW(4) = 1.0
      UVIEW(5) = MINTEN / 32767.0
      UVIEW(6) = DINTEN / 32767.0
      UWIND(1) = -1.001
      UWIND(2) = 1.001
      UWIND(3) = -1.001
      UWIND(4) = 1.001
      UWIND(5) = -1.001
      UWIND(6) = 1.001
      CLSTYL = 0
      DLSTYL = 0
      CMARKR = 1
      DMARKR = 1
      THREED = .FALSE.
C
C    INIT COLORS
C
      HUECOL( 1,1) =   0.0
      HUECOL( 2,1) = 120.0
      HUECOL( 3,1) = 240.0
      HUECOL( 4,1) = 180.0
      HUECOL( 5,1) =   0.0
      HUECOL( 6,1) =  60.0
      HUECOL( 7,1) = 300.0
      HUECOL( 8,1) =   0.0
      HUECOL( 9,1) =   0.0
      HUECOL(10,1) =   0.0
      HUECOL(11,1) = 150.0
      HUECOL(12,1) = 270.0
      HUECOL(13,1) = 210.0
      HUECOL(14,1) =  30.0
      HUECOL(15,1) =  90.0
      HUECOL(16,1) = 330.0
      HUECOL(17,1) = 120.0
      HUECOL(18,1) = 240.0
      HUECOL(19,1) = 180.0
      HUECOL(20,1) =   0.0
      HUECOL(21,1) =  60.0
      HUECOL(22,1) = 300.0
      HUECOL(23,1) = 150.0
      HUECOL(24,1) = 270.0
      HUECOL(25,1) = 210.0
      HUECOL(26,1) =  30.0
      HUECOL(27,1) =  90.0
      HUECOL(28,1) = 330.0
      HUECOL(29,1) = 120.0
      HUECOL(30,1) = 240.0
      HUECOL(31,1) = 180.0
      HUECOL(32,1) =   0.0
      HUECOL(33,1) =  60.0
      HUECOL(34,1) = 300.0
      HUECOL(35,1) = 150.0
      HUECOL(36,1) = 270.0
      HUECOL(37,1) = 210.0
      HUECOL(38,1) =  30.0
      HUECOL(39,1) =  90.0
      HUECOL(40,1) = 330.0
C
      SATCOL( 1,1) = 0.0
      SATCOL( 2,1) = 1.0
      SATCOL( 3,1) = 1.0
      SATCOL( 4,1) = 1.0
      SATCOL( 5,1) = 1.0
      SATCOL( 6,1) = 1.0
      SATCOL( 7,1) = 1.0
      SATCOL( 8,1) = 0.0
      SATCOL( 9,1) = 0.40
      SATCOL(10,1) = 0.40
      SATCOL(11,1) = 1.0
      SATCOL(12,1) = 1.0
      SATCOL(13,1) = 1.0
      SATCOL(14,1) = 1.0
      SATCOL(15,1) = 1.0
      SATCOL(16,1) = 1.0
      SATCOL(17,1) = 0.80
      SATCOL(18,1) = 0.80
      SATCOL(19,1) = 0.80
      SATCOL(20,1) = 0.80
      SATCOL(21,1) = 0.80
      SATCOL(22,1) = 0.80
      SATCOL(23,1) = 0.80
      SATCOL(24,1) = 0.80
      SATCOL(25,1) = 0.80
      SATCOL(26,1) = 0.80
      SATCOL(27,1) = 0.80
      SATCOL(28,1) = 0.80
      SATCOL(29,1) = 0.80
      SATCOL(30,1) = 0.60
      SATCOL(31,1) = 0.60
      SATCOL(32,1) = 0.60
      SATCOL(33,1) = 0.60
      SATCOL(34,1) = 0.60
      SATCOL(35,1) = 0.60
      SATCOL(36,1) = 0.60
      SATCOL(37,1) = 0.60
      SATCOL(38,1) = 0.60
      SATCOL(39,1) = 0.60
      SATCOL(40,1) = 0.60

C
C    SET THE DEFAULT COLORS
C
      DO 10 I=1,40
        HUECOL(I,2)  = HUECOL(I,1)
        SATCOL(I,2)  = SATCOL(I,1)
   10 CONTINUE
C
C    Initialize TEXATT common block
C
      DPATH  = 1
      DHJUST = 1
      DVJUST = 1
      DXSIZE = 0.02
      DYSIZE = 0.02
      DGAP   = 0.0
      DXBASE = 1.0
      DYBASE = 0.0
      DZBASE = 0.0
      DXPLAN = 0.0
      DYPLAN = 1.0
      DZPLAN = 0.0
C
C    Initialize PLGATT common block
C
      DPIDCO = 0
      DPIDIN = 0
      DPINTR = 0
      DPEDGE = 0
      PFSIM  = 1
C
      CALL KINIC
C
      RETURN
      END
