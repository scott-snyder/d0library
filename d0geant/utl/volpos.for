      SUBROUTINE VOLPOS(NMSRCP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Does a GSVOLU followed by a GSPOS on
C-                         Parameters obtained from SRCP
C-
C-   Inputs  : NMSRCP  SRCP Character Ident.
C-   Outputs   SRCPAR array containing the parameters of the
C-             Volume in question. In SRCPR.INC
C-   Controls: None
C-
C-   Created   4-OCT-1988   Rajendran Raja
C-   Updated  27-JUL-1989   Stuart Fuess        Add 'NONE' option to
C-                                              skip positioning
C-   Updated  09-MAR-1992   S. Abachi   Used SD0(3) to modify for creation
C-                                      of STP file MURECO_GSAVE.DAT.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:SRCPR.INC'
      INCLUDE 'D0$INC:D0LOG.INC'
C
      CHARACTER*(*) NMSRCP
      CHARACTER*32 NMSRC1
      INTEGER IVOLU,IERR
      CHARACTER*4 POSTYP
      CHARACTER*4  NAMES, MOTHERS, SHAPES

      real zero(1)
      data zero / 0 /
C----------------------------------------------------------------------
      CALL EZGET(NMSRCP,SRCPAR(1),IERR)
      ENTRY VOLPOS1                     ! For zebra direct VOLPOS
C !
C ! VOLUME DESCRIPTOR SRCPARS are formatted as follows
C ! SRCPAR(1) = volume name
C ! SRCPAR(2) = Volume shape
C ! SRCPAR(3) = Medium number
C ! SRCPAR(4) = Mother volume from which this volume hangs
C ! SRCPAR(5) = Type of positioning (POS,POSP DVN etc)
C !    POS   = Do GSVOLU (if ICOPY = 1)followed by GSPOS
C !    POSP  = Do GSVOLU (if ICOPY = 1)followed by GSPOSP
C !    DVN   = Do GSDVN
C !    DVN2  = Do GSDVN2
C !    DVT   = Do GSDVT
C !    DVT2  = Do GSDVT2
C !
C ! IF( SRCPAR(5) is DVN,DVN2,DVT,DVT2)GO TO DIVISIONS FOR FORMAT INFO.
C !
C ! SRCPAR(6) = Rotation matrix
C ! SRCPAR(7) = Copy number of the volume
C ! SRCPAR(8) = x of positioning
C ! SRCPAR(9) = y of positioning
C ! SRCPAR(10)= z of positioning
C ! SRCPAR(11)= Number of parameters describing this volume
C ! SRCPAR(12..)=volume parameters
C !
C ! FOR DIVISIONS, then format is as follows.
C !
C ! SRCPAR(6) = Number of divisions (NDIV). Applicable for DVN,DVN2,DVX
C ! SRCPAR(7) = AXIS of division. (IAXIS)
C ! SRCPAR(8) = Origin for the divisions (C0) . Applicable for DVN2,DVX,DVT2
C ! SRCPAR(9) = STEP for divisions. applicable for DVX,DVT,DVT2
C ! SRCPAR(10)= Expected maximum number of divisions (NDVMX). DVX,DVT,DVT2
C !
C

      CALL UHTOC(NPSTYP,4,POSTYP,4) !Convert to characters
C
      CALL UHTOC(NAME,4,NAMES,4)
      CALL UHTOC(MOTHER,4,MOTHERS,4)
      CALL UHTOC(SHAPE,4,SHAPES,4)

      IF ( POSTYP.EQ.'POS ' ) THEN !GSPOS
        IF(ICOPY.EQ.1)CALL GSVOLU(NAMES,SHAPES,NUMED,PAR,NPAR,IVOLU)
        CALL GSPOS(NAMES,ICOPY,MOTHERS,X,Y,Z,IROT,'ONLY')
      ELSEIF ( POSTYP.EQ.'POSP' ) THEN !Only GSPOSP
        IF(ICOPY.EQ.1)CALL GSVOLU(NAMES,SHAPES,NUMED,zero,0,IVOLU)
        CALL GSPOSP(NAMES,ICOPY,MOTHERS,X,Y,Z,IROT,'ONLY',PAR,NPAR)
      ELSEIF ( POSTYP.EQ.'NONE' ) THEN !No positioning
        IF(ICOPY.EQ.1)CALL GSVOLU(NAMES,SHAPES,NUMED,PAR,NPAR,IVOLU)
C
C -  If the d0geant utility parameter SD0(3) is 1, then divisions are not
C -  made. This option is only to be used for creation of special STP file
C -  mureco_gsave.dat for mureco package.
C
      ELSEIF ( SD0(3) .EQ. 1.0 ) THEN
        GOTO 10
      ELSEIF ( POSTYP.EQ.'DVN ' ) THEN
        CALL GSDVN(NAMES,MOTHERS,NDIV,IAXIS)
      ELSEIF ( POSTYP.EQ.'DVN2' ) THEN
        CALL GSDVN2(NAMES,MOTHERS,NDIV,IAXIS,C0,NUMED)
      ELSEIF ( POSTYP.EQ.'DVT ' ) THEN
        CALL GSDVT(NAMES,MOTHERS,STEP,IAXIS,NUMED,NDVMX)
      ELSEIF ( POSTYP.EQ.'DVX ' ) THEN
        CALL GSDVX(NAMES,MOTHERS,NDIV,IAXIS,STEP,C0,NUMED,NDVMX)
      ELSEIF ( POSTYP.EQ.'DVT2' ) THEN
        CALL GSDVT2(NAMES,MOTHERS,STEP,IAXIS,C0,NUMED,NDVMX)
      ENDIF
C
   10 CONTINUE
C
  999 RETURN
      END
