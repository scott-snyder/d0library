      SUBROUTINE PXFILLST(FILTYP,MAXITM,MAXFIL,ITEMS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return a list of files satisfying wildcard
C-                         name type, VAX-specific
C-
C-   Inputs  : FILTYP : Wild card name to satisfy
C-             MAXITM : Max number of files to look for (dimension of ITEMS)
C-   Outputs : MAXFIL : Number of files found
C-             ITEMS:   List of stripped file names
C-   Controls: None
C-
C-   Created  31-AUG-1988   Jan S. Hoftun
C-   Updated  28-FEB-1990   Lupe Howell Chek that the file names in ITEMS are
C-                                      not repeated twice 
C-   Updated  27-JAN-1992   Lupe Howell   Update for SGI
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) ITEMS(1),FILTYP
      INTEGER MAXITM,MAXFIL,J
C&IF VAXVMS
      CHARACTER*80 INUSE,INFILE,STRING
      INTEGER CONTXT,LIB$FIND_FILE,LIB$FIND_FILE_END,I,K,
     &  ISTAT
C----------------------------------------------------------------------
C
C        Find the files corresponding to FILTYP
C
      CONTXT=0
      MAXFIL=0
      ISTAT=1
      DO WHILE (MAXFIL.LT.MAXITM)
        STRING = FILTYP//';'
        CALL WORD(STRING,I,J,K)
        ISTAT=LIB$FIND_FILE(STRING(1:K),INFILE,CONTXT,,,,)
        IF(.NOT.ISTAT) GOTO 2000
        INUSE=INFILE(INDEX(INFILE,']')+1:INDEX(INFILE,';'))
        INUSE=INUSE(1:INDEX(INUSE,'.')-1)                  !Strip off extension
C
C Skip files for control menu when not in such a menu
C
        IF(.NOT.(INDEX(FILTYP,'MENUDEF').EQ.0.AND.
     *      INDEX(INUSE,'MENUDEF$').GT.0)) THEN
          K=INDEX(INUSE,'$')
          IF(K.GT.0)INUSE=INUSE(K+1:)
          K=INDEX(INUSE,'$')                 !Find possible second '$'
          IF(K.GT.0)INUSE=INUSE(K+1:)
          DO 1000 J=1, MAXFIL                ! Searching for same file 
            IF(ITEMS(J).EQ.INUSE) GO TO 1100 
 1000     CONTINUE
          MAXFIL=MAXFIL+1
          ITEMS(MAXFIL)=INUSE
 1100     CONTINUE
        ENDIF
      ENDDO
 2000 CONTINUE
      ISTAT=LIB$FIND_FILE_END(CONTXT)
C&ELSE
C&      CALL OUTMSG('0Finding list of files not supported here!')
C&      CALL OUTMSG(' ')
C&ENDIF
  999 RETURN
      END
