      SUBROUTINE FILLST(FILTYP,MAXITM,MAXFIL,ITEMS)
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
C-   Modified 16-AUG-1992   sss - handle unix pathnames
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) ITEMS(*),FILTYP
      INTEGER MAXITM,MAXFIL
      CHARACTER*80 INUSE,INFILE
      INTEGER CONTXT,LIB$FIND_FILE,LIB$FIND_FILE_END,I,K,
     &  ISTAT,TRULEN
      EXTERNAL TRULEN
C----------------------------------------------------------------------
      LOGICAL ODD
      ODD (I) = 2*INT (I/2) .NE. I
C
C        Find the files corresponding to FILTYP
C
      CONTXT=0
      MAXFIL=0
      ISTAT=1
      DO WHILE (MAXFIL.LT.MAXITM)
C&IF VAXVMS
        ISTAT=LIB$FIND_FILE(FILTYP//';',INFILE,CONTXT,,,,)
C&ELSE
C&        ISTAT=LIB$FIND_FILE(FILTYP,INFILE,CONTXT)
C&ENDIF
        IF(.NOT.ODD(ISTAT)) GOTO 2000
C&IF VAXVMS
        INUSE=INFILE(INDEX(INFILE,']')+1:INDEX(INFILE,';'))
        INUSE=INUSE(1:INDEX(INUSE,'.')-1)                  !Strip off extension
C&ELSE
C&        i = 0
C&        k = trulen (infile)
C&        do while (k .gt. 0)
C&          if (infile(k:k) .eq. '.' .and. i.eq.0) then
C&            infile = infile(:k-1)
C&            i = 1
C&          else if (infile(k:k) .eq. '/') then
C&            infile = infile(k+1:)
C&            goto 10
C&          endif
C&          k = k-1
C&        enddo
C& 10     continue
C&        inuse = infile
C&ENDIF
C
C Skip files for control menu when not in such a menu
C
        IF(TRULEN (INUSE) .GT. 0 .AND.
     &     .NOT.(INDEX(FILTYP,'MENUDEF').EQ.0.AND.
     *           INDEX(INUSE,'MENUDEF$').GT.0)) THEN
          K=INDEX(INUSE,'$')
          IF(K.GT.0)INUSE=INUSE(K+1:)
          K=INDEX(INUSE,'$')                 !Find possible second '$'
          IF(K.GT.0)INUSE=INUSE(K+1:)
          MAXFIL=MAXFIL+1
          ITEMS(MAXFIL)=INUSE
        ENDIF
      ENDDO
 2000 CONTINUE
      ISTAT=LIB$FIND_FILE_END(CONTXT)
  999 RETURN
      END
