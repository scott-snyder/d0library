      SUBROUTINE D0HCMP_CRE_PAWDIRS(SUBDIRS,CPREFIX,NELEM,JSORT,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to alphabeticaly sort the PAW subdirectories
C-                         and to create the subdirectory structure
C-
C-   Inputs  : SUBDIRS table with the subdirectory names
C-             CPREFIX the name of the main directory (e.g. //PAWC)
C-             NELEM   number of elements to be sorted
C-   Outputs : JSORT   table of pointers to the sorted array
C-             IER     0 if OK
C-   Controls:
C-   CALLS      HBOOK FATMEN CERNLIB routines plus D0HCMP_GET_SUBDIRS
C-
C-   Created  24-MAR-1992   Krzysztof L. Genser
C-   Updated  31-AUG-1992   Krzysztof L. Genser
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:D0HCMP.DEF/LIST'
      CHARACTER*(*) CPREFIX
      INTEGER NELEM

      CHARACTER*255 MSUBDIRS(MAXDIRN)

      CHARACTER*255 LSUBDIRS(MAXNCHIST)
      CHARACTER*255 SSUBDIRS(MAXDIRL)

      CHARACTER*255 SBUF
      INTEGER LSBUF

      INTEGER KNOWNDIR
      INTEGER NLSUB
      INTEGER I,L,K
      INTEGER IELEM
C----------------------------------------------------------------------
      IER = 0
C
C ****  sort the directories first (a routine from FATLIB)
C
      CALL SORTCH (SUBDIRS,JSORT,NELEM)
C
C ****  leave only unique subdirs
C
      IELEM = 0

      DO 30 I = 1,NELEM

        SBUF = SUBDIRS(JSORT(I))
        LSBUF = LENOCC(SBUF)

        IF ( IELEM.GT.0 ) THEN

          DO 40 L = 1,IELEM
            IF ( SBUF(1:LSBUF) .EQ.
     &         LSUBDIRS(L)(1:LENOCC(LSUBDIRS(L))) ) GOTO 30
   40     CONTINUE

        ENDIF

        IELEM = IELEM + 1
        LSUBDIRS(IELEM)=SBUF

   30 CONTINUE

C      PRINT *, ' '
C      DO 210 I =  1,IELEM
C        PRINT ('(1X,A50,I8,F8.4)'), LSUBDIRS(I)(1:LENOCC(LSUBDIRS(I)))
C  210 CONTINUE
C      PRINT *, ' '

C
C ****  make local tables of know directories and thier levels
C
      KNOWNDIR = 0

      DO 10 I = 1 , IELEM

        CALL D0HCMP_GET_SUBDIRS(LSUBDIRS(I),SSUBDIRS,NLSUB,IER)

        DO 20 L = 1 ,  NLSUB

          SBUF = SSUBDIRS(L)
          LSBUF = LENOCC(SBUF)

          IF ( KNOWNDIR.GT.0 ) THEN

            DO 50 K = 1 , KNOWNDIR
              IF ( SBUF(1:LSBUF) .EQ.
     &               MSUBDIRS(K)(1:LENOCC(MSUBDIRS(K))))
     &               GOTO 20
   50       CONTINUE

          ENDIF

          KNOWNDIR = KNOWNDIR + 1
          MSUBDIRS(KNOWNDIR) = SBUF
C
C ****  create PAW directory
C
          SBUF='//'//CPREFIX//'/'//SBUF(1:LSBUF)
          LSBUF = LENOCC(SBUF)
          CALL HMDIR( SBUF(1:LSBUF),' ')

   20   CONTINUE

   10 CONTINUE

C      DO 110 I =  1,KNOWNDIR
C        PRINT ('(1X,A50,I8,F8.4)'), MSUBDIRS(I)(1:LENOCC(MSUBDIRS(I)))
C  110 CONTINUE

      RETURN

      END
