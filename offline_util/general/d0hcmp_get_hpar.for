      SUBROUTINE D0HCMP_GET_HPAR(FIELDID,SUBDIRS,IDS,CLEV,NIDS,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to get histogram directory, ID and confidence level
C-                         from an rcp file
C-                         it is assumed that the first column is the dir name
C-                         in the form of an character string, the second column
C-                         is the hist ID and the third the confidence level
C-
C-   Inputs  : FIELDID name of RCP array
C-   Outputs : SUBDIRS full name of the hbook subdirectory (below the main one)
C-             IDS     histogram id
C-             CLEV    histogram confidence level
C-             NIDS    number of histogram ids
C-             IER     ERR = 0 if everything is OK
C-   Controls:
C-
C-   Created  23-MAR-1992   Krzysztof L. Genser
C-   Updated  31-AUG-1992   Krzysztof L. Genser
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$PARAMS:D0HCMP.DEF/LIST'
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF/LIST'

      CHARACTER*(*) FIELDID

      INTEGER IHTYPE(MAXNCHIST)
      INTEGER IHVAL(MAXNCHIST)
      REAL    RHVAL(MAXNCHIST)
      EQUIVALENCE ( IHVAL(1),  RHVAL(1) )
      INTEGER IHNVAL
      INTEGER ICOLUMN
      INTEGER I,J1,J2,J3,L

C----------------------------------------------------------------------
      IER = 0

      CALL EZGET_VALUE_TYPE (FIELDID(1:LENOCC(FIELDID)),
     &                        IHVAL,IHTYPE,IHNVAL,IER)
      IF (EZERR(IER)) THEN
        CALL EZGET_ERROR_TEXT(IER,EMSG)
        CALL ERRMSG('D0HCMP','D0HCMP',
     &      EMSG(1:LENOCC(EMSG)),'I')
        CALL ERRMSG('D0HCMP','D0HCMP',
     &      'Can not get histogram information','F')
      ENDIF

      J1 = 0
      J2 = 0
      J3 = 0

      DO 20 I =  1, IHNVAL

        IF ( IHTYPE(I).GT.VTCHR ) THEN

          IF ( I .EQ. 1  ) THEN

            J1 = J1 + 1
            L = IHTYPE(I) - VTCHR                 ! Get string length
            SUBDIRS(J1) = ' '                     ! Initializing it.
            CALL UHTOC(IHVAL(I),4,SUBDIRS(J1),L)

          ELSEIF ( IHTYPE(I-1).LT.VTCHR ) THEN

            J1 = J1 + 1
            L = IHTYPE(I) - VTCHR                 ! Get string length
            SUBDIRS(J1) = ' '                     ! Initializing it.
            CALL UHTOC(IHVAL(I),4,SUBDIRS(J1),L)

          ENDIF

          ICOLUMN = 1

        ELSE

          ICOLUMN = ICOLUMN + 1

        ENDIF

        IF ( ICOLUMN .EQ. 2 ) THEN
C
C ****  histogram ids
C
          J2=J2+1
          IDS(J2) = IHVAL(I)

        ELSEIF ( ICOLUMN.EQ. 3 ) THEN
C
C ****  confidence levels
C
          J3=J3+1
          CLEV(J3) = RHVAL(I)

        ENDIF

   20 CONTINUE

      IF ( J1 .NE. J2 .OR. J1 .NE. J3 .OR. J2 .NE. J3 ) THEN
        CALL ERRMSG('D0HCMP','D0HCMP',
     &      'Histogram information inconsistent' , 'I')
        IER = 1
      ENDIF

      NIDS=J1

      RETURN

      END
