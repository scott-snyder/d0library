      SUBROUTINE FETCH_ELEMENTS
     &(LUNOUT,FLAG,LISMAX,CMS_LIBRARY,NLIB,REFERENCE,OLDCLASS,CLASS,
     &CMS_ELEMENT,CMS_GENERATION,NELEM,ERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fetch elements which are newer than the
C-                         specified reference file from the given
C-                         list of CMS libraries. Or, if OLDCLASS is
C-                         not blank fetch elements relative to that
C-                         class.
C-
C-   Inputs  : LUNOUT           [I]     If > 0 print listing.
C-             FLAG             [*]     ' ', 'CLASS', 'ONLY'
C-             LISMAX           [I]     Maximum number of elements
C-             CMS_LIBRARY(*)   [C*]    List of CMS libraries
C-             NLIB             [I]     Number of libraries
C-             REFERENCE        [C*]    Name of reference file
C-             OLDCLASS         [C*]    Old-CMS-class if non-blank.
C-             CLASS            [C*]    CMS-class if non-blank.
C-             CMS_ELEMENT(*)   [C*]    Element file spec
C-             CMS_GENERATION(*)[I]     Not used
C-             NELEM            [I]     Number of elements
C-
C-   Outputs : CMS_ELEMENT(*)   [C*]    Elements fetched
C-             CMS_GENERATION(*)[I]     Generation numbers
C-             NELEM            [I]     Number of elements fetched
C-             ERROR            [I]     0 -- OK
C-
C-   Controls:
C-
C-   Created   8-JUL-1989   Harrison B. Prosper
C-   Updated   6-DEC-1989   Harrison B. Prosper
C-      Added OLDCLASS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER       LUNOUT
      CHARACTER*(*) FLAG
      INTEGER       LISMAX
      CHARACTER*(*) CMS_LIBRARY(*)
      INTEGER       NLIB
      CHARACTER*(*) REFERENCE
      CHARACTER*(*) OLDCLASS
      CHARACTER*(*) CLASS
      CHARACTER*(*) CMS_ELEMENT(*)
      INTEGER       CMS_GENERATION(*)
      INTEGER       NELEM
      INTEGER       ERROR

      INTEGER STATUS
      INTEGER CMS$SET_LIBRARY
      INTEGER CMS$SHOW_GENERATION
      INTEGER CMS$FETCH
      INTEGER LDB(50)
      INTEGER SYS$BINTIM
      INTEGER SYS$ASCTIM

      INTEGER MAXLIST
      PARAMETER( MAXLIST = 511 )
      INTEGER LENLIST,LENELEM
      CHARACTER*511 LIBRARY_LIST,ELEMENT_LIST

      INTEGER NOHISTORY
      PARAMETER( NOHISTORY = 1 )        ! No history should be appended

      CHARACTER*23 DATETIME
      INTEGER ELEMENT_TIME(2),REFERENCE_TIME(2),DIFFERENCE(2)
      LOGICAL NEWER,FETCH,FOUND,USE_TIME_STAMP
      INTEGER LSPEC,II,JJ,KK,I,J,K,L,N,M,LCLASS,LOLDCLASS,NUMELEM
      CHARACTER*80 STRING

      INTEGER NUMBER,OFFSET
      INTEGER MAXELEM
      PARAMETER( MAXELEM = 2000 )
      CHARACTER*80 ELEMENT(0:2*MAXELEM)
      CHARACTER*20 GENERATION(0:2*MAXELEM)
      INTEGER    DATE_TIME(2,0:2*MAXELEM)
      COMMON / ELEMENT_NAME_LIST / OFFSET,NUMBER, ELEMENT, DATE_TIME,
     &  GENERATION

      EXTERNAL OUTPUT_ROUTINE
C----------------------------------------------------------------------
      USE_TIME_STAMP = OLDCLASS(1:1) .EQ. ' '
      FETCH = FLAG(1:1) .EQ. ' '
      ERROR = 0
C
C ****  Build up CMS element list
C
      CALL WORD (CMS_ELEMENT(1),I,J,L)
      ELEMENT_LIST = CMS_ELEMENT(1)(1:L)
      LENELEM = L

      IF ( NELEM .GT. 1 ) THEN

        DO II = 2, NELEM

          CALL WORD (CMS_ELEMENT(II),I,J,L)
          JJ = LENELEM + 1 + L

          IF ( JJ .LE. MAXLIST ) THEN
            ELEMENT_LIST = ELEMENT_LIST(1:LENELEM)//','//
     &      CMS_ELEMENT(II)(1:L)
            LENELEM = JJ
          ELSE
            CALL ERRMSG ('USERLIB','FETCH_ELEMENTS',
     &        'CMS element list string is too long','W')
          ENDIF
        ENDDO
      ENDIF
C
C ****  Build up CMS library search-list string
C
      CALL WORD (CMS_LIBRARY(1),I,J,L)
      LIBRARY_LIST = CMS_LIBRARY(1)(1:L)
      LENLIST = L

      IF ( NLIB .GT. 1 ) THEN

        DO II = 2, NLIB

          CALL WORD (CMS_LIBRARY(II),I,J,L)
          JJ = LENLIST + 1 + L

          IF ( JJ .LE. MAXLIST ) THEN
            LIBRARY_LIST = LIBRARY_LIST(1:LENLIST)//','//
     &      CMS_LIBRARY(II)(1:L)
            LENLIST = JJ
          ELSE
            CALL ERRMSG ('USERLIB','FETCH_ELEMENTS',
     &        'CMS library search-list string is too long','W')
          ENDIF
        ENDDO
      ENDIF
C
C ****  Set CMS library
C
      STATUS = CMS$SET_LIBRARY (LDB, LIBRARY_LIST(1:LENLIST))
      IF ( .NOT. STATUS ) THEN
        CALL LIB$SIGNAL (%VAL(STATUS))
      ELSE
C
C ****  Get generation info
C
        CALL WORD (CLASS,I,J,LCLASS)
        OFFSET = 0
        NUMBER = 0
        IF ( LCLASS .GT. 0 ) THEN
          STATUS = CMS$SHOW_GENERATION (LDB,
     &                                  OUTPUT_ROUTINE,
     &                                  ,
     &                                  ELEMENT_LIST(1:LENELEM),
     &                                  CLASS(1:LCLASS))
        ELSE
          STATUS = CMS$SHOW_GENERATION (LDB,
     &                                  OUTPUT_ROUTINE,
     &                                  ,
     &                                  ELEMENT_LIST(1:LENELEM))
        ENDIF

        IF ( STATUS .AND. NUMBER .GT. 0 ) THEN
          NUMELEM = NUMBER              ! Number of elements

          IF ( USE_TIME_STAMP ) THEN
C
C ****  Get revision time of reference file
C
            CALL WORD(REFERENCE,I,J,L)
            CALL GET_REVISION_DATE (REFERENCE(1:L),DATETIME)
            STATUS = SYS$BINTIM (DATETIME, REFERENCE_TIME)
            IF ( .NOT. STATUS ) CALL LIB$SIGNAL (%VAL(STATUS))
          ELSE
C
C ****  Get generation info for OLD class
C
            CALL WORD (OLDCLASS,I,J,LOLDCLASS)
            CALL WORD (ELEMENT(JJ),I,J,L)
            OFFSET = MAXELEM
            NUMBER = 0
            STATUS = CMS$SHOW_GENERATION (LDB,
     &                                    OUTPUT_ROUTINE,
     &                                    ,
     &                                    ELEMENT_LIST(1:LENELEM),
     &                                    OLDCLASS(1:LOLDCLASS))
          ENDIF

          NELEM = 0                     ! Zero fetched element counter
          FOUND = .TRUE.
          DO JJ =  1, NUMELEM

            ELEMENT_TIME(1) = DATE_TIME(1,JJ)
            ELEMENT_TIME(2) = DATE_TIME(2,JJ)

            IF ( USE_TIME_STAMP ) THEN
C
C ****  Subtract element revision date from that of reference file
C
              CALL LIB$SUBX (REFERENCE_TIME, ELEMENT_TIME, DIFFERENCE)
              NEWER = BTEST (DIFFERENCE(2),31)  ! Test for negative
            ELSE
C
C ****  Determine if ELEMENT(JJ) is NEWER than its partner in the
C ****  OLD class. First check if ELEMENT(JJ) is in the OLD class.
C
              CALL LOCSTR (ELEMENT(JJ),
     &                     ELEMENT(OFFSET+1),
     &                     NUMBER,
     &                     FOUND,
     &                     KK)
              IF ( FOUND ) THEN
                NEWER = GENERATION(JJ) .GT. GENERATION(OFFSET+KK)
              ELSE
                NEWER = .TRUE.              ! Element not in OLD class
              ENDIF
            ENDIF
C
C ****  Fetch only those elements newer than reference file
C ****  or corresponding element in the old class.
C
            IF ( NEWER ) THEN

              IF ( LUNOUT .GT. 0 ) THEN
                DATETIME = ' '
                STATUS = SYS$ASCTIM (,DATETIME,ELEMENT_TIME,)
                IF ( .NOT. STATUS ) CALL LIB$SIGNAL (%VAL(STATUS))

                IF ( FETCH .AND. LUNOUT .EQ. 6 )
     &            WRITE(LUNOUT,FMT='(''    '')')

                IF ( FOUND ) THEN       ! Element in OLD class
                  WRITE(LUNOUT,FMT=
     &            '(''    Fetch: '',A40,''/GEN='',A4,''/'',A20)')
     &            ELEMENT(JJ),GENERATION(JJ),DATETIME
                ELSE
                  WRITE(LUNOUT,FMT=
     &            '('' ***Fetch: '',A40,''/GEN='',A4,''/'',A20)')
     &            ELEMENT(JJ),GENERATION(JJ),DATETIME
                ENDIF
                IF ( FETCH .AND. LUNOUT .EQ. 6 )
     &            WRITE(LUNOUT,FMT='(''    '')')
              ENDIF
C
C ****  Return element name and generation number
C
              NELEM = NELEM + 1
              CMS_ELEMENT(NELEM) = ELEMENT(JJ)
              READ (UNIT=GENERATION(JJ),FMT=*) CMS_GENERATION(NELEM)

              IF ( FETCH ) THEN

                CALL WORD (ELEMENT(JJ),I,J,L)
                CALL WORD (GENERATION(JJ),N,M,K)
                STATUS = CMS$FETCH (LDB,
     &                              ELEMENT(JJ)(I:J),
     &                              ,
     &                              GENERATION(JJ)(N:M),
     &                              ,,
     &                              NOHISTORY)
                IF ( .NOT. STATUS ) CALL LIB$SIGNAL (%VAL(STATUS))
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDIF

  999 RETURN
      END

      INTEGER FUNCTION OUTPUT_ROUTINE
     &  (NEW_ELEMENT,LDB,USER_PARAM,ELEMENT_ID, GENERATION_ID,
     &  USER_NAME_ID,TRANS_TIME, CREATE_TIME, REVISION_TIME,
     &  REMARK_ID,CLASS_LIST_ID, FORM, ATTR, REVISION_NUMBER,
     &  RESERVATIONS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Routine called by CMS$SHOW_GENERATION
C-
C-   Returned value  : 1
C-   Inputs  : See manual: VAX DEC/CMS Callable Interface Manual.
C-   Outputs : See above
C-   Controls: See above
C-
C-   Created  10-JUL-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER NEW_ELEMENT
      INTEGER LDB(50)
      INTEGER USER_PARAM
      INTEGER ELEMENT_ID
      INTEGER GENERATION_ID
      INTEGER USER_NAME_ID
      INTEGER TRANS_TIME(2)
      INTEGER CREATE_TIME(2)
      INTEGER REVISION_TIME(2)
      INTEGER REMARK_ID
      INTEGER CLASS_LIST_ID
      INTEGER FORM
      INTEGER ATTR
      INTEGER REVISION_NUMBER
      INTEGER RESERVATIONS

      CHARACTER*79  ELEMENT_NAME,GENERATION_NUMBER
      EXTERNAL CMS$GET_STRING
      INTEGER I,J,L

      INTEGER NUMBER,OFFSET
      INTEGER MAXELEM
      PARAMETER( MAXELEM = 2000 )
      CHARACTER*80 ELEMENT(0:2*MAXELEM)
      CHARACTER*20 GENERATION(0:2*MAXELEM)
      INTEGER    DATE_TIME(2,0:2*MAXELEM)
      COMMON / ELEMENT_NAME_LIST / OFFSET,NUMBER, ELEMENT, DATE_TIME,
     &  GENERATION

C----------------------------------------------------------------------
      IF ( NUMBER .LT. MAXELEM ) THEN
        CALL CMS$GET_STRING (ELEMENT_ID, ELEMENT_NAME)
        CALL CMS$GET_STRING (GENERATION_ID, GENERATION_NUMBER)
        NUMBER = NUMBER + 1
        ELEMENT(OFFSET+NUMBER)     = ELEMENT_NAME
        GENERATION(OFFSET+NUMBER)  = GENERATION_NUMBER
        DATE_TIME(1,OFFSET+NUMBER) = REVISION_TIME(1)
        DATE_TIME(2,OFFSET+NUMBER) = REVISION_TIME(2)
      ENDIF

      OUTPUT_ROUTINE = 1
  999 RETURN
      END
