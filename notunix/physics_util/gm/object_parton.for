      SUBROUTINE OBJECT_PARTON(IDX,NMAX,ARRAY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return quantities for object ISAJ-PARTON
C-
C-   Inputs  : IDX      [I]   Object Number
C-             NMAX     [I]   Maximum number of quantities to return
C-   Outputs : ARRAY(*) [R]   Array of quantities
C-
C-              ARRAY(1..9)
C-                            PX, PY, PZ, E, ET, ETA, PHI, DET_ETA, QUALITY
C-                    I = 9
C-              ARRAY(I+1)    ISAJET-ID of the parton
C-              ARRAY(I+2)    closest parent NUMBER
C-              ARRAY(I+3)    closest parent ISAJET-ID
C-              ARRAY(I+4)    family NUMBER
C-              ARRAY(I+5)    family ISAJET-ID
C-   Controls:
C-
C-   Created   11-FEB-1993   Stan M. Krzywdzinski and Harrison B. Prosper
C-   Updated  25-MAY-1993   Marc Paterno  Corrected FLINT complaints 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDX, NMAX
      REAL    ARRAY(*)
      INTEGER NOBJS, NSIZE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      INTEGER STATUS, I, J, K, L, II, NN
      INTEGER NUMBER, NTOTAL
      INTEGER LISAE, GZISAE, GZISAJ, LISAJ, GZISAQ, LISAQ
      INTEGER NUMBER1, NUMBER2, NP, DEBUG_LEVEL
C
      INTEGER IBUFFER(100)
      REAL    BUFFER(100)
      EQUIVALENCE (IBUFFER(1),BUFFER(1))
C
      INTEGER    MAXOBJECTS
      PARAMETER (MAXOBJECTS = 200)
      REAL P(8,MAXOBJECTS)
      INTEGER PARTONID(MAXOBJECTS),BANKID(MAXOBJECTS)
      INTEGER PARENTNUM(MAXOBJECTS),PARENTID(MAXOBJECTS)
      INTEGER FAMILYNUM(MAXOBJECTS),FAMILYID(MAXOBJECTS)
C
      INTEGER NLIN
      CHARACTER LABEL*8, LINE*80,STRING*8
      REAL    VALUE
C
      INTEGER LUN, SSUNIT
C----------------------------------------------------------------------
      INTEGER MINNUM
      PARAMETER( MINNUM  =   9 )
      INTEGER JPX, JPY, JPZ, JE, JPT, JETA, JPHI, JDETA, JQUAL, JBASE
      PARAMETER( JPX = 1,
     &           JPY = JPX + 1,
     &           JPZ = JPY + 1,
     &           JE  = JPZ + 1,
     &           JPT = JE  + 1,
     &           JETA  = JPT   + 1,
     &           JPHI  = JETA  + 1,
     &           JDETA = JPHI  + 1,
     &           JQUAL = JDETA + 1,
     &           JBASE = JQUAL)
C----------------------------------------------------------------------
      LOGICAL DEBUG
      LOGICAL FIRST
      SAVE NUMBER, NTOTAL
      DATA DEBUG /.FALSE./
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
C ****  OBJECT: PARTON FROM ISAJET
C
      II = MIN(IDX,NUMBER)
      NN = MIN(NMAX,NTOTAL)
      CALL VZERO(BUFFER,NN)
C
      IF ( II .GT. 0 ) THEN
C
C ****  FIXED PART
C

        CALL UCOPY(P(1,II),BUFFER(JPX),4)
        BUFFER(JPT)     = SQRT(P(1,II)**2+P(2,II)**2)
        BUFFER(JETA)    = P(8,II)
        BUFFER(JPHI)    = P(6,II)
C
C ****  OTHER
C
        BUFFER(JBASE+1) = PARTONID(II)      ! ISAJET-ID of the parton
        BUFFER(JBASE+2) = PARENTNUM(II)     ! closest parent NUMBER
        BUFFER(JBASE+3) = PARENTID(II)      ! closest parent ISAJET-ID
        BUFFER(JBASE+4) = FAMILYNUM(II)     ! family NUMBER
        BUFFER(JBASE+5) = FAMILYID(II)      ! family ISAJET-ID
      ENDIF
      CALL UCOPY(BUFFER,ARRAY,NN)
      RETURN
C
C
      ENTRY NOBJ_PARTONS(NOBJS,NSIZE)
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL TRNLNM('DEB_OBJECT_PARTON',STRING,L)
        DEBUG = STRING(1:L) .NE. 'DEB_OBJECT_PARTON'
        IF ( DEBUG ) THEN
          DEBUG_LEVEL = VALUE(STRING(1:L),I,J,K)
          LUN = SSUNIT()
        ENDIF
      ENDIF   ! FIRST
C
      NUMBER= 0
C
      LISAE = GZISAE()
      IF ( LISAE .LE. 0 ) THEN
        GO TO 999
      ENDIF
C
C ****  Scan ISAJ for original partons.
C ****  An original parton has reference link (-1) = 0.
C
      LISAJ = GZISAJ()
      IF ( LISAJ .LE. 0 ) GOTO 999
C
      DO WHILE ( LISAJ .GT. 0 )
        IF ( LQ(LISAJ-1) .EQ. 0 ) THEN
          IF ( NUMBER .GE. MAXOBJECTS )  GOTO 999
          NUMBER = NUMBER + 1
          PARTONID(NUMBER) = IQ(LISAJ+1)
          BANKID(NUMBER)   = IQ(LISAJ-5)
          PARENTNUM(NUMBER)= 0
          PARENTID(NUMBER) = 0
          FAMILYNUM(NUMBER)= NUMBER
          FAMILYID(NUMBER) = PARTONID(NUMBER)
C
          CALL UCOPY(Q(LISAJ+2),P(1,NUMBER),8)
C
        ENDIF
        LISAJ = LQ(LISAJ)
      ENDDO     ! WHILE
C
      IF ( NUMBER .LE. 0 ) GOTO 999
C
      IF ( DEBUG ) THEN
        LINE = 'Original ISAJ partons: '
        NLIN = 23
        DO I = 1, NUMBER
          LINE = LINE(1:NLIN)//LABEL(PARTONID(I))
          NLIN = NLIN + 8
        ENDDO
        WRITE(LUN,'('' '',A)') LINE(1:NLIN)
C
        IF ( DEBUG_LEVEL .GT. 1 ) THEN
C
C ****  Scan ISAQ for "initial" partons - but DO NOT include them
C ****  into objects, just write them out ...
C ****  An "initial" parton has reference link (-1) = 0.
C
          LISAQ = GZISAQ()
          LINE = 'Initial  ISAQ partons: '
          NLIN = 23
          DO WHILE ( LISAQ .GT. 0 )
            IF ( LQ(LISAQ-1) .EQ. 0 ) THEN
              IF ( NLIN .GE. 72 ) THEN
                WRITE(LUN,'('' '',A)') LINE(1:NLIN)
                LINE = ' '
                NLIN = 23
              ENDIF
              LINE = LINE(1:NLIN)//LABEL(IQ(LISAQ+1))
              NLIN = NLIN + 8
            ENDIF
            LISAQ = LQ(LISAQ)
          ENDDO
          WRITE(LUN,'('' '',A)') LINE(1:NLIN)
        ENDIF
      ENDIF
C
C ****   Now look for children  within ISAJ banks
C ****  of all original partons within ISAJ banks
C
      NUMBER1 = 1           ! Define set of
      NUMBER2 = NUMBER      ! parents
      IF ( NUMBER .GE. MAXOBJECTS )  GOTO 999
      CALL SET_MAX_ISAJET_CHILDREN(MAXOBJECTS-NUMBER)
      DO I =  NUMBER1, NUMBER2
        CALL GET_ISAJ_CHILDREN(PARTONID(I),BANKID(I),
     &                         PARTONID(NUMBER+1),
     &                         BANKID(NUMBER+1),
     &                         P(1,NUMBER+1),NP,STATUS)
        IF ( NP .GT. 0 )  THEN
          DO J =  1, NP
            IF ( NUMBER .GE. MAXOBJECTS )  GOTO 999
            NUMBER = NUMBER + 1
            PARENTNUM(NUMBER)= I
            PARENTID(NUMBER) = PARTONID(I)
            FAMILYNUM(NUMBER)= FAMILYNUM(I)
            FAMILYID(NUMBER) = FAMILYID(I)
          ENDDO
        ENDIF
      ENDDO
C
C ****  Now look for children within ISAQ banks
C ****         of all parents within ISAJ banks found above
C
      NUMBER1 = 1           ! Define set of
      NUMBER2 = NUMBER      ! parents
      IF ( NUMBER .GE. MAXOBJECTS )  GOTO 999
      CALL SET_MAX_ISAJET_CHILDREN(MAXOBJECTS-NUMBER)
      DO I =  NUMBER1, NUMBER2
        CALL GET_ISAQ_CHILDREN(PARTONID(I),BANKID(I),
     &                       PARTONID(NUMBER+1),
     &                       BANKID(NUMBER+1),
     &                       P(1,NUMBER+1),NP,STATUS)
        IF ( NP .GT. 0 )  THEN
          DO J =  1, NP
            IF ( NUMBER .GE. MAXOBJECTS )  GOTO 999
            NUMBER = NUMBER + 1
            PARENTNUM(NUMBER)= I
            PARENTID(NUMBER) = PARTONID(I)
            FAMILYNUM(NUMBER)= FAMILYNUM(I)
            FAMILYID(NUMBER) = FAMILYID(I)
          ENDDO
        ENDIF
      ENDDO
C
C ****  Now look for children within ISAL banks
C ****         of all parents within ISAQ banks found above
C
      NUMBER1 = NUMBER2 + 1 ! Define set of
      NUMBER2 = NUMBER      ! parents
      IF ( NUMBER1 .GT. NUMBER2 )       GOTO 999
      IF ( NUMBER .GE. MAXOBJECTS )  GOTO 999
      CALL SET_MAX_ISAJET_CHILDREN(MAXOBJECTS-NUMBER)
      DO I =  NUMBER1, NUMBER2
        CALL GET_ISAL_CHILDREN(PARTONID(I),BANKID(I),
     &                         PARTONID(NUMBER+1),
     &                         BANKID(NUMBER+1),
     &                         P(1,NUMBER+1),NP,STATUS)
        IF ( NP .GT. 0 )  THEN
          DO J =  1, NP
            IF ( NUMBER .GE. MAXOBJECTS )  GOTO 999
            NUMBER = NUMBER + 1
            PARENTNUM(NUMBER)= I
            PARENTID(NUMBER) = PARTONID(I)
            FAMILYNUM(NUMBER)= FAMILYNUM(I)
            FAMILYID(NUMBER) = FAMILYID(I)
          ENDDO
        ENDIF
      ENDDO
C
  999 CONTINUE
      NTOTAL= MINNUM + 5
      NOBJS = NUMBER
      NSIZE = NTOTAL
C
      IF ( DEBUG ) THEN
C
C ***   Print parents and their children
C
        DO I = 1, NUMBER
          LINE = ' '
          NLIN = 13
          IF (PARENTNUM(I) .LE. 0) THEN
            LINE = LABEL(PARTONID(I))//' --> '
          ELSE
            LINE = LINE(1:NLIN)//LABEL(PARTONID(I))//' --> '
            NLIN = NLIN + 13
          ENDIF
          NP = 0
          DO J = 1, NUMBER
            IF ( (I .NE. J) .AND. (PARENTNUM(J) .GT. 0) ) THEN
              IF ( (PARENTNUM(J) .EQ. I)           .AND.
     &             (PARENTID(J)  .EQ. PARTONID(I))      ) THEN
                NP = NP + 1
                LINE = LINE(1:NLIN)//LABEL(PARTONID(J))
                NLIN = NLIN + 8
              ENDIF
            ENDIF
          ENDDO
          IF ( NP .GT. 0) WRITE(LUN,'('' '',A)') LINE(1:NLIN)
        ENDDO
      ENDIF
C
      RETURN
      END
