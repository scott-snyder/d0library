      SUBROUTINE BLDNLX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create banks DNLI and DNLO containing sense 
C-                         wire nonlinearity parameters
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   1-OCT-1992   Domenico Pizzuto
C-   Updated   5-JAN-1993   Qizhong Li-Demarteau  use D0OPEN and fixed 
C-                                                bombs on UNIX
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$LINKS:IZDNLI.LINK'
      INCLUDE 'D0$LINKS:IZDNLO.LINK'
C
      INTEGER I,WIRE,LAYER,SIDE,MPDNLI (5),MPDNLO (5)
      INTEGER LDTVA,GZDTVA,LDNLI,LDNLO,GZDNLI,GZDNLO,IPDNLI,IPDNLO
      REAL NLI (3,5,0:3,0:1),NLO (7,2,0:3,0:1)
      LOGICAL OK
C
      DATA MPDNLI /0,0,0,32,0/
      DATA MPDNLO /0,0,0,30,0/
C----------------------------------------------------------------------
C
C- Read file containing nonlinearity coefficients for inner and outer wires
C
      CALL D0OPEN(32,'NONLIN_INNER','IF',OK)
      IF (.NOT. OK) THEN
        CALL ERRMSG('CDCSTP','BLDNLX',
     &    'Unable to open input file D0NONLINI.DAT','W')
        GOTO 999
      ENDIF
      CALL D0OPEN(33,'NONLIN_OUTER','IF',OK)
      IF (.NOT. OK) THEN
        CALL ERRMSG('CDCSTP','BLDNLX',
     &    'Unable to open input file D0NONLINO.DAT','W')
        GOTO 999
      ENDIF
      READ (32,2002) ((((NLI (I,WIRE,LAYER,SIDE),I=1,3),WIRE=1,5),
     +                                           LAYER=0,3),SIDE=0,1)
      READ (33,2003) ((((NLO (I,WIRE,LAYER,SIDE),I=1,7),WIRE=1,2),
     +                                           LAYER=0,3),SIDE=0,1)
 2002 FORMAT (T1,3F12.6)
 2003 FORMAT (T1,7E14.5)
      CLOSE (32)
      CLOSE (33)

C- Create nonlinearity banks DNLI and DNLO hanging from DTVA via links -1 and -2
      CALL UCTOH ('DNLI',MPDNLI (1),4,4)
      CALL UCTOH ('DNLO',MPDNLO (1),4,4)
      CALL MZFORM ('DNLI','2I -F',MPDNLI (5))
      CALL MZFORM ('DNLO','2I -F',MPDNLO (5))

       DO 10 LAYER = 0, 3
        LDTVA = GZDTVA (LAYER)
        CALL MZLIFT (IDVSTP,LDNLI,LDTVA,-IZDNLI,MPDNLI,0)
        IC (LDNLI+1) = 2          ! # of drift sides per sense wire
        IC (LDNLI+2) = 3          ! # of parameters per drift side in DTNI
        CALL MZLIFT (IDVSTP,LDNLO,LDTVA,-IZDNLO,MPDNLO,0)
        IC (LDNLO+1) = 2          ! # of drift sides per sense wire
        IC (LDNLO+2) = 7          ! # of parameters per drift side in DTNO

        IPDNLI = GZDNLI (LAYER)+2
        IPDNLO = GZDNLO (LAYER)+2

C- Fill inner wire nonlinearity bank DNLI
         DO 15 WIRE = 1, 5
          DO 16 SIDE = 0, 1
           DO 17 I = 1, 3
            C (IPDNLI+I) = NLI (I,WIRE,LAYER,SIDE)
   17      CONTINUE
           IPDNLI = IPDNLI+IC (LDNLI+2)
   16     CONTINUE
   15    CONTINUE

C- Fill outer wire nonlinearity bank DNLO
         DO 20 WIRE = 1, 2
          DO 21 SIDE = 0, 1
           DO 22 I = 1, 7
            C (IPDNLO+I) = NLO (I,WIRE,LAYER,SIDE)
   22      CONTINUE
           IPDNLO = IPDNLO+IC (LDNLO+2)
   21     CONTINUE
   20    CONTINUE

   10  CONTINUE

  999 RETURN
      END
