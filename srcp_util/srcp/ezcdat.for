      SUBROUTINE EZCDAT (LUNIN,LUNOUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert DATA statements into formatted lines
C-                         that can be read by EZRDF .
C-
C-   Inputs  : LUNIN       Logical unit number of input file
C-             LUNOUT      Logical unit number of output file
C-   Outputs : None
C-   Controls: None
C-
C-   Created  15-SEP-1988   Harrison B. Prosper
C-   Modified 12-NOV-1988   Harrison B. Prosper
C-                          Uses general SRCP printing routine EZZDMP
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL      APPEND
C
      INTEGER      LUNIN,LUNOUT
      INTEGER      NNAME,NVAL,II,JJ,NN,NNAM,IDATA,ILINE,NSAY,ISAY
      INTEGER      I,J,K,L,M,N,NBUFF,NUMMAX,NMAX,TOTAL,NDATA,III
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
C----------------------------------------------------------------------
      CHARACTER*(CHRCRD) INLINE,OUTLIN,STRING
      CHARACTER*160 NAMS
      CHARACTER*(CHRCRD*10) BUFFER,VALS
      PARAMETER( NSAY   = 100 )
      PARAMETER( NUMMAX = 500 )
      PARAMETER( NMAX   = 5000 )
      CHARACTER*8 NAME(NUMMAX)
      CHARACTER*40 COMMEN,REMARK(NUMMAX)
      CHARACTER*(CHRCRD) SAYING(NSAY)
      INTEGER      TYPE(NMAX),INUMBR(NMAX),LINE(NMAX),NUMVAL(NMAX)
      REAL         NUMBER(NMAX)
C----------------------------------------------------------------------
C
C ****  Perform initial pass over file
C
      IDATA = 0
      ILINE = 0
   20 CONTINUE
      READ (LUNIN,40,END=25) INLINE
      ILINE = ILINE + 1
      IF ( (INLINE(1:1)  .EQ. ' ') .AND. ! Look for DATA statement
     &     (INDEX(INLINE,'DATA ') .GT. 0) ) THEN
        IDATA = IDATA + 1
        LINE(IDATA) = ILINE
      ENDIF
      GOTO 20
C
   25 CONTINUE
C
      REWIND LUNIN
      NDATA = IDATA
      LINE(IDATA+1) = ILINE + 1         ! To get correct boundary cond.
C
C ******************************
C ****  GET DATA STATEMENT  ****
C ******************************
C
C ****  START OF LOOP OVER DATA STATEMENTS
C
      OUTLIN = '! SRCP V3.1 16-MAR-1989'
      WRITE(LUNOUT,FMT='(A)') OUTLIN
C
      IDATA = 1
      ILINE = 0
   30 CONTINUE
      READ (LUNIN,40,END=300) INLINE
   40 FORMAT(A)
      ILINE = ILINE + 1                 ! Increment line counter
C
      IF ( INLINE(1:1) .NE. ' ' ) THEN  ! Check for comments
        IF ( INLINE(1:1) .NE. '!' ) INLINE = '!'//INLINE(2:)
        IF ( INLINE(1:2) .EQ. '!!') INLINE = INLINE(2:)
        WRITE(LUNOUT,FMT='(A)') INLINE
        GOTO 30
      ENDIF
C
C ****  Look for word DATA
C
      IF ( LINE(IDATA) .NE. ILINE ) GOTO 30
      IDATA = IDATA + 1                 ! Increment DATA statement
C                                       ! counter
      I = INDEX(INLINE,'DATA ')         ! Remove token DATA from string
      INLINE = INLINE(I+5:)
C
C ****  Extract possible comment from line
C
      J = INDEX (INLINE,'!')
      IF ( J .GT. 0 ) THEN
        COMMEN = INLINE(J+1:)
        INLINE = INLINE(:J-1)
      ELSE
        COMMEN = ' '
      ENDIF
C
C ****  Initialize buffer; remove trailing blanks
C
      CALL SWORDS (INLINE,I,J,N)
      BUFFER = INLINE(I:J)
      NBUFF = N
C
C ****  If current line continues onto next line append further lines
C       to character buffer.
C
      ISAY = 0                          ! ZERO remark line counter
      DO 70 I = 1,LINE(IDATA)-LINE(IDATA-1)-1
        READ (LUNIN,40) INLINE
        ILINE = ILINE + 1                 ! Increment line counter
C
        IF ( INLINE(1:1) .EQ. ' ' ) THEN
          APPEND = INLINE(6:6) .NE. ' '
C
          IF ( APPEND ) THEN
            INLINE = INLINE(7:)
            II = INDEX (INLINE,'!')         ! Remove in-line comments
            IF ( II .GT. 0 ) INLINE = INLINE(:II-1)
            CALL SWORDS (INLINE,II,JJ,N)
            BUFFER = BUFFER(1:NBUFF)//INLINE(II:JJ)
            NBUFF = NBUFF + N
          ENDIF
        ELSE
          ISAY = ISAY + 1
          SAYING(ISAY) = INLINE         ! Remember remarks
        ENDIF
   70 CONTINUE
C
C *********************************
C ****  DECODE DATA STATEMENT  ****
C *********************************
C
C
C ****  START OF DATA STATEMENT DECODING LOOP
C
      III = 1
   80 CONTINUE
      I = INDEX (BUFFER(III:NBUFF),'/') + III - 1
C
      J = INDEX (BUFFER(I-4:I),'''')     ! Check if '/' is in 4-char string
      IF ( J .GT. 0 ) THEN
        K = INDEX (BUFFER(I:I+4),'''')
        IF ( K .GT. 0 ) THEN
          N = K-J+3
          IF ( N .EQ. 4 ) THEN
            III = I + 1
            GOTO 80
          ENDIF
        ENDIF
      ENDIF
C
      IF ( I .LE. 0 ) THEN              ! Check for end-of-data statement
        IF ( ISAY .GT. 0 ) THEN         ! Print possible remarks
          DO 85 I =  1,ISAY
            WRITE(LUNOUT,FMT='(A)') SAYING(I)
   85     CONTINUE
        ENDIF
        GOTO 30
      ENDIF
C
C ****  Decode BUFFER into NAME and VALUES strings
C
      NNAM = I + 1
      NAMS = BUFFER(:NNAM-2)//','
      BUFFER = BUFFER(NNAM:)
      NBUFF = NBUFF - NNAM + 1
C
   88 CONTINUE
      I = INDEX (BUFFER(III:NBUFF),'/') + III - 1
C
      J = INDEX (BUFFER(I-4:I),'''')     ! Check if '/' is in 4-char string
      IF ( J .GT. 0 ) THEN
        K = INDEX (BUFFER(I:I+4),'''')
        IF ( K .GT. 0 ) THEN
          N = K-J+3
          IF ( N .EQ. 4 ) THEN
            III = I + 1
            GOTO 88
          ENDIF
        ENDIF
      ENDIF
C
      NVAL = I - 1
      VALS = BUFFER(:NVAL)
      BUFFER = BUFFER(NVAL+2:)
      NBUFF = NBUFF - NVAL + 1
C
C ****  Get name list
C ****  NOTE: If more than one name assume one value/name
C
      NNAME = 0
   90 CONTINUE
      I = INDEX (NAMS,',')
      IF ( I .GT. 0 ) THEN
        IF ( I .EQ. 1 ) THEN
          STRING = ' '
        ELSE
          STRING = NAMS(:I-1)
        ENDIF
        CALL WORD (STRING,II,JJ,NN)
        IF ( NN .GT. 0 ) THEN
          NNAME = NNAME + 1
          NAME(NNAME)   = STRING(II:JJ)
          REMARK(NNAME) = COMMEN
        ENDIF
        NAMS = NAMS(I+1:)
        GOTO 90
      ENDIF
      IF ( NNAME .LE. 0 ) GOTO 30
C
C ****  Extract numbers from VALS string
C
      CALL VALUES (VALS(1:NVAL),NUMBER,TYPE,TOTAL)
C
C ******************************
C ****  BUILD SRCP RECORDS  ****
C ******************************
C
C ****  Print out numbers in SRCP format
C
      IF ( NNAME .EQ. 1 ) THEN
        CALL EZZDMP (LUNOUT,NAME,REMARK,NUMBER,TYPE,TOTAL)
      ELSE
        DO 100  I =  1,NNAME
          CALL EZZDMP (LUNOUT,NAME(I),REMARK(I),NUMBER(I),TYPE(I),1)
  100   CONTINUE
      ENDIF
C
C ****  END OF DATA STATEMENT DECODING LOOP
C
      III = 1
      GOTO 80
C
C ****  END OF LOOP OVER DATA STATEMENTS
C
  300 CONTINUE
C
  999 RETURN
      END
