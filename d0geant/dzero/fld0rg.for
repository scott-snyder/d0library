      SUBROUTINE FLD0RG(NENTRY,NCHAR,TEXT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill D0RG (NENTRY Entries) with TEXT
C-
C-   Inputs  : NENTRY - Number of Entries
C-             NCHAR  - Number of characters/entry
C-             TEXT   - Text strings to put into bank
C-   Outputs :
C-   Controls:
C-
C-   Created  15-SEP-1989   Alan M. Jonckheere
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER NENTRY,NCHAR
      CHARACTER*(*) TEXT(*)
C
      INCLUDE 'D0$INC:GCBANK.INC'
      CHARACTER*72 TEMP,BLANK/' '/
      INTEGER NTEMP(18)
      EQUIVALENCE ( TEMP,NTEMP )
      INTEGER LD0RG,GZD0RG
      INTEGER NTEXT
      INTEGER ND,I,J
C----------------------------------------------------------------------
      LD0RG = GZD0RG()
      IF ( LD0RG.LE.0 ) GOTO 999
      NTEXT = NCHAR/4
      IF ( 4*NTEXT.NE.NCHAR ) THEN
        NTEXT = NTEXT + 1
      ENDIF
      ND = IQ(LD0RG-1)
      IF ( ND.LT.NENTRY*NCHAR+3 ) THEN
        CALL MZPUSH(IXSTOR,LD0RG,0,NENTRY*NCHAR+3-ND,' ')
      ENDIF
      IQ(LD0RG+2) = NENTRY
      IQ(LD0RG+3) = 4*NTEXT
      DO J = 1, NENTRY
        TEMP = TEXT(J)                    ! Move TEXT into TEMP
        DO I = 1, NTEXT
          IQ(LD0RG+(J-1)*NTEXT+I+3) = NTEMP(I)        ! Store Text
        ENDDO
      ENDDO
  999 RETURN
      END
