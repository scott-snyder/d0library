      SUBROUTINE FLD0RG(NENTRY,TEXT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill D0RG (NENTRY Entries) with TEXT
C-
C-   Inputs  : NENTRY - Number of Entries
C-                             (version 1 fills 40 character lines)
C-                             (version 2 fills 72 character lines)
C-                              version # is obtained from bank itself
C-             TEXT   - Text strings to put into bank
C-   Outputs :
C-   Controls:
C-
C-   Created  15-SEP-1989   Alan M. Jonckheere
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER NENTRY
      CHARACTER*(*) TEXT(*)
C
      INCLUDE 'D0$INC:GCBANK.INC'
      CHARACTER*72 TEMP,BLANK/' '/
      INTEGER NTEMP(18)
      EQUIVALENCE ( TEMP,NTEMP )
      INTEGER LD0RG,GZD0RG
      INTEGER NTEXT
      INTEGER I,J
C----------------------------------------------------------------------
      LD0RG = GZD0RG()
      IF ( LD0RG.LE.0 ) GOTO 999
      IF ( IQ(LD0RG+1).EQ.1 ) THEN
        NTEXT = 10
      ELSE
        NTEXT = 18
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
