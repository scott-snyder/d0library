      SUBROUTINE HMATRIX_FIND (NAME,LBANK,ID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given the name of an Hmatrix return the address
C-   of the corresponding HMTR bank and the index into the link area 
C-   HMTR_LINKS (in /ZHMATRIX/).
C-
C-   Inputs  : NAME        Name of Hmatrix. Up to 32 characters.
C-
C-   Outputs : LBANK       Address of HMTR bank
C-             ID          Index of bank address in ZHMATRIX link area
C-                        > 0 --- OK
C-                          0 --- Bank not found.
C-   Controls: None
C-
C-   Created  24-JAN-1991   Harrison B. Prosper   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) NAME
      INTEGER LBANK
      INTEGER ID
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZHMATRIX.INC'     ! Link area
      INCLUDE 'D0$INC:NHMATRIX.INC'     ! Names of Hmatrices
C----------------------------------------------------------------------
      INTEGER I,J
      CHARACTER*32 STRING
C----------------------------------------------------------------------
C
      LBANK = 0
      ID    = 0
      IF ( HMTR_TOTAL .LE. 0 ) GOTO 999
C
      STRING = NAME(1:LEN(NAME))
      CALL UPCASE (STRING,STRING)
C
C ****  Do simple sequential search
C ****  but skip addresses that are zero (could be dropped banks)
C
      I = 0
      J = 0
      DO WHILE ( (I .LT. HMTR_TOTAL) .AND. (J .LT. HMTR_MAX) )
        J = J + 1
        IF ( HMTR_LINKS(J) .GT. 0 ) THEN
          I = I + 1
          IF ( STRING .EQ. HMTR_NAME(J) ) THEN
            ID = J
            LBANK = HMTR_LINKS(ID)
            GOTO 999
          ENDIF
        ENDIF
      ENDDO
C
  999 RETURN
      END
