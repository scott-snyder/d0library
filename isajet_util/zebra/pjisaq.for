C DEC/CMS REPLACEMENT HISTORY, Element PJISAQ.FOR
C *3    10-JAN-1990 10:40:05 SERBAN "check that PJET bank exists"
C *2    22-DEC-1989 12:11:18 CSTEWART "Chip Stewart: MAKES REFERENCE LINK FROM ISAQ TO PJET"
C *1    21-DEC-1989 14:40:45 SERBAN "PJET subroutines"
C DEC/CMS REPLACEMENT HISTORY, Element PJISAQ.FOR
      SUBROUTINE PJISAQ(LPJHDI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO FILL LINK FROM ISAQ (-2) TO
C-   A REFERENCE LINK TO PJET JET NUMBER.
C-
C-   Inputs  :   LPJHDI - ADDRESS OF PJET HEADER BANK LPJHDI = 0 - first PJHD bank
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created  29-NOV-1989   Chip Stewart
C-   Updated   2-JUL-1993   Chip Stewart  - protected against ISP1 overwrite 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LPJHD,LPJET,LPJPT,LPJHDI
      INTEGER LISAQ,J,GZPJHD
      CHARACTER BANK*4
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPJET.LINK'
      INCLUDE 'D0$LINKS:IZPJPT.LINK'
C----------------------------------------------------------------------
C
C ****  DETERMINE WHICH ISAQ PARTONS GOTO WHICH PJET FROM PJPT
C
      LPJHD = LPJHDI
      LPJET=0
      IF (LPJHD .EQ. 0 ) LPJHD = GZPJHD ()           ! PJET HEADER BANK
      IF(LPJHD.GT.0) LPJET = LQ ( LPJHD - IZPJET)    ! PJET BANK FROM PJHD
      IF(LPJET.LE.0) GOTO 999
    1 LPJPT = LQ ( LPJET - IZPJPT)              ! PJET POINTER BANK HANGING FROM
      DO 2, J = 2, IQ(LPJPT-3)                  ! LOOP OVER POINTERS TO ISAQ
C
C ****  OVERWRITE ISAQ LINK WITH REFERNCE TO PJET BANKS
C
       LISAQ = LQ(LPJPT- J)
       CALL DHTOC(4,IQ(LISAQ-4),BANK)
       IF(BANK.EQ.'ISP1') THEN
         LISAQ = LQ(LISAQ-2)
       END IF
       IF(LISAQ.GT.0) LQ (LISAQ - 2) = LPJET
    2 CONTINUE
      LPJET = LQ ( LPJET  )
      IF ( LPJET .GT. 0 ) GOTO 1
  999 RETURN
      END
