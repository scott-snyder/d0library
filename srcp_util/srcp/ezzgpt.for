      SUBROUTINE EZZGPT (LSRCP,LPTI,LPTO,LPTV,LPTT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-      Return base addresses of the different sections of the selected
C-      SRCP bank.
C-
C-   Inputs  : None
C-
C-   Outputs : LSRCP            Address of SRCP bank
C-             LPTI             Base address of identifier list
C-             LPTO             Base address of order list
C-             LPTV             Base address of value list
C-             LPTT             Base address of value-type list
C-   Controls: None
C-
C-   Created  15-DEC-1988   Harrison B. Prosper
C-   Updated  15-MAY-1990   Harrison B. Prosper   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LPTI,LPTO,LPTV,LPTT
C
      INCLUDE 'D0$LINKS:IZSRCP.LINK'
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
C
C ****  Get bank addresses
C
      LSRCP = KSRCP(ISRCP)
C
C ****  Compute actual base addresses for data records etc.
C
      LPTI = LSRCP+IC(LSRCP+JJPIDS)-1 ! Identifiers
      LPTO = LSRCP+IC(LSRCP+JJPORD)-1 ! Order list
      LPTV = LSRCP+IC(LSRCP+JJPVAL)-1 ! Values
      LPTT = LSRCP+IC(LSRCP+JJPTYP)-1 ! Type
C
  999 RETURN
      END
