      SUBROUTINE HMATRIX_DROP (NAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Drop the Hmatrix banks identified by the
C-   given name.
C-
C-   Inputs  : NAME   [C*]      Name of Hmatrix
C-   Outputs : None
C-   Controls: None
C-
C-   Created  24-JAN-1991   Harrison B. Prosper   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) NAME
      INTEGER IER,LBANK
      CHARACTER*132 CTEMP
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
C----------------------------------------------------------------------
C
      CALL HMATRIX_SET (NAME(1:LEN(NAME)),IER)
      IF ( IER .NE. 0 ) THEN
        CTEMP = 'Problem selecting Hmatrix: '//NAME
        CALL ERRMSG('HMATRIX','HMATRIX_DROP',CTEMP,'W')
        GOTO 999
      ELSE
C
C ****  Reset the address in the link area to zero to signify
C ****  that a bank has been dropped.
C
        LBANK = HMTR_LINKS(HMTR_PTR)    ! Get bank address
        CALL MZDROP (IXSTP,LBANK,' ')   ! Drop Hmatrix bank(s)
        HMTR_LINKS(HMTR_PTR) = 0        ! Dropped
        HMTR_TOTAL = HMTR_TOTAL - 1     ! Decrement HMTR bank count
        CALL HMATRIX_RESET
      ENDIF
  999 RETURN
      END
