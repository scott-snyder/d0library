C-   Updated  24-MAR-2004   sss - compile with g77.
      SUBROUTINE OUTMSG(STRING)
      CHARACTER*(*) STRING
      write (*,10),STRING
   10 FORMAT(A)
      RETURN
      END

