      SUBROUTINE MVSTRN(CHARIN,OUTSTR,NCHAR)
C-   Purpose and Methods : Move a character string from CHARIN to OUTSTR
      CHARACTER*(*) CHARIN,OUTSTR
      OUTSTR(1:NCHAR)=CHARIN(1:NCHAR)
      RETURN
      END
