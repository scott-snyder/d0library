      CHARACTER*(*) FUNCTION TRANUP(INTXT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Translate a string to upper-case
C-
C-   Inputs  : INTXT: String to be translated
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-   Updated  10-NOV-1988   Jan S. Hoftun     Use LOWER and UPPER instead
C-                                            of ICHAR and CHAr 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) INTXT
      CHARACTER*80 COM
      INTEGER MAX,K,INT
      CHARACTER*26 LOWER
      CHARACTER*26 UPPER
      DATA LOWER/'abcdefghijklmnopqrstuvwxyz'/
      DATA UPPER/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C----------------------------------------------------------------------
      COM=' '
      MAX=MIN(LEN(INTXT),LEN(TRANUP))
      DO 900 K=1,MAX
      INT=INDEX(LOWER,INTXT(K:K))
      IF(INT.GT.0) THEN
         COM(K:K)=UPPER(INT:INT)
      ELSE 
         COM(K:K)=INTXT(K:K)
      ENDIF
900   CONTINUE  
      TRANUP=COM(1:MAX)
      RETURN
      END
