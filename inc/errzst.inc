C----------------------------------------------------------------------
C-      Buffer for saving error messages with no event to hang on
C-   Created  11-APR-1992   Andrew J. Milder
C-
C----------------------------------------------------------------------
      INTEGER WAIT_SIZ
      PARAMETER( WAIT_SIZ = 10 )
      INTEGER TOTWAIT, NUMWAIT(WAIT_SIZ), SEVERWAIT(WAIT_SIZ), 
     &  LENWAIT(WAIT_SIZ)
      COMMON /ERRZST/ TOTWAIT, NUMWAIT, SEVERWAIT, LENWAIT
