C----------------------------------------------------------------------
C-
C-   Created  22-AUG-1990   Srini Rajagopalan
C-   Modified 05-JUL-1991   Yi-Cheng Liu to include FDC
C-
C----------------------------------------------------------------------
      LOGICAL IMAP(0:11)
C      INTEGER MAP(0:65000)
      INTEGER MAP(8192:20031)
C
      COMMON /CDMAP/ IMAP,MAP
