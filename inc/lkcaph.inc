C----------------------------------------------------------------------
C-
C-   Name     LKCAPH.INC
C-   Purpose  Permanent link area for CAPH banks
C-   Created  23-FEB-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
C
C     JPROC     Address of PROC bank
C     JCAPH     Address of currently selected CAPH banks
C     ICAPH     Stack pointer
C     KCAPH(*)  Stack containing CAPH bank addresses
C
      INTEGER MXCAPH
      PARAMETER (MXCAPH=20)
      INTEGER ICAPH,JPROC,JCAPH,KCAPH(MXCAPH)
      COMMON /LKCAPH/ ICAPH,JPROC,JCAPH,KCAPH
