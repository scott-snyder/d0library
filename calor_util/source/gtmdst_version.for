      SUBROUTINE GTMDST_VERSION( LMDST, IVERSION, IER )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Getting the version of the QCD MDST bank is
C-    not straightforward because a great portion of the data in which
C-    MDST was version 2 was mistakenly labeled version 1.
C-
C-    VERSION 1: CAPH,JETS,PMUO,PNUT,PELC,PTAU and PPHO banks
C-    VERSION 2: GLOB,JTCS added
C-    VERSION 3: Add extra PELC/PPHO/GLOB words
C-    VERSION 4: Add extra PTAU words
C-
C-   Inputs  :
C-              LMDST [I] Zebra pointer to MDST bank
C-   Outputs :
C-           IVERSION [I] Real MDST version number
C-                IER [I] Error code 0=ok -1=LMDST is <=0
C-                          =2 unknown error (overwrite?)
C-   Controls:
C-
C-   Created  26-SEP-1994   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER   LMDST, IVERSION, IER
      INTEGER   NLEN
C----------------------------------------------------------------------
      IER   = 0       ! OK
C
C: check MDST pointer
C
      IF ( LMDST .LE. 0 ) THEN
        IER   = -1
        GOTO 999
      ENDIF
C
C: If the version number is 2 or greater it is probably right.
C
      IVERSION  = IQ( LMDST + 1 )

      IF ( IVERSION .GT. 1 ) THEN
        IER   =   0
        GOTO 999
      ENDIF
C
C: Check the header integrity
C
      NLEN  = IQ( LMDST + 4 ) - 2
      IF ( NLEN .EQ. 27 ) THEN
        IER = 0
        IVERSION = 2
      ELSEIF ( NLEN .EQ. 21 ) THEN
        IER = 0
        IVERSION = 1
      ELSE                          ! ?
        IER = -2
      ENDIF
      GOTO 999

  999 RETURN
      END
