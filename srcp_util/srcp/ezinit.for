      SUBROUTINE EZINIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Declare ALL SRCP banks in /ZEBSTP/ to the
C-                         EZ routines. This routine would normally
C-                         be called after ZEBRA banks have been read
C-                         into /ZEBSTP/. Any SRCP banks amongst the
C-                         banks read in would be automatically declared
C-                         to the EZ package. The store /ZEBSTP/ is
C-                         searched sequentially using LZFIDH for SRCP
C-                         banks. Upon finding an SRCP bank EZNAME
C-                         is called with the name obtained from the
C-                         header section of the SRCP bank. NOTE: If you
C-                         know explicitly where the SRCP banks are in
C-                         /ZEBSTP/ it is clearly quicker to call EZNAME
C-                         directly. The routine also searches for CRCP
C-                         banks and builds SRCP banks from them.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: NOne
C-
C-   Created  14-SEP-1989   Harrison B. Prosper
C-   Updated   6-MAY-1991   Harrison B. Prosper
C-      Change error level from Fatal to Warning
C-   Updated  10-JUL-1992   Harrison B. Prosper
C-      Search for CRCP banks also
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER LF,LGO,IDH,LZFIDH,J
      CHARACTER*4  BANK(2)
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
C
C ****  Scan for SRCP/CRCP banks
C
      BANK(1) = 'SRCP'
      BANK(2) = 'CRCP'
      DO J = 1, 2
        CALL DCTOH (4,BANK(J),IDH)
        LF = 1
        LGO= 0                            ! Start at header bank
        DO WHILE ( LF .GT. 0 )
          LF = LZFIDH (IDVSTP,IDH,LGO)    ! Find next SRCP/CRCP bank
C
          IF ( LF .GT. 0 ) THEN
            LGO = LF
C
C ****  Declare SRCP/CRCP bank to EZ routines
C
            CALL EZNAME(' ',LF,0)
          ENDIF
        ENDDO
      ENDDO
C
  999 RETURN
      END
