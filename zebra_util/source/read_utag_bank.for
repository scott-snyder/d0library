      SUBROUTINE READ_UTAG_BANK(IDMAX,NDIMG,XTAGS,XGRP,IGRP,NGRP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : read UTAG bank
C-
C-   Inputs  :  IGRP  - maximum number of groups
C-              NGRP  - maximum number words per group
C-   Outputs :  IDMAX - number of groups
C-              NDIMG - array of number of words per object for all groups
C-              XTAGS - array of tags
C-              XGRP  - array of group names
C-   Controls:
C-
C-   Created   6-APR-1993   Balamurali V
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZUTAG.LINK'
      INTEGER IGRP,NGRP,IDMAX,NDIMG(IGRP)
      INTEGER LUTAG,LUDST,LANLS,I,J,LEN,LENOCC,POINTER,IVERSION
      CHARACTER*8   CTAG,XTAGS(NGRP,IGRP)
      CHARACTER*4   XGRP(IGRP),CWORD
      REAL    RWORD
      EQUIVALENCE(RWORD,CWORD)
C----------------------------------------------------------------------
C
C Get Link of UTAG bank
C
      IF(LHEADR.EQ.0)THEN       ! if there is no run division
        LANLS = LQ(LHEAD-11)    ! look for UTAG bank in event record
      ELSE 
        LANLS = LQ(LHEADR-11)   ! else get it from run division
      ENDIF
      LUTAG = LQ(LANLS-IZUTAG)
      IF(LUTAG.LE.0)CALL ERRMSG('no UTAG bank','READ_UTAG_BANK',' ','F')
      IVERSION=IQ(LUTAG+1)                  ! Bank version
C
C Determine IDMAX, NobjMAX....
C
      IDMAX = IQ(LUTAG+2)
C
C Determine NDIMG(i)
C
      POINTER = LUTAG+2
      DO I = 1, IDMAX
        POINTER = POINTER + 1
        NDIMG(I) = IQ (POINTER)
      ENDDO
C
C Now assign proper TAGS
C
      POINTER = POINTER+1
      DO J = 1,IDMAX
        RWORD = Q(POINTER)
        XGRP(J) = CWORD
        POINTER = POINTER + 1
      ENDDO
      DO I = 1, IDMAX
        DO J = 1, NDIMG(I)
          CALL GET_CHAR(POINTER,CTAG)
          POINTER = POINTER+2
          LEN  = LENOCC(CTAG)
          XTAGS(J,I)=CTAG(1:LEN)
        ENDDO
      ENDDO
C
  999 RETURN
      END
