      CHARACTER*8 FUNCTION UDST_TAG(LUDST,INDEX,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : compute tag for a word in the UDST bank
C-
C-   Returned value  : tag
C-   Inputs  : LUDST - pointer to UDST bank
C-             INDEX - offset of word in UDST bank (Q(LUDST+index)
C-   Outputs : IER - error flag
C-                 = 0 if no error condition
C-                 = 1 if LUDST doesn't point to a UDST bank
C-                 = 2 if INDEX doesn't point to a data word in the UDST bank
C-
C-   Created  16-NOV-1994   Ulrich Heintz
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:UDST_DIMENSIONS.PARAMS'
      INCLUDE 'D0$INC:UTAG_DATA.INC'
      INCLUDE 'D0$INC:UTAG_VERSIONS.INC'
      INTEGER I,IER,OBJ,LUDST,INDEX,JGRP,IV
      CHARACTER*4 GRP
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      IER=0
      UDST_TAG=' '
      IF (LUDST.LE.0) THEN
        IER=1                      ! LUDST = 0
        GOTO 999
      ELSEIF (Q(LUDST-4).NE.4HUDST) THEN
        IER=1                      ! LUDST doesn't point to a UDST bank
        GOTO 999
      ENDIF
C
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL FILL_UTAG_VERSIONS    ! initialize arrays XGRPV,NDIMGV,XTAGSV
      ENDIF
C
      IV=IQ(LUDST+1)
C
      IF(INDEX.LE.IQ(LUDST+2)+2)THEN
        UDST_TAG=XGRPV(IV,INDEX-2)
      ELSEIF(INDEX.LE.IQ(LUDST-1))THEN
        I=IQ(LUDST+2)+2            ! number of book keeping words
        JGRP=0
        DO WHILE(I.LT.INDEX)
          JGRP=JGRP+1
          I=I+IQ(LUDST+2+JGRP)     ! increment by number of words in group JGRP
        ENDDO
        GRP=XGRPV(IV,JGRP)         ! group name
        I=INDEX-(I-IQ(LUDST+2+JGRP)) ! subtract # words in groups before JGRP
        OBJ=(I-1)/NDIMGV(IV,JGRP)+1    ! object number
        I=I-(OBJ-1)*NDIMGV(IV,JGRP)! tag number
        UDST_TAG=XTAGSV(IV,JGRP,I) ! tag name
      ELSE
        IER=2                      ! INDEX > # data words in UDST bank
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
