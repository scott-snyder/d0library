      SUBROUTINE UDST_BOOK_GROUP(ID,GRPTAG,TAGS,NDIM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : book an object group
C-
C-      ENTRY UDST_FILL_GROUP(IDX,X,NDIMX,NOBJ)
C-
C-   Purpose and Methods : fill an object group
C-
C-      ENTRY UDST_WRITE_EVENT
C-
C-   Purpose and Methods : fill UTAG and UDST Banks
C-
C-   Created  16-OCT-1992   Ulrich Heintz
C-   Updated  14-AUG-1993   Ulrich Heintz - add group tags
C-   Updated  15-MAY-1994   Ulrich Heintz - move call to UDST_OUT to UDST.FOR 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:UDST_DIMENSIONS.PARAMS'
      INTEGER I,ID,NDIM,IDX,NDIMX,IDMAX,J,NOBJ,LUTAG,NDIMG(IGRP)
      INTEGER NWORD(IGRP)
      REAL    X(NDIMX),XDATA(IGRP,NGRP),RGRP(IGRP)
      CHARACTER*8 TAGS(NDIM),XTAGS(IGRP,NGRP)
      CHARACTER*4 GRPTAG,MSG*5,XGRP(IGRP)
      DATA XGRP/IGRP*' '/
      EQUIVALENCE(RGRP,XGRP)
C----------------------------------------------------------------------
C
C... check that ID is within range
      IF(ID.GT.IGRP)THEN
        WRITE(MSG,2)ID
        CALL ERRMSG('ID>IGRP','UDST_BOOK_GROUP',
     &    'ID='//MSG//' exceeds dimension','W')
        GOTO 999
      ENDIF
C
C... store group tag and check if group ID has already been used
      IF(XGRP(ID).EQ.' ')THEN
        XGRP(ID)=GRPTAG
      ELSE
        WRITE(MSG,2)ID
        CALL ERRMSG('ID_USED','UDST_BOOK_GROUP',' ID = '//MSG,'W')
        GOTO 999
      ENDIF
C
C... check dimension of tag array
      IF(NDIM.GT.NGRP)THEN
        WRITE(MSG,2)NDIM
        CALL ERRMSG('NDIM>NGRP','UDST_BOOK_GROUP',' NDIM = '//MSG,'W')
        NDIM=NGRP
      ENDIF
C
      DO I=1,NDIM
        XTAGS(ID,I)=TAGS(I)
      ENDDO
      IF(ID.GT.IDMAX)IDMAX=ID
      NDIMG(ID)=NDIM
C
  999 RETURN
C
C
      ENTRY UDST_FILL_GROUP(IDX,X,NDIMX,NOBJ)
C
C... check validity of ID
      IF(XGRP(IDX).EQ.' ')THEN
        WRITE(MSG,2)IDX
        CALL ERRMSG('ID_NOT_BOOKED','UDST_FILL_GROUP',' ID = '//MSG,'W')
        GOTO 998
      ENDIF
C
C... check size of data array
      IF(NOBJ.GT.NGRP/NDIMG(IDX))THEN
        WRITE(MSG,2)NOBJ
    2   FORMAT(I5)
        CALL ERRMSG('NOBJ>NGRP/NDIMG','UDST_FILL_GROUP',MSG//
     &    ' '//XGRP(IDX)//' objects','W')
        NOBJ=NGRP/NDIMG(IDX)
      ENDIF
C
      DO I=1,NOBJ*NDIMG(IDX)
        XDATA(IDX,I)=X(I)
      ENDDO
      NWORD(IDX)=NOBJ*NDIMG(IDX)
C
  998 RETURN
C
C
      ENTRY UDST_WRITE_EVENT
C
C fill UTAG bank
C
      CALL UTAGFL(IDMAX,NDIMG,XTAGS,RGRP,IGRP,NGRP,LUTAG)
C
C Fill UDST Bank
C
      CALL UDSTFL(IDMAX,NWORD,XDATA,IGRP,NGRP)
C
      RETURN
      END
