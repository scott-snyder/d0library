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
C-   Updated  13-Jan-1996   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:UDST_DIMENSIONS.PARAMS'
      INTEGER I,ID,NDIM,IDX,NDIMX,IDMAX,J,NOBJ,LUTAG,NDIMG(IGRP)
      INTEGER NWORD(IGRP)
      REAL    X(NDIMX),XDATA(IGRP,NGRP)
      integer ixGRP(IGRP)
      CHARACTER*8 TAGS(NDIM),XTAGS(IGRP,NGRP)
      CHARACTER*4 GRPTAG,MSG*5, grptmp
      DATA iXGRP/IGRP*0/
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
      IF(iXGRP(ID).EQ.0)THEN
        call uctoh (grptag, ixgrp, 4, 4)
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
      IF(iXGRP(IDX).EQ.0)THEN
        WRITE(MSG,2)IDX
        CALL ERRMSG('ID_NOT_BOOKED','UDST_FILL_GROUP',' ID = '//MSG,'W')
        GOTO 998
      ENDIF
C
C... check size of data array
      IF(NOBJ.GT.NGRP/NDIMG(IDX))THEN
        WRITE(MSG,2)NOBJ
    2   FORMAT(I5)
        call uhtoc (ixgrp(idx), 4, grptmp, 4)
        CALL ERRMSG('NOBJ>NGRP/NDIMG','UDST_FILL_GROUP',MSG//
     &    ' '//grptmp//' objects','W')
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
      CALL UTAGFL(IDMAX,NDIMG,XTAGS,ixGRP,IGRP,NGRP,LUTAG)
C
C Fill UDST Bank
C
      CALL UDSTFL(IDMAX,NWORD,XDATA,IGRP,NGRP)
C
      RETURN
      END
