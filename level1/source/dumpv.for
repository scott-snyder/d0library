      SUBROUTINE DUMPV (VECT,NLIN,NCOL,STRING,ZERO_SUP,IFIRST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : dump a generic vector INTEGER in a sensible way 
C-                         Primary intended to print vectors 0/1 so for each
C-                         non zero element it will printout the column
C-   Inputs  : VECT/NLIN/NCOL vector to be dumped and its dimensions
C-             STRING Title
C_             ZERO_SUP flag to skip printing all the vector is null
C_             FIRST    First element to be printed 
C-   Outputs : 
C-   Controls: 
C-
C-   Original  Guilherme Lima
C-   Created  16-FEB-1994   Jussara M. de Miranda 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER VECT(*),NLIN,NCOL,IFIRST
      LOGICAL ZERO_SUP,PRFLG
      CHARACTER*(*) STRING
      INTEGER ILIN,ICOL,IND
      INTEGER DCC(40)
C----------------------------------------------------------------------

      PRFLG=.FALSE.
      IF(.NOT.ZERO_SUP) THEN
        PRFLG=.TRUE.
        GOTO 10
      ENDIF

C.. Check whether there is at least one bit set
      DO ILIN=1,NLIN
        DO ICOL=1,NCOL
          IND=NCOL*(ILIN-1)+ICOL
          IF(VECT(IND).NE.0) THEN
            PRFLG=.TRUE.
            GOTO 10
          ENDIF
        ENDDO
      ENDDO
      IF(ZERO_SUP .AND. .NOT.PRFLG) RETURN

C.. Go dump the array
   10 DO ILIN=1,NLIN
        DO ICOL=1,NCOL
          IND=NCOL*(ILIN-1)+ICOL
          IF(VECT(IND).EQ.0) THEN
            DCC(ICOL)=0
          ELSE
            DCC(ICOL)=IFIRST+ICOL-1
            IF(DCC(ICOL).EQ.0) DCC(ICOL)=99
          ENDIF
        ENDDO
        PRINT 900,STRING,(DCC(ICOL),ICOL=1,NCOL)
  900   FORMAT(1X,A15,2X,40I3)
      ENDDO

  999 RETURN
      END
