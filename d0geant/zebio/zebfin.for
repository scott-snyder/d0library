      SUBROUTINE ZEBFIN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finish up Zebcom, write End-of-Run Record and
C-                         close
C-
C-   Inputs  : /ZEBIO/, /ZEBCOM/
C-   Outputs : NONE
C-
C-   Created   8-JUL-1987   A.M.Jonckkhere
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZEBIO.INC'
C
      INTEGER NIO,ISTATS,I
      INTEGER CHGEAN,CHEND
C----------------------------------------------------------------------
C
        CALL UCTOH('GEAN',CHGEAN,4,4)
        CALL UCTOH('END ',CHEND,4,4)
C
C --write out end run record...
        IF(IWRUNI.NE.0) THEN    
C     --check if existing bank is end run record.
          IF(LHEAD.NE.0) THEN
            IF(IQ(LHEAD+1).NE.1002) THEN    ! ????? this may be wrong ?????
              CALL MZWIPE(IXCOM+21)
            ENDIF
          ENDIF
C     --create fake end run record.
          IF(LHEAD.EQ.0) THEN                                      
            CALL MZFORM('HEAD','1I 2H 19I',NIO)
            CALL MZBOOK(IXMAIN,LHEAD,LHEAD,1,'HEAD',18,18,22,NIO,0)
            IQ(LHEAD+1)=1002
            IQ(LHEAD+2)=CHGEAN
            IQ(LHEAD+3)=CHEND
            DO I = 4,22
              IQ(LHEAD+I)=0
            ENDDO
          ENDIF
C --write out begin run record...
          IQ(LHEAD+2)=CHGEAN
          CALL WRZEB(ISTATS)
        ENDIF
C
C --FZ end procedure...
        CALL ENDZEB
C
        END
