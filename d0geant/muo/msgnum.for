      SUBROUTINE MSGNUM(HNAME,ANAME,NUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine decodes a module/plane/cell
C    name in hollerith to character name and integer number. 
C-
C-   Inputs  : 
C-      HNAME     I     Hollerith name, 'AMxx','APxx','ACxx'
C-                                      'BMxx','BPxx','BCxx'
C-                                      'CMxx','CPxx','CCxx'
C-                                      
C-   Outputs : 
C-      ANAME     A4    Character name, 'AMxx' etc
C-      NUM       I     Integer number for module/plane/cell.
C-   Controls: 
C-
C-   Created  27-MAR-1990   KUNORI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER HNAME,NUM
      CHARACTER*4 ANAME
      INTEGER I,J,K(4)
      CHARACTER*1 B
      CHARACTER*4 A(11)
      DATA A/'0','1','2','3','4','5','6','7','8','9','A'/
C----------------------------------------------------------------------
C
      CALL UHTOC(HNAME,4,ANAME,4)
      NUM=0
C
      B=ANAME(1:1)
      IF(B.NE.'A'.AND.B.NE.'B'.AND.B.NE.'C') THEN
         GO TO 999
      ENDIF
C
      DO 100 I=3,4
        K(I)=-100
        DO 110 J=1,11
          IF(ANAME(I:I).EQ.A(J)) THEN
             K(I)=J-1
             GO TO 111
          ENDIF
  110   CONTINUE
  111   CONTINUE
  100 CONTINUE
C
      IF(K(3).GE.0.AND.K(4).GE.0) THEN
        NUM=K(3)*10+K(4)
        IF(ANAME(1:1).EQ.'B') THEN
           NUM=NUM+100
        ELSE IF(ANAME(1:1).EQ.'C') THEN
           NUM=NUM+200
        ENDIF
      ELSE
        NUM=0
      ENDIF
C
  999 RETURN
      END
