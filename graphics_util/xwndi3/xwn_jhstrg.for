      SUBROUTINE JHSTRG(STRING)
C
      CHARACTER*(*)STRING
c      INTEGER MAXLEN
c      PARAMETER (MAXLEN=200)          !MAX STRING LENGTH
c      CHARACTER*200 OUTSTR
c      LOGICAL*1 OUTB(MAXLEN)
c      EQUIVALENCE(OUTSTR,OUTB)
c      INTEGER I,IIN,IOUT,L
c      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
C CONVERT THE FOLLOWING SPECIAL CASES (FONT,CHAR) BEFORE PASSING TO J3STRG
C
C    23,e   left arrow          128 = -128
C    23,f       right arrow     129 = -127
C    23,V   arrow shaft         130 = -126
C    9,q    pi                  131 = -125
C    9,h    theta               132 = -124
C    9,v        phi             133 = -123
C
C FIRST CHECK FOR NORMAL FONT,NO FONT SWITCH
c      I=INDEX(STRING,'[FONT]')
c      IF(I.EQ.0)THEN
c        IF(IFONTC.EQ.1)THEN
C!!!          CALL J3STRG(STRING)     !COMPLETELY NORMAL PRINT
       CALL J_STRING(STRING)
c          RETURN
c        ENDIF
c      ENDIF
C ABNORMAL PRINT...COPY CHAR BY CHAR & CONVERT
c      L=LEN(STRING)                   !GET LENGTH
c      IF(L.GT.MAXLEN)L=MAXLEN         !MAXIMUM ALLOWED
c      IOUT=0                          !OUTPUT COUNT
c      IIN=1                           !INPUT POINTER
C LOOP TO DEAL WITH ABNORMAL STRING
c  100 IF(STRING(IIN:IIN).EQ.'[')THEN  !START OF ESCAPE SEQUENCE?
c        IF(INDEX(STRING(IIN:L),'[FONT]').EQ.1)THEN   !END-OF-CRAZY FONT?
c          IFONTC=IFONDF               !BACK TO THE DEFAULT FONT
c          IIN=IIN+6                   !SKIP IN INPUT BUFFER
c          GOTO 200                    !JUMP AHEAD
c        ELSEIF(INDEX(STRING(IIN:L),'[FONT=9]').EQ.1)THEN  !SWITCH TO FONT 9
c          IFONTC=9                    !SWITCH FONT
c          IIN=IIN+8                   !SKIP IN INPUT BUFFER
c          GOTO 200                    !JUMP AHEAD
c        ELSEIF(INDEX(STRING(IIN:L),'[FONT=23]').EQ.1)THEN  !SWITCH TO FONT 23
c          IFONTC=23                   !SWITCH FONT
c          IIN=IIN+9                   !SKIP IN INPUT BUFFER
c          GOTO 200                    !JUMP AHEAD
c        ENDIF
c      ENDIF
C NO FONT SWITCH...JUST TRANSLATE THE CHAR
c      IOUT=IOUT+1                     !BUMP OUTPUT POINTER
c      IF(IFONTC.EQ.9)THEN             !ARE WE IN FONT 9? (GREEK)
c        IF(STRING(IIN:IIN).EQ.'q')THEN  !PI
c          OUTB(IOUT)=-125
c        ELSEIF(STRING(IIN:IIN).EQ.'h')THEN      !theta
c          OUTB(IOUT)=-124
c        ELSEIF(STRING(IIN:IIN).EQ.'v')THEN      !PHI
c          OUTB(IOUT)=-123
c        ELSE                            !unknown..just copy
c          OUTSTR(IOUT:IOUT)=STRING(IIN:IIN)
c        ENDIF
C    23,e   left arrow          128 = -128
C    23,f       right arrow     129 = -127
C    23,V   arrow shaft         130 = -126
c      ELSEIF(IFONTC.EQ.23)THEN        !ARE WE IN FONT 23? (SYMBOLIC)
c        IF(STRING(IIN:IIN).EQ.'e')THEN          !left arrow
c          OUTB(IOUT)=-128
c        ELSEIF(STRING(IIN:IIN).EQ.'f')THEN      !right arrow
c          OUTB(IOUT)=-127
c        ELSEIF(STRING(IIN:IIN).EQ.'V')THEN      !arrow shaft
c          OUTB(IOUT)=-126
c        ELSE                            !unknown..just copy
c          OUTSTR(IOUT:IOUT)=STRING(IIN:IIN)
c        ENDIF
c      ELSE                            !NORMAL FONT
c        OUTSTR(IOUT:IOUT)=STRING(IIN:IIN)
c      ENDIF
c      IIN=IIN+1                               !BUMP INPUT POINTER
C LOOP BACK UNLESS PAST END OF INPUT STRING
c  200 IF(IIN.LE.L)GOTO 100
c      IF(IOUT.GT.0)CALL J3STRG(OUTSTR(1:IOUT))        !PRINT STRING
      END
