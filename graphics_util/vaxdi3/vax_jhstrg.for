      SUBROUTINE JHSTRG(STRING)
C
      CHARACTER*(*)STRING
      INTEGER MAXLEN
      PARAMETER (MAXLEN=200)          !MAX STRING LENGTH
      CHARACTER*200 OUTSTR
      LOGICAL*1 OUTB(MAXLEN)
      EQUIVALENCE(OUTSTR,OUTB)
      INTEGER I,IIN,IOUT,L
      INCLUDE 'D0$INC:DI3INC.INC'
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
      I=INDEX(STRING,'[FONT]')
      IF(I.EQ.0)THEN
        IF(IFONT.EQ.1)THEN
          CALL J3STRG(STRING)     !COMPLETELY NORMAL PRINT
          RETURN
        ENDIF
      ENDIF
C ABNORMAL PRINT...COPY CHAR BY CHAR & CONVERT
      L=LEN(STRING)                   !GET LENGTH
      IF(L.GT.MAXLEN)L=MAXLEN         !MAXIMUM ALLOWED
      IOUT=0                          !OUTPUT COUNT
      IIN=1                           !INPUT POINTER
C LOOP TO DEAL WITH ABNORMAL STRING
  100 IF(STRING(IIN:IIN).EQ.'[')THEN  !START OF ESCAPE SEQUENCE?
        IF(INDEX(STRING(IIN:L),'[FONT]').EQ.1)THEN   !END-OF-CRAZY FONT?
          IFONT=IFONDF                !BACK TO THE DEFAULT FONT
          IIN=IIN+6                   !SKIP IN INPUT BUFFER
          GOTO 200                    !JUMP AHEAD
        ELSEIF(INDEX(STRING(IIN:L),'[FONT=9]').EQ.1)THEN  !SWITCH TO FONT 9
          IFONT=9                     !SWITCH FONT
          IIN=IIN+8                   !SKIP IN INPUT BUFFER
          GOTO 200                    !JUMP AHEAD
        ELSEIF(INDEX(STRING(IIN:L),'[FONT=23]').EQ.1)THEN  !SWITCH TO FONT 23
          IFONT=23                    !SWITCH FONT
          IIN=IIN+9                   !SKIP IN INPUT BUFFER
          GOTO 200                    !JUMP AHEAD
        ENDIF
      ENDIF
C NO FONT SWITCH...JUST TRANSLATE THE CHAR
      IOUT=IOUT+1                     !BUMP OUTPUT POINTER
      IF(IFONT.EQ.9)THEN !ARE WE IN FONT 9
        IF(STRING(IIN:IIN).EQ.'q')THEN  !PI
          OUTB(IOUT)=-125
        ELSEIF(STRING(IIN:IIN).EQ.'h')THEN      !theta
          OUTB(IOUT)=-124
        ELSEIF(STRING(IIN:IIN).EQ.'v')THEN      !PHI
          OUTB(IOUT)=-123
        ELSE                            !unknown..just copy
          OUTSTR(IOUT:IOUT)=STRING(IIN:IIN)
        ENDIF
C    23,e   left arrow          128 = -128
C    23,f       right arrow     129 = -127
C    23,V   arrow shaft         130 = -126
      ELSEIF(IFONT.EQ.23)THEN        !ARE WE IN FONT 23
        IF(STRING(IIN:IIN).EQ.'e')THEN          !left arrow
          OUTB(IOUT)=-128
        ELSEIF(STRING(IIN:IIN).EQ.'f')THEN      !right arrow
          OUTB(IOUT)=-127
        ELSEIF(STRING(IIN:IIN).EQ.'V')THEN      !arrow shaft
          OUTB(IOUT)=-126
        ELSE                            !unknown..just copy
          OUTSTR(IOUT:IOUT)=STRING(IIN:IIN)
        ENDIF
      ELSE                            !NORMAL FONT
        OUTSTR(IOUT:IOUT)=STRING(IIN:IIN)
      ENDIF
      IIN=IIN+1                               !BUMP INPUT POINTER
C LOOP BACK UNLESS PAST END OF INPUT STRING
  200 IF(IIN.LE.L)GOTO 100
      IF(IOUT.GT.0)CALL J3STRG(OUTSTR(1:IOUT))        !PRINT STRING
      END
