      SUBROUTINE GDCOLE(COLOR)
C *****************************************************************
C *
C *  GOAL: Set the graphic foreground color for the Envision
C *       terminal.
C *
C *      Limitation : does not neceesarely work on a capture file
C *
C *  INPUT ARGUMENTS :      COLOR : a 12 character string. recognised
C *      color are :
C *      'black', 'red', 'green', 'yellow', 'blue', 'magenta',
C *       'cyan', 'white', 'dark-gray', 'pink', 'brown', 'orange',
C *      'violet', 'peach', 'dark-red', 'dark-puprle'
C *
C *  OUTPUT ARGUMENTS :
C *
C *  ORIGIN :--
C *
C *  DATE : OCTOBER 1985
C *
C *
C *****************************************************************
C
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
C
      CHARACTER*12 COLOR
      CHARACTER*1 C
      C='N'
      IF(COLOR(1:5).EQ.'BLACK') THEN
        C='0'
      ELSE IF(COLOR(1:3).EQ.'RED') THEN
        C='1'
      ELSE IF(COLOR(1:5).EQ.'GREEN') THEN
        C='2'
      ELSE IF(COLOR(1:6).EQ.'YELLOW') THEN
        C='3'
      ELSE IF(COLOR(1:4).EQ.'BLUE') THEN
        C='4'
      ELSE IF(COLOR(1:7).EQ.'MAGENTA') THEN
        C='5'
      ELSE IF(COLOR(1:4).EQ.'CYAN') THEN
        C='6'
      ELSE IF(COLOR(1:5).EQ.'WHITE') THEN
        C='7'
      ELSE IF(COLOR(1:9).EQ.'DARK-GRAY') THEN
        C='8'
      ELSE IF(COLOR(1:4).EQ.'PINK') THEN
        C='9'
      ELSE IF(COLOR(1:5).EQ.'BROWN') THEN
        C=':'
      ELSE IF(COLOR(1:6).EQ.'ORANGE') THEN
        C=';'
      ELSE IF(COLOR(1:6).EQ.'VIOLET') THEN
        C='<'
      ELSE IF(COLOR(1:5).EQ.'PEACH') THEN
        C='='
      ELSE IF(COLOR(1:8).EQ.'DARK-RED') THEN
        C='>'
      ELSE IF(COLOR(1:11).EQ.'DARK-PURPLE') THEN
        C='?'
      END IF
      IF(C.EQ.'N') GOTO 99
      WRITE (LOUT,9001) CHAR(27),C
 9001 FORMAT(1X,A1,'C',A1)
C
   99 CONTINUE
      END
