      SUBROUTINE PXCOLR(COLOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Interface routine that will call the color table
C-                       (PXCOLN) and perform the color operations.   This is
C-                       equivalent to call JCOLOR(NUM)
C-
C-   Inputs  : COLOR    [C*]    Color desired to use out of the color table.
C-                              Only the first three letters of the color are
C-                              significant.
C-
C-   Created  11-DEC-1989   LUPE ROSAS
C-   Updated  15-MAY-1991   Harrison B. Prosper  
C-      Change declaration of COLOR 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) COLOR
      CHARACTER*3  IDET
      INTEGER     ITYP, INDX, ENSCOL(17), KCOLOR,
     & KINTEN, KFILL, KSTYL, COLVAL(17)
      REAL        RLEVEL
      LOGICAL     CALFLG,CONVERT
      DATA        CALFLG /.TRUE./
      DATA        ITYP /1/
      DATA        IDET /'CDC'/
      DATA        ENSCOL/0, 0, 13, 31, 4, 15, 6, 2, 17, 11,
     &            5, 1, 14, 20, 16, 3, 0/
C----------------------------------------------------------------------
      INTEGER ICOLOR
      CHARACTER*3 CCOLOR
C----------------------------------------------------------------------
      CONVERT = .FALSE.
      GO TO 10
C
      ENTRY PXCOLCTOI(COLOR,ICOLOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given a color table index (character) it will return 
C-   the INTEGER index corresponding to it in the color table.
C-
C-   Inputs  : COLOR [C*]: Three letter character that represents the 
C-                         given integer. If the character index entered is 
C-                         not known IDX will be returned as 0.
C-   
C-   Outputs : ICOLOR [I]: Integer index of the color table. If the character 
C-                         index entered is not known IDX will be returned as 0.
C-
C-   Created   6-AUG-1991   Lupe Howell
C-   Modified  6-AUG-1991   Nobuaki Oshima ( Replace Argu. INDX by ICOLOR )
C-
C----------------------------------------------------------------------
      CONVERT = .TRUE.
      INDX = 0
   10 IF     (COLOR(1:3) .EQ. 'WHI') THEN   ! White
        INDX =  1
      ELSEIF (COLOR(1:3) .EQ. 'BLA') THEN   ! Black
        INDX =  2
      ELSEIF (COLOR(1:3) .EQ. 'DPU') THEN   ! Dark purple
        INDX =  3
      ELSEIF (COLOR(1:3) .EQ. 'PUR') THEN   ! Purple
        INDX =  4
      ELSEIF (COLOR(1:3) .EQ. 'DBL') THEN   ! Dark Blue
        INDX =  5
      ELSEIF (COLOR(1:3) .EQ. 'BLU') THEN   ! Blue
        INDX =  6
      ELSEIF (COLOR(1:3) .EQ. 'CYA') THEN   ! Cyan
        INDX =  7
      ELSEIF (COLOR(1:3) .EQ. 'DGR') THEN   ! Dark Green
        INDX =  8
      ELSEIF (COLOR(1:3) .EQ. 'GRE') THEN   ! Green
        INDX =  9
      ELSEIF (COLOR(1:3) .EQ. 'BGR') THEN   ! Blue Green
        INDX =  10
      ELSEIF (COLOR(1:3) .EQ. 'DMA') THEN   ! Dark Magenta
        INDX =  11
      ELSEIF (COLOR(1:3) .EQ. 'DRE') THEN   ! Dark Red
        INDX =  12
      ELSEIF (COLOR(1:3) .EQ. 'RED') THEN   ! Red
        INDX =  13
      ELSEIF (COLOR(1:3) .EQ. 'MAG') THEN   ! Magenta
        INDX =  14
      ELSEIF (COLOR(1:3) .EQ. 'ORA') THEN   ! Orange
        INDX =  15
      ELSEIF (COLOR(1:3) .EQ. 'YEL') THEN   ! Yellow
        INDX =  16
      ELSEIF (COLOR(1:3) .EQ. 'FOR') THEN   ! Foreground
        INDX =  17
      ELSE                             !  Bad input parameter
        GO TO 888 
      ENDIF
      IF( CONVERT ) THEN   ! If entry  PXCOLCTOI called exit with index
        ICOLOR = INDX
        GO TO 888
      ENDIF
C
      CALL JIQDIL(RLEVEL)
      IF ( RLEVEL.LT.0 ) THEN           ! Evens & Sutherland machine
        CALL JCOLOR(ENSCOL(INDX))
      ELSE
        CALL PXCOLN(IDET,INDX,ITYP,CALFLG,KCOLOR,KINTEN,KFILL,
     &    KSTYL)
      ENDIF
  888 RETURN 

      ENTRY PXCOLITOC(ICOLOR,CCOLOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given a color table index (integer) it will return 
C-   the three letter index corresponding to it.
C-
C-   Inputs  : ICOLOR [I]: Integer index of the color table
C-   
C-   Outputs : CCOLOR [C*]: Three letter character that represents the 
C-                         given integer. If the index entered is out of 
C-                         range  CCOLOR will be return as black
C-
C-   Created   5-AUG-1991   Lupe Howell
C-
C----------------------------------------------------------------------
      CCOLOR = ' '
      IF     ( ICOLOR .EQ. 1 ) THEN
        CCOLOR = 'WHI'
      ELSEIF( ICOLOR .EQ.  2 ) THEN
        CCOLOR = 'BLA'
      ELSEIF( ICOLOR .EQ.  3 ) THEN
        CCOLOR = 'DPU'
      ELSEIF( ICOLOR .EQ.  4 ) THEN
        CCOLOR = 'PUR'
      ELSEIF( ICOLOR .EQ.  5 ) THEN
        CCOLOR = 'DBL'
      ELSEIF( ICOLOR .EQ.  6 ) THEN
        CCOLOR = 'BLU'
      ELSEIF( ICOLOR .EQ.  7 ) THEN
        CCOLOR = 'CYA'
      ELSEIF( ICOLOR .EQ.  8 ) THEN
        CCOLOR = 'DGR'
      ELSEIF( ICOLOR .EQ.  9 ) THEN
        CCOLOR = 'GRE'
      ELSEIF( ICOLOR .EQ. 10 ) THEN
        CCOLOR = 'BGR'
      ELSEIF( ICOLOR .EQ. 11 ) THEN
        CCOLOR = 'DMA'
      ELSEIF( ICOLOR .EQ. 12 ) THEN
        CCOLOR = 'DRE'
      ELSEIF( ICOLOR .EQ. 13 ) THEN
        CCOLOR = 'RED'
      ELSEIF( ICOLOR .EQ. 14 ) THEN
        CCOLOR = 'MAG'
      ELSEIF( ICOLOR .EQ. 15 ) THEN
        CCOLOR = 'ORA'
      ELSEIF( ICOLOR .EQ. 16 ) THEN
        CCOLOR = 'YEL'
      ELSEIF( ICOLOR .EQ. 17 ) THEN
        CCOLOR = 'FOR'
      ENDIF
  999 RETURN
      END
