      SUBROUTINE LSTYL( DRVNAM, ILTAB )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Creates a table with 10 different line styles.
C-                   Using a GPV driver will give 10 different line  styles
C-                   After the 10 differen line styles it will repeat the first
C-                   seven line styles again.
C-                   Using a SEIKO will give 6 different styles repeating 
C-                   twice the first four styles to fill the size 10 array.
C-                   Using s LN3 driver will give 5 different lines styles
C-                   the line styles will repeate themselves three times to
C-                   fill the table size 17 array.
C-                   Using a Tektronic window will give 9 different styles 
C-                   repeating them over to fill the table size 17.
C-                     
C-
C-   Inputs  : DRVNAM - Driver's name use in device 
C-   Outputs : ILTAB  - line style table with 10 different line styles
C-
C-   Created  21-APR-1989   LUPE ROSAS
C-   Updated   4-APR-1990   Lupe Howell  XDW driver  
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ILTAB(17), I, J, LNUM, DSPDEV, SLSTYL(17), TEKSTY(17)
      CHARACTER*3 DRVNAM
      DATA LNUM/17/
      DATA DSPDEV/1/
      DATA SLSTYL/ 0, 1, 2, 3, 5, 6, 1, 2, 3, 5, 6, 1, 6, 5, 3, 2, 0/
      DATA TEKSTY/ 0, 1, 2, 3, 5, 6, 7, 9,10, 11,1, 2, 3, 5, 6, 1, 0/
C----------------------------------------------------------------------
      J = 0
       IF (( DRVNAM .EQ. 'GPV' ).OR.(DRVNAM.EQ.'XDW')) THEN ! Workstation
        DO 10 I=1, (LNUM-1)
          ILTAB(I) = J
          J = J + 1
   10   CONTINUE
        ILTAB(LNUM) = 0
        ILTAB(16) = 3
      ELSEIF( DRVNAM.EQ.'LN3' )THEN ! LN03
        DO 20 I=1,9, 2
          ILTAB(I) = J
          ILTAB(I+1) = J
          J = J + 1
   20   CONTINUE
      ELSEIF( DRVNAM.EQ.'S04' )THEN
        DO 30 I=1, LNUM    ! Seiko (6 different lstyl)
          ILTAB(I) = SLSTYL(I)
   30   CONTINUE
      ELSE                 ! Tektronic window (9 different styll)
        DO 40 I=1, LNUM
          ILTAB(I) = TEKSTY(I)
   40   CONTINUE
      ENDIF
  999 RETURN
      END
