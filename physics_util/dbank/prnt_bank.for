      SUBROUTINE PRNT_BANK(TCHR,VECT,CHF)

C----------------------------------------------------------------------
C-
C-   Purpose and Methods : OPEN file with name TCHR and PRINT bank to it.
C-
C-   Inputs  : TCHR file name
C-             VECT array containing Bank
C-             CHF format for bank
C-   Outputs :
C-   Controls:
C-
C-   Created   5-JUL-1989   Rajendran Raja
C-   Updated  20-AUG-1992   sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TCHR,CHF
      REAL    VECT(*)
C----------------------------------------------------------------------
      INTEGER I,IFRST,LEN3,ILAST
      INTEGER II
C
      CHARACTER*60 FMT,AUTO_FMT,FMT1,FMT2
      INTEGER IDEL,IMAX,INUM
      PARAMETER( IDEL = 26 )            ! NUMBER OF COLUMNS PER ELEMENT
      PARAMETER( IMAX = 80 )            ! MAXIMUM NUMBER OF COULMNS PER LINE
      PARAMETER( INUM = IMAX/IDEL )
      CHARACTER*(IDEL) LINE
      CHARACTER*(IMAX) LINEB
      INTEGER ICUR                      ! CURRENT LINE POINTER
C
      INCLUDE 'D0$INC:AUTOF.INC'
C
      CHARACTER*6 ZFLG(-8:0)            ! ZEBRA LINK AREA DEFNS
      DATA ZFLG/'NXL LQ','UPL LQ','ORG LQ','IDN IQ','IDH IQ',
     &  'NL  IQ','NS  IQ','ND  IQ','STA IQ'/
      CHARACTER*6 ZFLG1,ZFLG2,ZFLG3
      DATA ZFLG1/'DTA IQ'/,ZFLG2/'STR LQ'/,ZFLG3/'REF LQ'/
C
      REAL QNL,QNS,QND
      INTEGER NL,NS,ND
      EQUIVALENCE (QNL,NL),(QNS,NS),(QND,ND)
C
      INTEGER IUN,IERR
      LOGICAL OK
C&IF IBMAIX
C&      integer vec1(3)
C&      pointer (ptvec1, vec1)
C&ENDIF
C----------------------------------------------------------------------
      ICUR = 1
      LINEB = ' '
C
      CALL GTUNIT(999,IUN,IERR)
      CALL D0OPEN(IUN,TCHR,'FO',OK)
C
C
C ****  Write header
C
      WRITE(IUN,1)
    1 FORMAT(/' *********DBANK Dump of Bank **********'//)
C
C&IF IBMAIX   ! wimpy ibm compiler complains about out-of-bounds indices...
C&      ptvec1 = loc (vect(1)) - 4*(loc (vect(2)) - loc (vect(1)))
C&      nl = vec1(1)
C&      ns = vec1(2)
C&      nd = vec1(3)
C&ELSE
      QNL = VECT(-3)                    ! Number of links
      QNS = VECT(-2)                ! Number of structural links
      QND = VECT(-1)                    ! Number of data words.
C&ENDIF
      IFRST = -NL - 8
      ILAST = ND
      DO 100 I=IFRST,ILAST
        LINE=' '
        FMT = ' '
        FMT2 = ' '
        IF(I.LE.0)THEN                  ! BANK LINK AREA. Special
          II = I
          IF(I.LT.-5)II=II+8            ! in LQ space
          IF(I.GE.-8.AND.I.NE.-4)THEN               ! Fixed link area
            WRITE(LINE,'(A6,I5,I10)',ERR=81)ZFLG(I),II,VECT(I)
          ELSEIF(I.EQ.-4)THEN           ! Hollerith data
            WRITE(LINE,'(A6,I5,6X,A4)',ERR=81)ZFLG(I),II,VECT(I)
          ELSEIF(II.GE.-NS)THEN         ! Structural link area
            WRITE(LINE,'(A6,I5,I10)',ERR=81)ZFLG2,II,VECT(I)
          ELSEIF(II.LT.-NS.AND.II.GE.-NL)THEN         ! Reference link area
            WRITE(LINE,'(A6,I5,I10)',ERR=81)ZFLG3,II,VECT(I)
          ENDIF
        ELSE
          IF(CHF.EQ.'AUTO'.OR.CHF.EQ.'auto')THEN
            IDATA = I
            FMT = AUTO_FMT()
            FMT1 = '(A6,'//FMT
            CALL ADDSTR(FMT1,')',FMT2,LEN3)
          ELSE
            FMT = CHF
            FMT1 = '(A6,'//FMT
            CALL ADDSTR(FMT1,')',FMT2,LEN3)
          ENDIF
          WRITE(LINE,FMT2(1:LEN3),ERR=81) ZFLG1,I,VECT(I)
        ENDIF
   81   CONTINUE
   80   CONTINUE
C WRITE LINE OUT HERE
        LINEB(ICUR:ICUR+IDEL-1) = LINE(1:IDEL)
        ICUR = ICUR + IDEL
        IF(ICUR .GT. INUM*IDEL)THEN
          ICUR = 1
          WRITE(IUN,82)LINEB
          LINEB = ' '
        ENDIF
   82   FORMAT(A)
  100 CONTINUE
      IF(ICUR.NE.1)WRITE(IUN,82)LINEB
      CLOSE(IUN)
      CALL RLUNIT(999,IUN,IERR)
  999 RETURN
      END
