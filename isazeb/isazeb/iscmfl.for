      SUBROUTINE ISCMFL(CUNIT,N_ENDS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO FILL THE 
C-
C-   Inputs  : 
C-   CUNIT = unit number for command file
C-   Outputs :
C-   N_ENDS = number of END lines in file
C-
C-   Created  13-DEC-1989   Chip Stewart
C-   Updated  26-NOV-1991   Serban D. Protopopescu  (added N_ENDS) 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER MAXLIN
      PARAMETER( MAXLIN =  200 )
      CHARACTER LINE(MAXLIN)*80
      INTEGER NCHAR(MAXLIN)
      INTEGER LISCM,CUNIT,IER,N_ENDS
      INTEGER NLINE,NDATA,I1,I2,L1,IPT,ILINE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C ****  READ IN COMMAND FILE
C
      IF(FIRST.AND.CUNIT.GT.0) THEN
        REWIND(CUNIT)
        N_ENDS=0
        NLINE = 0
        NDATA = 0
    1   NLINE = NLINE + 1
        READ (CUNIT,'(A80)',END=2)LINE(NLINE)
        IF(LINE(NLINE)(1:3).EQ.'END') N_ENDS=N_ENDS+1
        CALL SWORDS(LINE(NLINE),I1,I2,L1)
        LINE(NLINE) = LINE(NLINE)(1:I2)//CHAR(13)
        NCHAR(NLINE) = I2 + 1
        NDATA = NDATA + ( NCHAR(NLINE) + 3 ) / 4
        IF( NLINE.LT.MAXLIN) GOTO 1                            
    2   CALL BKISCM(NDATA+1,LISCM)
        REWIND(CUNIT)      
        IPT = LISCM + 1
C
C ****  FILL ISCM WITH COMMAND FILE
C
        DO ILINE = 1, NLINE
          CALL UCTOH(LINE(ILINE)(1:NCHAR(ILINE)),IQ(IPT),4,
     &      NCHAR(ILINE)) 
          IPT = IPT + ( NCHAR(ILINE) + 3 ) / 4
        END DO
        LINE(1)(1:3)='   '
        DO WHILE (LINE(1)(1:3).NE.'END')
          READ (CUNIT,'(A80)',END=3) LINE(1)
        ENDDO
    3   FIRST=.FALSE.
      ENDIF
  999 RETURN
      END
