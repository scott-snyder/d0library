      PROGRAM TESTEZFILL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Test EZFILL and other features of the SRCP
C-                         package.
C-
C-   Inputs  : SRCP          DEFINEd to be TESTEZFILL.DAT
C-
C-   Outputs : FOR002.DAT   (See also TESTDUMP.DAT)
C-
C-   Controls: None
C-
C-   Created  14-NOV-1988   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL EZCHEK,FOUND
      INTEGER      I,J,K,L,M,N,NPAR,IOPT,ID,ITYPE,SIZE
C
      INTEGER     IARRAY(10)
      LOGICAL     LARRAY(10)
      REAL        RARRAY(10)
      CHARACTER*4 CARRAY(10),TYPE(10)
      EQUIVALENCE (IARRAY,LARRAY,RARRAY,CARRAY)
C
      INTEGER      LUN,LUNIN,LUNOUT,LUNOT,WRDIDS,IER
      PARAMETER( LUN    =  2 )
      PARAMETER( LUNIN  = 10 )
      PARAMETER( LUNOUT = 20 )
      PARAMETER( LUNOT  = 30 )
      PARAMETER( WRDIDS = 20 )
      CHARACTER*32 PAR(100),EZHDRC,NAME,TIME
C
      REAL    ARRAY(22),REAL1,REAL2,REAL3
      INTEGER INTEGER1,INTEGER2,INTEGER3,COUNT
      CHARACTER*4  BONG
      CHARACTER*32 A_LONG_STRING
      CHARACTER*20 A_COUNTED_STRING
      LOGICAL TRUE
      EQUIVALENCE (ARRAY(1) ,INTEGER1)
      EQUIVALENCE (ARRAY(2) ,REAL1)
      EQUIVALENCE (ARRAY(3) ,INTEGER2)
      EQUIVALENCE (ARRAY(4) ,REAL2)
      EQUIVALENCE (ARRAY(5) ,A_LONG_STRING)
      EQUIVALENCE (ARRAY(13),BONG)
      EQUIVALENCE (ARRAY(14),COUNT)
      EQUIVALENCE (ARRAY(15),A_COUNTED_STRING)
      EQUIVALENCE (ARRAY(20),TRUE)
      EQUIVALENCE (ARRAY(21),REAL3)
      EQUIVALENCE (ARRAY(22),INTEGER3)
C----------------------------------------------------------------------
C
C ****  Initialize ZEBRA
C
      CALL MZEBRA (0)
C
C ****  Initialize STP structure
C
      CALL INZSTP
C
C ****  Read parameter file and create and SRCP bank called
C ****  EZTEST
C
      PRINT*,'Testing EZREAD..'
      OPEN (UNIT=LUNIN,FILE='SRCP',STATUS='OLD')
      CALL EZREAD (LUNIN,'EZTEST',WRDIDS,0,0)
      CLOSE (UNIT=LUNIN)
C
C ****  Add simple parameters
C
      PRINT*,'Testing EZFILL...'
      CALL EZFILL ('INTEGER','This is of type integer',111,'I',1)
      CALL EZFILL ('BOOLEAN','This is of type boolen',.TRUE.,'L',1)
      CALL EZFILL ('REAL','This is of type real',222.222,'R',1)
      CALL EZFILL
     &  ('HOLLERITH','This is of type hollerith','ZORO','H',1)
C
C ****  Add an array of mixed type
C
      IARRAY(1) = 11
      LARRAY(2) = .TRUE.
      RARRAY(3) = 1.234E10
      CARRAY(4) = 'BONG'
      TYPE(1)   = 'I'
      TYPE(2)   = 'L'
      TYPE(3)   = 'R'
      TYPE(4) = 'H'
C
      CALL EZFILL
     &  ('MIXED_R','This is of type array structure',RARRAY,TYPE,4)
C
      CALL EZFILL
     &  ('MIXED_I','This is of type array structure',IARRAY,TYPE,4)

      DO I=1,8
        TYPE(I) = 'H'
      ENDDO
C
C ****  Pack string into real array
C
      CALL DCTOH (32,'TheTimeHasComeTheWalrus             ',RARRAY(1))
      CALL EZFILL
     &  ('STRING','This is a string',RARRAY,TYPE,8)

      PRINT*,'Testing EZEND...'
      CALL EZEND
C
      PRINT*,'Testing EZDUMP...'
      CALL EZDUMP (6,0,2)               ! List identifiers only
      CALL EZDUMP (LUN,0,2)
      CALL EZDUMP (6,0,0)               ! Dump SRCP in original order
      CALL EZDUMP (LUN,0,0)
      WRITE(LUN,*) '  '
C
      OPEN (UNIT=LUNOUT,FILE='TESTDUMP.DAT',STATUS='NEW')
      CALL EZDUMP (LUNOUT,0,0)
      CLOSE (UNIT=LUNOUT)
C
      NAME = EZHDRC ('EZTEST','NAME')
      TIME = EZHDRC ('EZTEST','TIME')
      WRITE(6,190) NAME,TIME
      WRITE(LUN,190) NAME,TIME
  190 FORMAT(1X,'BANK NAME: ',A32,/,
     &         1X,'DATE     : ',A32/)
C
C ****  Get list of identifiers from file DUMP
C
      OPEN(UNIT=LUNOUT,FILE='TESTDUMP.DAT',STATUS='OLD')
      CALL EZPAR  (LUNOUT,'ALL',PAR,NPAR)
      CLOSE(UNIT=LUNOUT)
C
      DO I =  1,NPAR
        FOUND = EZCHEK (PAR(I))         ! Check if identifier present
C
        IF ( FOUND ) THEN
C
          CALL EZGETA(PAR(I),0,0,0,N,IER)       ! Get parameter size
          CALL EZGETI(PAR(I),ID,IER)            ! Get parameter index
          CALL EZGETT(ID,NAME,L,ITYPE,SIZE)     ! Get parameter type
C
          WRITE(6,200) I,PAR(I),N,SIZE,ITYPE
          WRITE(LUN,200) I,PAR(I),N,SIZE,ITYPE
  200     FORMAT(1X,I5,5X,A32,3I5,'  FOUND')
C
        ELSE
          WRITE(LUN,210) I,PAR(I)
          WRITE(6,210) I,PAR(I)
  210     FORMAT(1X,I5,5X,A32,' ******* NOT FOUND')
        ENDIF
      ENDDO
C
      PRINT*,'Testing EZGSET...'
      CALL EZGSET ('A_MIXED_ARRAY',ARRAY,1)
C
      WRITE(6,*) '  '
      WRITE(6,*) 'Contents of array: A_MIXED_ARRAY'
      WRITE(6,*) 'Integer         : ',INTEGER1
      WRITE(6,*) 'Real            : ',REAL1
      WRITE(6,*) 'Integer         : ',INTEGER2
      WRITE(6,*) 'Real            : ',REAL2
      WRITE(6,*) 'Long-string     : ','/'//A_LONG_STRING//'/'
      WRITE(6,*) 'Short-string    : ','/'//BONG//'/'
      WRITE(6,*) 'Counted string'
      WRITE(6,*) '        Count   : ',COUNT
      WRITE(6,*) '        String  : ',
     &  '/'//A_COUNTED_STRING(1:COUNT)//'/'
      WRITE(6,*) 'Boolean         : ',TRUE
      WRITE(6,*) 'Real            : ',REAL3
      WRITE(6,*) 'Integer         : ',INTEGER3
C
      WRITE(LUN,*) '  '
      WRITE(LUN,*) 'Contents of array: A_MIXED_ARRAY'
      WRITE(LUN,*) 'Integer         : ',INTEGER1
      WRITE(LUN,*) 'Real            : ',REAL1
      WRITE(LUN,*) 'Integer         : ',INTEGER2
      WRITE(LUN,*) 'Real            : ',REAL2
      WRITE(LUN,*) 'Long-string     : ','/'//A_LONG_STRING//'/'
      WRITE(LUN,*) 'Short-string    : ','/'//BONG//'/'
      WRITE(LUN,*) 'Counted string'
      WRITE(LUN,*) '        Count   : ',COUNT
      WRITE(LUN,*) '        String  : ',
     &  '/'//A_COUNTED_STRING(1:COUNT)//'/'
      WRITE(LUN,*) 'Boolean         : ',TRUE
      WRITE(LUN,*) 'Real            : ',REAL3
      WRITE(LUN,*) 'Integer         : ',INTEGER3
C
      END
