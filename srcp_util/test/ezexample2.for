      PROGRAM EZEXAMPLE2
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TEST of EZDROP etc.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  10-MAY-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IER,TOTAL,LENGTH
      CHARACTER*80 STRING
      CHARACTER*32 LIST(20)
      INTEGER NLIST,I,LUN,LUNOUT
      PARAMETER( LUN = 1 )
      PARAMETER( LUNOUT = 2 )
C----------------------------------------------------------------------
      OPEN (UNIT=LUN,FILE='TEST',STATUS='NEW'
C&IF VAXVMS,ULTRIX,SIUNIX
     &          ,CARRIAGECONTROL='LIST'
C&ENDIF
     &  )
C
      CALL MZEBRA(0)                    ! (1)  Initialize ZEBRA
C
      CALL INZSTP                       ! (2)  Setup /ZEBSTP/
C
      CALL INRCP('EZEXAMPLE2_RCP',IER)
C
      CALL INRCP_LIST(LIST,NLIST)
      WRITE(6,*) (LIST(I),I=1,NLIST)
      WRITE(LUN,*) ' ************************* '
      WRITE(LUN,*) (LIST(I),I=1,NLIST)
C
C ****  Open dump file
C
      WRITE(LUN,*) ' ************************* '
      CALL EZDUMP (LUN,0,0)  
      WRITE(LUN,*) ' ************************* '

      CALL EZPICK('TEST1')
      CALL EZGETS ('BONG',1,STRING,LENGTH,IER)   
      WRITE(6,*) '<'//STRING(1:LENGTH)//'>'
      WRITE(LUN,*) '<'//STRING(1:LENGTH)//'>'
C
      CALL EZPICK('TEST2')
      CALL EZGETS ('BONG',1,STRING,LENGTH,IER)   
      WRITE(6,*) '<'//STRING(1:LENGTH)//'>'
      WRITE(LUN,*) '<'//STRING(1:LENGTH)//'>'
C
      CALL EZPICK('TEST3')
      CALL EZGETS ('BONG',1,STRING,LENGTH,IER)   
      WRITE(6,*) '<'//STRING(1:LENGTH)//'>'
      WRITE(LUN,*) '<'//STRING(1:LENGTH)//'>'
C
      CALL EZPICK('TEST4')
      CALL EZGETS ('BONG',1,STRING,LENGTH,IER)   
      WRITE(6,*) '<'//STRING(1:LENGTH)//'>'
      WRITE(LUN,*) '<'//STRING(1:LENGTH)//'>'
C
      CALL EZPICK('TEST5')
      CALL EZGETS ('BONG',1,STRING,LENGTH,IER)   
      WRITE(6,*) '<'//STRING(1:LENGTH)//'>'
      WRITE(LUN,*) '<'//STRING(1:LENGTH)//'>'
C
      CALL EZRSET
      CALL EZGETS ('BONG',1,STRING,LENGTH,IER)   
      WRITE(6,*) '<'//STRING(1:LENGTH)//'>'
      WRITE(LUN,*) '<'//STRING(1:LENGTH)//'>'
C
      CALL EZRSET
      CALL EZGETS ('BONG',1,STRING,LENGTH,IER)   
      WRITE(6,*) '<'//STRING(1:LENGTH)//'>'
      WRITE(LUN,*) '<'//STRING(1:LENGTH)//'>'
C
      CALL EZRSET
      CALL EZGETS ('BONG',1,STRING,LENGTH,IER)   
      WRITE(6,*) '<'//STRING(1:LENGTH)//'>'
      WRITE(LUN,*) '<'//STRING(1:LENGTH)//'>'
C
      CALL EZPICK('TEST5')
      CALL EZGETS ('BONG',1,STRING,LENGTH,IER)   
      WRITE(6,*) '<'//STRING(1:LENGTH)//'>*****'
      WRITE(LUN,*) '<'//STRING(1:LENGTH)//'>*****'
C
      CALL EZRSET
      CALL EZGETS ('BONG',1,STRING,LENGTH,IER)   
      WRITE(6,*) '<'//STRING(1:LENGTH)//'>*****'
      WRITE(LUN,*) '<'//STRING(1:LENGTH)//'>*****'
C
      CALL EZRSET
      CALL EZGETS ('BONG',1,STRING,LENGTH,IER)   
      WRITE(6,*) '<'//STRING(1:LENGTH)//'>'
      WRITE(LUN,*) '<'//STRING(1:LENGTH)//'>'
C
      CALL EZRSET
      CALL EZGETS ('BONG',1,STRING,LENGTH,IER)   
      WRITE(6,*) '<'//STRING(1:LENGTH)//'>'
      WRITE(LUN,*) '<'//STRING(1:LENGTH)//'>'
C
      WRITE(LUN,*) ' ************************* '
      CALL EZDBUG(6)
      CALL EZDBUG(LUN)
      CALL EZCHAIN(LIST,NLIST)
      CALL EZDBUG(6)
      CALL EZDBUG(LUN)
C
      WRITE(LUN,*) ' ************************* '
      CALL EZDROP('TEST4')
      CALL EZDBUG(6)
      CALL EZDBUG(LUN)
C
      WRITE(LUN,*) ' ************************* '
      CALL EZPICK('TEST2')
      CALL EZGETS ('BONG',1,STRING,LENGTH,IER)   
      WRITE(6,*) '<'//STRING(1:LENGTH)//'>'
      WRITE(LUN,*) '<'//STRING(1:LENGTH)//'>'
C
      CALL EZRSET
      CALL EZGETS ('BONG',1,STRING,LENGTH,IER)   
      WRITE(6,*) '<'//STRING(1:LENGTH)//'>'
      WRITE(LUN,*) '<'//STRING(1:LENGTH)//'>'
C
      WRITE(LUN,*) ' ************************* '
      CALL EZDUMP(LUN,0,2)
C
C ****  Write out banks
C
      CALL ZZOPEN(LUNOUT,'ZEBRA',IER,'OUTPUT')
      CALL EZOUT (LUNOUT,'EZEXAMPLE2_RCP')
      CALL ZZCLOS(LUNOUT,IER,'OUTPUT')
C
      WRITE(LUN,*) ' ************************* '
      CALL EZDROP('EZEXAMPLE2_RCP')
      CALL EZDBUG(6)
      CALL EZDBUG(LUN)
C
C ****  Read in banks
C
      CALL ZZOPEN(LUNOUT,'ZEBRA',IER,'INPUT')
      CALL EZIN  (LUNOUT,' ')
      CALL ZZCLOS(LUNOUT,IER,'INPUT')
C
      WRITE(LUN,*) ' ************************* '
      CALL EZDBUG(6)
      CALL EZDBUG(LUN)
C
      CLOSE(UNIT=LUN)
  999 CONTINUE
      END
