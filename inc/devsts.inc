C **** COMMON BLOCK DEVSTS
C-    Also for Dial Box control. ( Nobu Oshima, 30-APR-1993 )
C
C     This common block is used for determining device state.
C
C        DEVON    logical        designates that JDEVON has been called
C                                and the device has been initialized.
      LOGICAL DEVON,LASTCO
C
      COMMON /DEVSTS/ DEVON,LASTCO

