C     FSAMPL.INC
C----------------------------------------------------------------------
C-
C-   Created  17-DEC-1987   Dave Buchholz
C-   Updated   3-FEB-1989   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      COMMON /FSAMPL/ IFLAG, 
C                            
C                            
     &  ISTART,              
     &  ISAMPL,              
     &  ICHECK,              
     &  MAXSAM,              
     &  NSAMPL(100),         
     &  STDSAM,              ! set 'true' if want sample as set in fdc_ctrl.dat
     &  SKPEVT               
C
      INTEGER IFLAG,ISTART,ISAMPL,ICHECK,MAXSAM,NSAMPL,SKPEVT
      LOGICAL STDSAM
C----------------------------------------------------------------------
