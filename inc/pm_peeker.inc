C----------------------------------------------------------------------
C-
C-   Created  17-JAN-1992   Lee Lueking
C-
C----------------------------------------------------------------------
C use with PM queue peeker and resource viewer
C
C setup of pasteboards and virtual keyboards
C
	integer*4  pb_id,pb_rows,pb_cols
	integer*4  display_id,display_rows,display_cols
	integer*4  keyboard_id
C
C setup of columns and headers
C
      CHARACTER*40 CH(10) 
      INTEGER      CW(10) 
      INTEGER      CL(10) 
      INTEGER      CS(10) 
      INTEGER nc  

      COMMON /PM_PEEKER/ 
     +			     pb_id,pb_rows,pb_cols,
     +			     display_id,display_rows,display_cols,
     +			     keyboard_id,ch,cw,cl,nc
