/*
        CLIST.C
         Created           :  6-MAR-1990 by Stephen Adler
*/
 
#include <stdio.h>                   /* I/O definitions                       */
#include <stdlib.h>
#include <descrip>

#define STRING_TYPE 1
#define BYTE_TYPE 2
#define SHORT_TYPE 3
#define LONG_TYPE 4
#define REAL_TYPE 5
#define DOUBLE_TYPE 6
#define STRUCTURE_TYPE 7

char String_str[] = "String";
char Byte_str[] = "Byte";
char Word_str[] = "Word Integer";
char Long_str[] = "Long Integer";
char Float_str[] = "Single Precision Real";
char Double_str[] = "Double Precision Real";
char Struct_str[] = "Structure";


char lmsg0[] = "-I-String list created";
char lmsg1[] = "-F-Memory allocation error";
char lmsg2[] = "-I-List dumped";
char lmsg3[] = "-W-Attempting to read null list";
char lmsg4[] = "-W-Attempting to read past EOL";
char lmsg5[] = "-I-List element read";
char lmsg6[] = "-I-List element written";
char lmsg7[] = "-E-Attempting to erase null list";
char lmsg8[] = "-I-List element erased";
char lmsg9[] = "-I-List pointer reset to BOL";
char lmsg10[] = "-I-String erased at BOL";
char lmsg11[] = "-I-List purged";
char lmsg12[] = "-E-Id search failed";
char lmsg13[] = "-I-Id found";
char lmsg14[] = "-I-List restored";
char lmsg15[] = "-W-Deleted string list empty";
char lmsg16[] = "-I-Address list created";
char lmsg17[] = "-I-Byte list created";
char lmsg18[] = "-I-Word list created";
char lmsg19[] = "-I-Long word list created";
char lmsg20[] = "-I-Real list created";
char lmsg21[] = "-I-Double real list created";
char lmsg22[] = "-E-Unrecognized list type";
char lmsg23[] = "-E-Modifying null list";
char lmsg24[] = "-E-List pointer at EOL";
char lmsg25[] = "-W-Attempting to read before BOL";
char lmsg26[] = "-I-Structure list created";
char lmsg27[] = "-I-Modified list element";

char *lerr_msg[] = {
				     lmsg0,
					 lmsg1,
					 lmsg2,
					 lmsg3,
					 lmsg4,
					 lmsg5,
					 lmsg6,
					 lmsg7,
					 lmsg8,
					 lmsg9,
					 lmsg10,
					 lmsg11,
					 lmsg12,
					 lmsg13,
					 lmsg14,
					 lmsg15,
					 lmsg16,
					 lmsg17,
					 lmsg18,
					 lmsg19,
					 lmsg20,
					 lmsg21,
					 lmsg22,
					 lmsg23,
					 lmsg24,
					 lmsg25,
					 lmsg26,
					 lmsg27
};

char *tmp_string,*save_string;
int parse_ok;

struct item_s{
	struct item_s *next,*prev;
	int id;
	union {
	  void *address;
	  struct string_s{
	    char *pointer;
	    short length;
	  } string;
	  char cvalue;
	  short svalue;
	  long lvalue;
	  float fvalue;
	  double dvalue;
	} element;
};

struct lstd_s{
	struct item_s *first,*current,*last,*dfirst,*dlast;
	int total,erased,purged,read,ltype,struct_size;
	long next_id,err_no;
	char *ident_str,*ltype_str,*struct_synx_str;
};

long llength(lstd_addr)
long *lstd_addr;
{

	struct lstd_s *lstd;

	lstd = (struct lstd_s *)*lstd_addr;
	if (lstd == NULL) return(0);
	else return(lstd->total - lstd->erased);

}

long lmodify(lstd_addr,value_addr)
long *lstd_addr;
void *value_addr;
{
	char *string;
	struct item_s *cur_item;
	struct lstd_s *lstd;
    struct dsc$descriptor_s *vmsstr_dsc;
    char *cvalue_addr;
    short *svalue_addr;
    long *lvalue_addr;
    float *fvalue_addr;
    double *dvalue_addr;

	lstd = (struct lstd_s *)*lstd_addr;

	if (lstd->first == NULL) {
	  lstd->err_no = 23;
	  return(-1);
	}
	if ((lstd->current == NULL)&&(lstd->read==FALSE)) {
	  lstd->err_no = 24;
	  return(-1);
	}

	if (lstd->read == FALSE) cur_item = lstd->current;
	else {
	  if (lstd->current == NULL) cur_item = lstd->last;
	  else cur_item = lstd->current->prev;
	}
	switch (lstd->ltype) {
	  case STRUCTURE_TYPE :
	    memcpy(cur_item->element.address,value_addr,
	                         (size_t)lstd->struct_size);
	    break;
	  case STRING_TYPE :
	    free(cur_item->element.string.pointer);
	    vmsstr_dsc = (struct dsc$descriptor_s *)value_addr;
	    cur_item->element.string.pointer = 
	      malloc((size_t)vmsstr_dsc->dsc$w_length);
	    if (cur_item->element.string.pointer == NULL) {
          lstd->err_no = 1;
	      return(-1);
	    }
	    memcpy(cur_item->element.string.pointer,vmsstr_dsc->dsc$a_pointer,
	                         (size_t)vmsstr_dsc->dsc$w_length);
	    cur_item->element.string.length = vmsstr_dsc->dsc$w_length;
	    break;
	  case BYTE_TYPE :
	    cvalue_addr = (char *)value_addr;
	    cur_item->element.cvalue = *cvalue_addr;
	    break;
	  case SHORT_TYPE :
	    svalue_addr = (short *)value_addr;
	    cur_item->element.svalue = *svalue_addr;
	    break;
	  case LONG_TYPE :
	    lvalue_addr = (long *)value_addr;
	    cur_item->element.lvalue = *lvalue_addr;
	    break;
	  case REAL_TYPE :
	    fvalue_addr = (float *)value_addr;
	    cur_item->element.fvalue = *fvalue_addr;
	    break;
	  case DOUBLE_TYPE :
	    dvalue_addr = (double *)value_addr;
	    cur_item->element.dvalue = *dvalue_addr;
	    break;
	}

    lstd->err_no = 27;
	return(cur_item->id);
}

int lrestore(lstd_ident)
long *lstd_ident;
{
	struct lstd_s *lstd;
	struct item_s *del_item,*next_del;
	void linsert();

	lstd = (struct lstd_s *)*lstd_ident;
	
	if (lstd->dfirst == NULL) {
	  lstd->err_no = 15;
	  return(-1);
	}

	del_item = lstd->dfirst;
	while(del_item != NULL) {
	  next_del = del_item->next;
	  linsert(del_item,lstd);
	  del_item = next_del;
	}
	lstd->dfirst = NULL;
	lstd->dlast = NULL;
	lstd->err_no = 14;

	return(0);
}


void linsert(item,lstd)
struct item_s *item;
struct lstd_s *lstd;
{
	struct item_s *cur_item,*cur_item_prev;

	cur_item = lstd->first;
	while (cur_item != NULL) {
	  if (item->id < cur_item->id) break;
	  cur_item = cur_item->next;
	}

	if (cur_item == NULL) {
	  if (lstd->first == NULL) {
	    lstd->last = item;
	    lstd->first = item;
	    lstd->current = item;
	    lstd->read = FALSE;
	    item->prev = NULL;
	    item->next = NULL;
	  }
	  else {
	    lstd->last->next = item;
	    item->next = NULL;
	    item->prev = lstd->last;
	    lstd->last = item;
	  }
	}
	else if (cur_item->prev == NULL) {
	  lstd->first = item;
	  item->prev = NULL;
	  item->next = cur_item;
	  cur_item->prev = item;
	}
	else {
	  cur_item_prev = cur_item->prev;
	  cur_item_prev->next = item;
	  item->next = cur_item;
	  cur_item->prev = item;
	  item->prev = cur_item_prev;
	}

	lstd->erased--;

	return;
}

int lfind(lstd_ident,id)
long *lstd_ident,*id;
{
	struct lstd_s *lstd;
	struct item_s *cur_item;

	lstd = (struct lstd_s *)*lstd_ident;

	cur_item = lstd->first;

	while(cur_item != NULL) {
	  if (cur_item->id == *id) break;
	  cur_item = cur_item->next;
	}

	if (cur_item == NULL) {
	  lstd->err_no = 12;
	  return(-1);
	}
	else {
	  lstd->err_no = 13;
	  lstd->current = cur_item;
	  lstd->read = FALSE;
	  return(*id);
	}
}

int lcreate(list_type,lstd_ident,struct_syntax)
long *list_type;
struct dsc$descriptor_s *lstd_ident,*struct_syntax;
{
	struct lstd_s *lstd;

	switch (*list_type) {
	  case STRING_TYPE :
	  case BYTE_TYPE :
	  case SHORT_TYPE :
	  case LONG_TYPE :
	  case REAL_TYPE :
	  case DOUBLE_TYPE :
	  case STRUCTURE_TYPE : break;
	  defalut : lstd->err_no = 22;
	            return(-1);
	}


	lstd = (struct lstd_s *)malloc((size_t)sizeof(struct lstd_s));
	if (lstd == NULL) {
	  lstd->err_no = 1;
	  return(-1);
	}

	lstd->first = NULL;
	lstd->current = NULL;
	lstd->last = NULL;
	lstd->dfirst = NULL;
	lstd->dlast = NULL;
	lstd->total = 0;
	lstd->erased = 0;
	lstd->purged = 0;
	lstd->read = FALSE;
	lstd->ltype = *list_type;
	lstd->next_id = 1;
	lstd->ident_str = malloc((size_t)lstd_ident->dsc$w_length+1);
	if (lstd->ident_str == NULL) {
	  lstd->err_no = 1;
	  return(-1);
	}
	strncpy(lstd->ident_str,lstd_ident->dsc$a_pointer,
	                        (size_t)lstd_ident->dsc$w_length);
	lstd->ident_str[lstd_ident->dsc$w_length] = 0;
	if (*list_type == STRUCTURE_TYPE) {
      lstd->struct_synx_str = malloc((size_t)struct_syntax->dsc$w_length+1);
      if (lstd->struct_synx_str == NULL) {
        lstd->err_no = 1;
        return(-1);
      }
	  strncpy(lstd->struct_synx_str,struct_syntax->dsc$a_pointer,
	                        (size_t)struct_syntax->dsc$w_length);
	  lstd->struct_synx_str[struct_syntax->dsc$w_length] = 0;
	  lstd->struct_size = get_struct_size(lstd->struct_synx_str);
    }

	switch (lstd->ltype) {
	  case STRING_TYPE : lstd->err_no = 0;
	                      lstd->ltype_str = String_str;
	                      break;
	  case BYTE_TYPE : lstd->err_no = 17;
	                      lstd->ltype_str = Byte_str;
	                      break;
	  case SHORT_TYPE : lstd->err_no = 18;
	                      lstd->ltype_str = Word_str;
	                      break;
	  case LONG_TYPE : lstd->err_no = 19;
	                      lstd->ltype_str = Long_str;
	                      break;
	  case REAL_TYPE : lstd->err_no = 20;
	                      lstd->ltype_str = Float_str;
	                      break;
	  case DOUBLE_TYPE : lstd->err_no = 21;
	                      lstd->ltype_str = Double_str;
	                      break;
	  case STRUCTURE_TYPE : lstd->err_no = 26;
	  				        lstd->ltype_str = Struct_str;
	  				        break;
	}

	return(lstd);
}

int ldelete(lstd_addr)
long *lstd_addr;
{
	struct lstd_s *lstd;
	struct item_s *cur_ptr,*save_next;
    char *buffer;
	int ret_value;

	lstd = (struct lstd_s *)*lstd_addr;
	cur_ptr = lstd->first;
	while (cur_ptr != NULL) {
	  if ((lstd->ltype==STRING_TYPE)&&(cur_ptr->element.string.pointer!=NULL))
	     free(cur_ptr->element.string.pointer);
	  if ((lstd->ltype==STRUCTURE_TYPE)&&(cur_ptr->element.address!=NULL))
	     free(cur_ptr->element.address);
	  save_next = cur_ptr->next;
	  free(cur_ptr);
	  cur_ptr = save_next;
	}

	cur_ptr = lstd->dfirst;
	while (cur_ptr != NULL) {
	  if ((lstd->ltype==STRING_TYPE)&&(cur_ptr->element.string.pointer!=NULL))
	     free(cur_ptr->element.string.pointer);
	  if ((lstd->ltype==STRUCTURE_TYPE)&&(cur_ptr->element.address!=NULL))
	     free(cur_ptr->element.address);
	  save_next = cur_ptr->next;
	  free(cur_ptr);
	  cur_ptr = save_next;
	}

	free(lstd);
	*lstd_addr = 0;

	return(0);
}

int lpurge(lstd_addr)
long *lstd_addr;
{
	struct lstd_s *lstd;
	struct item_s *cur_ptr,*save_next;
    char *buffer;
	int ret_value;

	lstd = (struct lstd_s *)*lstd_addr;

	cur_ptr = lstd->dfirst;
	while (cur_ptr != NULL) {
	  if ((lstd->ltype==STRING_TYPE)&&(cur_ptr->element.string.pointer!=NULL))
	     free(cur_ptr->element.string.pointer);
	  if ((lstd->ltype==STRUCTURE_TYPE)&&(cur_ptr->element.address!=NULL))
	     free(cur_ptr->element.address);
	  save_next = cur_ptr->next;
	  free(cur_ptr);
	  cur_ptr = save_next;
	}

	lstd->dfirst = NULL;
	lstd->dlast = NULL;
	lstd->purged += lstd->erased;
	lstd->total -= lstd->erased;
	lstd->erased = 0;
	lstd->err_no = 11;
	return(0);
}

void ldump(lstd_addr)
long *lstd_addr;
{
	struct lstd_s *lstd;
	struct item_s *dump_ptr;
	void dump_structure();

	lstd = (struct lstd_s *)*lstd_addr;

	printf("Dump of %s list '%s'\n",lstd->ltype_str,lstd->ident_str);
	printf("Active# %d, Erased# %d, Total# %d, Purged# %d\n",
	       lstd->total-lstd->erased,lstd->erased,lstd->total,lstd->purged);
	dump_ptr = lstd->first;
	if (lstd->ltype == STRUCTURE_TYPE)
	  printf("Structure syntax : %s\n",lstd->struct_synx_str);
	while (dump_ptr != NULL) {
	  switch (lstd->ltype) {
	    case STRUCTURE_TYPE :
	      if (dump_ptr != lstd->current) {
	        printf("Struct ID: %3d,: ",dump_ptr->id);
	        dump_structure(dump_ptr->element.address,
	          lstd->struct_synx_str,FALSE);
	      }
	      else {
	        printf("Struct ID:*%3d,: ",dump_ptr->id);
	        dump_structure(dump_ptr->element.address,
	          lstd->struct_synx_str,TRUE);
	      }
	      break;
	    case STRING_TYPE :
	      if (dump_ptr != lstd->current)
	        printf("String ID: %d,: '%s'\n",
	           dump_ptr->id,dump_ptr->element.string.pointer);
	      else
	        printf("String ID:*%d,: '%s'\n",
	    	   dump_ptr->id,dump_ptr->element.string.pointer);
	      break;
	    case BYTE_TYPE :
	      if (dump_ptr != lstd->current)
	        printf("String ID: %d,: %c\n",
	           dump_ptr->id,dump_ptr->element.cvalue);
	      else
	        printf("String ID:*%d,: %c\n",
	    	   dump_ptr->id,dump_ptr->element.cvalue);
	      break;
	    case SHORT_TYPE :
	      if (dump_ptr != lstd->current)
	        printf("String ID: %d,: %hd\n",
	           dump_ptr->id,dump_ptr->element.svalue);
	      else
	        printf("String ID:*%d,: %hd\n",
	    	   dump_ptr->id,dump_ptr->element.svalue);
	      break;
	    case LONG_TYPE :
	      if (dump_ptr != lstd->current)
	        printf("String ID: %d,: %ld\n",
	           dump_ptr->id,dump_ptr->element.lvalue);
	      else
	        printf("String ID:*%d,: %ld\n",
	    	   dump_ptr->id,dump_ptr->element.lvalue);
	      break;
	    case REAL_TYPE :
	      if (dump_ptr != lstd->current)
	        printf("String ID: %d,: %e\n",
	           dump_ptr->id,dump_ptr->element.fvalue);
	      else
	        printf("String ID:*%d,: %e\n",
	    	   dump_ptr->id,dump_ptr->element.fvalue);
	      break;
	    case DOUBLE_TYPE :
	      if (dump_ptr != lstd->current)
	        printf("String ID: %d,: %le\n",
	           dump_ptr->id,dump_ptr->element.dvalue);
	      else
	        printf("String ID:*%d,: %le\n",
	    	   dump_ptr->id,dump_ptr->element.dvalue);
	      break;
	  }
	  dump_ptr = dump_ptr->next;
	}
	if (lstd->current == NULL) printf("List pointer at EOL\n");

	dump_ptr = lstd->dfirst;
	while (dump_ptr != NULL) {
	  printf("String ID: %d has been erased.\n",dump_ptr->id);
	  dump_ptr = dump_ptr->next;
	}

	lstd->err_no = 2;
	return;
}

long lread(lstd_addr,value_addr,value_len)
long *lstd_addr;
void *value_addr;
unsigned short *value_len;
{

	struct lstd_s *lstd;
	struct item_s *lstcur;
    struct dsc$descriptor_s *vmsstr_dsc;
    char *cvalue_addr;
    short *svalue_addr;
    long *lvalue_addr;
    float *fvalue_addr;
    double *dvalue_addr;
	int ret_id;
	unsigned short strlength;
	size_t no_to_copy;

	lstd = (struct lstd_s *)*lstd_addr;
	lstcur = lstd->current;
	if (lstd->first == NULL) {
	  lstd->err_no = 3;
	  return(-1);
	}
	if (lstd->current == NULL) {
	  lstd->err_no = 4;
	  return(-1);
	}

	switch (lstd->ltype) {
	  case STRUCTURE_TYPE :
	    memcpy(value_addr,lstcur->element.address,
	        (size_t)lstd->struct_size);
	    break;
	  case STRING_TYPE :
	    vmsstr_dsc = (struct dsc$descriptor_s *)value_addr;
	    no_to_copy  = 
	      (vmsstr_dsc->dsc$w_length > lstcur->element.string.length ?
	         (size_t)lstcur->element.string.length :
	         (size_t)vmsstr_dsc->dsc$w_length );
	    memcpy(vmsstr_dsc->dsc$a_pointer,lstcur->element.string.pointer,
	           no_to_copy);
	    *value_len = (unsigned short)no_to_copy;
	    break;
	  case BYTE_TYPE :
	    cvalue_addr = (char *)value_addr;
	    *cvalue_addr = lstd->current->element.cvalue;
	    break;
	  case SHORT_TYPE :
	    svalue_addr = (short *)value_addr;
	    *svalue_addr = lstd->current->element.svalue;
	    break;
	  case LONG_TYPE :
	    lvalue_addr = (long *)value_addr;
	    *lvalue_addr = lstd->current->element.lvalue;
	    break;
	  case REAL_TYPE :
	    fvalue_addr = (float *)value_addr;
	    *fvalue_addr = lstd->current->element.fvalue;
	    break;
	  case DOUBLE_TYPE :
	    dvalue_addr = (double *)value_addr;
	    *dvalue_addr = lstd->current->element.dvalue;
	    break;
	}

	ret_id = lstd->current->id;
	lstd->current = lstd->current->next;
	lstd->err_no = 5;
	lstd->read = TRUE;
	return(ret_id);

}

long lrcur(lstd_addr,value_addr,value_len)
long *lstd_addr;
void *value_addr;
unsigned short *value_len;
{

	struct lstd_s *lstd;
	struct item_s *lstcur;
    struct dsc$descriptor_s *vmsstr_dsc;
    char *cvalue_addr;
    short *svalue_addr;
    long *lvalue_addr;
    float *fvalue_addr;
    double *dvalue_addr;
	int ret_id;
	unsigned short strlength;
	size_t no_to_copy;

	lstd = (struct lstd_s *)*lstd_addr;
	lstcur = lstd->current;
	if (lstd->first == NULL) {
	  lstd->err_no = 3;
	  return(-1);
	}
	if (lstd->current == NULL) {
	  lstd->err_no = 4;
	  return(-1);
	}

	switch (lstd->ltype) {
	  case STRUCTURE_TYPE :
	    memcpy(value_addr,lstcur->element.address,
	        (size_t)lstd->struct_size);
	    break;
	  case STRING_TYPE :
	    vmsstr_dsc = (struct dsc$descriptor_s *)value_addr;
	    no_to_copy = 
          (vmsstr_dsc->dsc$w_length > lstcur->element.string.length ?
	           (size_t)lstcur->element.string.length :
	           (size_t)vmsstr_dsc->dsc$w_length);
	    memcpy(vmsstr_dsc->dsc$a_pointer,lstcur->element.string.pointer,
	           no_to_copy);
	    *value_len = (unsigned short)no_to_copy;
	    break;
	  case BYTE_TYPE :
	    cvalue_addr = (char *)value_addr;
	    *cvalue_addr = lstd->current->element.cvalue;
	    break;
	  case SHORT_TYPE :
	    svalue_addr = (short *)value_addr;
	    *svalue_addr = lstd->current->element.svalue;
	    break;
	  case LONG_TYPE :
	    lvalue_addr = (long *)value_addr;
	    *lvalue_addr = lstd->current->element.lvalue;
	    break;
	  case REAL_TYPE :
	    fvalue_addr = (float *)value_addr;
	    *fvalue_addr = lstd->current->element.fvalue;
	    break;
	  case DOUBLE_TYPE :
	    dvalue_addr = (double *)value_addr;
	    *dvalue_addr = lstd->current->element.dvalue;
	    break;
	}

	ret_id = lstd->current->id;
	lstd->err_no = 5;
	return(ret_id);

}

long lrnext(lstd_addr,value_addr,value_len)
long *lstd_addr;
void *value_addr;
unsigned short *value_len;
{

	struct lstd_s *lstd;
	struct item_s *lstnext;
    struct dsc$descriptor_s *vmsstr_dsc;
    char *cvalue_addr;
    short *svalue_addr;
    long *lvalue_addr;
    float *fvalue_addr;
    double *dvalue_addr;
	int ret_id;
	unsigned short strlength;
	size_t no_to_copy;

	lstd = (struct lstd_s *)*lstd_addr;
	if (lstd->first == NULL) {
	  lstd->err_no = 3;
	  return(-1);
	}
	if (lstd->current == NULL) {
	  lstd->err_no = 24;
	  return(-1);
	}
	lstnext = lstd->current->next;
	if (lstnext == NULL) {
	  lstd->err_no = 4;
	  return(-1);
	}

	switch (lstd->ltype) {
	  case STRUCTURE_TYPE :
	    memcpy(value_addr,lstnext->element.address,
	        (size_t)lstd->struct_size);
	    break;
	  case STRING_TYPE :
	    vmsstr_dsc = (struct dsc$descriptor_s *)value_addr;
	    no_to_copy = 
	      (vmsstr_dsc->dsc$w_length > lstnext->element.string.length ?
	        (size_t)lstnext->element.string.length :
	        (size_t)vmsstr_dsc->dsc$w_length);
	    memcpy(vmsstr_dsc->dsc$a_pointer,lstnext->element.string.pointer,
	           no_to_copy);
	    *value_len = (unsigned short)no_to_copy;
	    break;
	  case BYTE_TYPE :
	    cvalue_addr = (char *)value_addr;
	    *cvalue_addr = lstnext->element.cvalue;
	    break;
	  case SHORT_TYPE :
	    svalue_addr = (short *)value_addr;
	    *svalue_addr = lstnext->element.svalue;
	    break;
	  case LONG_TYPE :
	    lvalue_addr = (long *)value_addr;
	    *lvalue_addr = lstnext->element.lvalue;
	    break;
	  case REAL_TYPE :
	    fvalue_addr = (float *)value_addr;
	    *fvalue_addr = lstnext->element.fvalue;
	    break;
	  case DOUBLE_TYPE :
	    dvalue_addr = (double *)value_addr;
	    *dvalue_addr = lstnext->element.dvalue;
	    break;
	}

	ret_id = lstnext->id;
	lstd->err_no = 5;
	return(ret_id);

}

long lrprev(lstd_addr,value_addr,value_len)
long *lstd_addr;
void *value_addr;
unsigned short *value_len;
{

	struct lstd_s *lstd;
	struct item_s *lstprev;
    struct dsc$descriptor_s *vmsstr_dsc;
    char *cvalue_addr;
    short *svalue_addr;
    long *lvalue_addr;
    float *fvalue_addr;
    double *dvalue_addr;
	int ret_id;
	unsigned short strlength;
	size_t no_to_copy;

	lstd = (struct lstd_s *)*lstd_addr;
	if (lstd->first == NULL) {
	  lstd->err_no = 3;
	  return(-1);
	}
	if (lstd->current == NULL) lstprev = lstd->last;
	else lstprev = lstd->current->prev;
	if (lstprev == NULL) {
	  lstd->err_no = 25;
	  return(-1);
	}

	switch (lstd->ltype) {
	  case STRUCTURE_TYPE :
	    memcpy(value_addr,lstprev->element.address,
	        (size_t)lstd->struct_size);
	    break;
	  case STRING_TYPE :
	    vmsstr_dsc = (struct dsc$descriptor_s *)value_addr;
	    no_to_copy = 
	      (vmsstr_dsc->dsc$w_length > lstprev->element.string.length ?
	        (size_t)lstprev->element.string.length :
	        (size_t)vmsstr_dsc->dsc$w_length);
	    memcpy(vmsstr_dsc->dsc$a_pointer,lstprev->element.string.pointer,
	           no_to_copy);
	    *value_len = (unsigned short)no_to_copy;
	    break;
	  case BYTE_TYPE :
	    cvalue_addr = (char *)value_addr;
	    *cvalue_addr = lstprev->element.cvalue;
	    break;
	  case SHORT_TYPE :
	    svalue_addr = (short *)value_addr;
	    *svalue_addr = lstprev->element.svalue;
	    break;
	  case LONG_TYPE :
	    lvalue_addr = (long *)value_addr;
	    *lvalue_addr = lstprev->element.lvalue;
	    break;
	  case REAL_TYPE :
	    fvalue_addr = (float *)value_addr;
	    *fvalue_addr = lstprev->element.fvalue;
	    break;
	  case DOUBLE_TYPE :
	    dvalue_addr = (double *)value_addr;
	    *dvalue_addr = lstprev->element.dvalue;
	    break;
	}

	ret_id = lstprev->id;
	lstd->err_no = 5;
	return(ret_id);

}

long lwrite(lstd_addr,value_addr)
long *lstd_addr;
void *value_addr;
{
	char *string;
	struct item_s *cur_item;
	struct lstd_s *lstd;
    struct dsc$descriptor_s *vmsstr_dsc;
    char *cvalue_addr;
    short *svalue_addr;
    long *lvalue_addr;
    float *fvalue_addr;
    double *dvalue_addr;

	lstd = (struct lstd_s *)*lstd_addr;
	cur_item = malloc(sizeof(struct item_s));
	if (cur_item == NULL) {
	  lstd->err_no = 1;
	  return(-1);
	}

	if (lstd->first == NULL) {
	  lstd->last = cur_item;
	  lstd->first = cur_item;
	  lstd->current = cur_item;
	  cur_item->prev = NULL;
	  cur_item->next = NULL;
	}
	else {
	  lstd->last->next = cur_item;
	  cur_item->next = NULL;
	  cur_item->prev = lstd->last;
	  lstd->last = cur_item;
	}

	switch (lstd->ltype) {
	  case STRUCTURE_TYPE :
	    cur_item->element.address = malloc((size_t)lstd->struct_size);
	    if (cur_item->element.address == NULL) {
          lstd->err_no = 1;
	      return(-1);
	    }
	    memcpy(cur_item->element.address,value_addr,
	                         (size_t)lstd->struct_size);
	    break;
	  case STRING_TYPE :
	    vmsstr_dsc = (struct dsc$descriptor_s *)value_addr;
	    cur_item->element.string.pointer =
	      malloc((size_t)vmsstr_dsc->dsc$w_length+1);
	    if (cur_item->element.string.pointer == NULL) {
          lstd->err_no = 1;
	      return(-1);
	    }
	    memcpy(cur_item->element.string.pointer,vmsstr_dsc->dsc$a_pointer,
	                         (size_t)vmsstr_dsc->dsc$w_length);
        cur_item->element.string.length = vmsstr_dsc->dsc$w_length;
        cur_item->element.string.pointer[vmsstr_dsc->dsc$w_length] = 0;
	    break;
	  case BYTE_TYPE :
	    cvalue_addr = (char *)value_addr;
	    cur_item->element.cvalue = *cvalue_addr;
	    break;
	  case SHORT_TYPE :
	    svalue_addr = (short *)value_addr;
	    cur_item->element.svalue = *svalue_addr;
	    break;
	  case LONG_TYPE :
	    lvalue_addr = (long *)value_addr;
	    cur_item->element.lvalue = *lvalue_addr;
	    break;
	  case REAL_TYPE :
	    fvalue_addr = (float *)value_addr;
	    cur_item->element.fvalue = *fvalue_addr;
	    break;
	  case DOUBLE_TYPE :
	    dvalue_addr = (double *)value_addr;
	    cur_item->element.dvalue = *dvalue_addr;
	    break;
	}

    cur_item->id = lstd->next_id++;
    lstd->total++;

    lstd->err_no = 6;
	return(cur_item->id);
}

void lmovtd(lstd,del_item)
struct lstd_s *lstd;
struct item_s *del_item;
{

	if (del_item == lstd->current) {
	  lstd->current = lstd->current->next;
	}

	if ((del_item->prev == NULL) && (del_item->next == NULL)) {
	  lstd->first = NULL;
	  lstd->last = NULL;
	  lstd->current = NULL;
	}
	else if (del_item->prev == NULL) {
	  lstd->first = del_item->next;
	  lstd->first->prev = NULL;
	}
	else if (del_item->next == NULL) {
	  lstd->last = del_item->prev;
	  lstd->last->next = NULL;
	}
	else {
	  del_item->prev->next = del_item->next;
	  del_item->next->prev = del_item->prev;
	}

	if (lstd->dfirst == NULL) {
	  lstd->dlast = del_item;
	  lstd->dfirst = del_item;
	  del_item->prev = NULL;
	  del_item->next = NULL;
	}
	else {
	  lstd->dlast->next = del_item;
	  del_item->next = NULL;
	  del_item->prev = lstd->dlast;
	  lstd->dlast = del_item;
	}

	return;
}

int lerase(lstd_addr)
long *lstd_addr;
{
	struct lstd_s *lstd;
	struct item_s *del_item;

	lstd = (struct lstd_s *)*lstd_addr;

	if (lstd->first == NULL) {
	  lstd->err_no = 7;
	  return(-1);
	}
	if (lstd->read == TRUE) {
	  lstd->read = FALSE;
	  if (lstd->current == NULL) lmovtd(lstd,lstd->last);
	  else lmovtd(lstd,lstd->current->prev);
	}
	else {
	  if (lstd->current == NULL) lmovtd(lstd,lstd->last);
	  else lmovtd(lstd,lstd->current);
	}
	lstd->erased++;

	lstd->err_no = 8;
	return(0);
}

int lreset(lstd_addr)
long *lstd_addr;
{
	struct lstd_s *lstd;

	lstd = (struct lstd_s *)*lstd_addr;
	lstd->current = lstd->first;
	lstd->read = FALSE;

	lstd->err_no = 9;
	return(0);
}

void lerror(lstd_addr)
long *lstd_addr;
{
	struct lstd_s *lstd;
	int err_index;

	lstd = (struct lstd_s *)*lstd_addr;
	err_index = lstd->err_no;
	printf("%CLIST%s : list '%s'\n",lerr_msg[err_index],lstd->ident_str);

	return;
}

int get_struct_size(struct_synx_str)
char *struct_synx_str;
{
	int struct_size,size,rep;
	char type;
	int get_syntax_item();
	void prep_syntax_parse(),end_syntax_parse();

	struct_size = 0;
	prep_syntax_parse(struct_synx_str);
	while(get_syntax_item(&type,&size,&rep)) struct_size += size*rep;
	end_syntax_parse();
	return(struct_size);
}

void dump_structure(item,syntax)
char *item,*syntax;
{
	char *char_item;
	short *short_item;
	long *long_item;
	float *float_item,real_float,imaginary_float;
	double *double_item,real_double,imaginary_double;
	char type,*item_cur_ptr,print_str[100];
	int size,rep,i,j;
	void prep_syntax_parse(),end_syntax_parse();
	union byte_word_long {
	  char byte[4];
	  short word[2];
	  long big;
	} logi;
	char space[] = "                 ",ospace[100];
	int first;

	item_cur_ptr = item;
    prep_syntax_parse(syntax);
    first = TRUE;
    ospace[0] = 0;
    while(get_syntax_item(&type,&size,&rep)) {
      switch (type) {
      	case 'I' :
      	  switch (size) {
      	    case 1 :
      	      char_item = item_cur_ptr;
      	      for (i=0;i<rep;i++) {
      	      	if (rep==1) printf("%sI*1 : %d\n",ospace,*(char_item++));
      	      	else printf("%sI*1[%d] : %d\n",ospace,i+1,*(char_item++));
                if (first) {
      	          strcpy(ospace,space);
                  first = FALSE;
                }
      	      }
      	      item_cur_ptr = char_item;
      	      break;
      	    case 2 :
      	      short_item = (short *)item_cur_ptr;
      	      for (i=0;i<rep;i++) {
      	      	if (rep==1) printf("%sI*2 : %d\n",ospace,*(short_item++));
      	      	else printf("%sI*2[%d] : %d\n",ospace,i+1,*(short_item++));
                if (first) {
      	          strcpy(ospace,space);
                  first = FALSE;
                }
      	      }
      	      item_cur_ptr = (char *)short_item;
      	      break;
      	    case 4 :
      	      long_item = (long *)item_cur_ptr;
      	      for (i=0;i<rep;i++) {
      	      	if (rep==1) printf("%sI*4 : %ld\n",ospace,*(long_item++));
      	      	else printf("%sI*4[%d] : %ld\n",ospace,i,*(long_item++));
                if (first) {
      	          strcpy(ospace,space);
                  first = FALSE;
                }
      	      }
      	      item_cur_ptr = (char *)long_item;
      	      break;
      	    default :
      	      for (i=0;i<rep;i++) {
      	        if (rep==1) printf("%sI*%d : not supported\n",ospace,size);
      	        else printf("%sI*%d[%d] : not supported\n",ospace,size,i+1);
      	        item_cur_ptr += size;
                if (first) {
      	          strcpy(ospace,space);
                  first = FALSE;
                }
      	      }
      	  }
      	  break;
      	case 'L' :
      	  switch (size) {
      	  	case 1 :
      	  	  char_item = item_cur_ptr;
      	  	  for (i=0;i<rep;i++) {
      	  	  	logi.byte[0] = *(char_item++);
      	  	  	for (j=1;j<4;j++) logi.byte[j] = 0;
      	  	  	if (rep==1) printf("%sL*1 : %02.2X\n",ospace,logi.big);
      	  	  	else printf("%sL*1[%d] : %02.2X\n",ospace,i+1,logi.big);
                if (first) {
      	          strcpy(ospace,space);
                  first = FALSE;
                }
      	  	  }
      	  	  item_cur_ptr = char_item;
      	  	  break;
      	  	case 2 :
      	  	  short_item = (short *)item_cur_ptr;
      	  	  for (i=0;i<rep;i++) {
      	  	  	logi.word[0] = *(short_item++);
      	  	  	logi.word[1] = 0;
      	  	  	if (rep==1) printf("%sL*2 : %04.4X\n",ospace,logi.big);
      	  	  	else printf("%sL*2[%d] : %04.4X\n",ospace,i+1,logi.big);
                if (first) {
      	          strcpy(ospace,space);
                  first = FALSE;
                }
      	  	  }
      	  	  item_cur_ptr = (char *)short_item;
      	  	  break;
      	  	case 4 :
      	  	  long_item = (long *)item_cur_ptr;
      	  	  for (i=0;i<rep;i++) {
      	  	  	if (rep==1) printf("%sL*4 : %08.8X\n",ospace,*(long_item++));
      	  	  	else printf("%sL*4[%d] : %08.8X\n",ospace,i+1,*(long_item++));
                if (first) {
      	          strcpy(ospace,space);
                  first = FALSE;
                }
      	  	  }
      	  	  item_cur_ptr = (char *)long_item;
      	  	  break;
      	    default :
      	      for (i=0;i<rep;i++) {
      	        if (rep==1) printf("%sL*%d : not supported\n",ospace,size);
      	        else printf("%sL*%d[%d] : not supported\n",ospace,size,i+1);
      	        item_cur_ptr += size;
                if (first) {
      	          strcpy(ospace,space);
                  first = FALSE;
                }
      	      }
      	  }
      	  break;
      	case 'R' :
      	  switch (size) {
      	    case 4 :
      	      float_item = (float *)item_cur_ptr;
      	      for (i=0;i<rep;i++) {
      	      	if (rep==1) printf("%sR*4 : %f\n",ospace,*(float_item++));
      	      	else printf("%sR*4[%d] : %f\n",ospace,i+1,*(float_item++));
                if (first) {
      	          strcpy(ospace,space);
                  first = FALSE;
                }
      	      }
      	      item_cur_ptr = (char *)float_item;
      	      break;
      	    case 8 :
      	      double_item = (double *)item_cur_ptr;
      	      for (i=0;i<rep;i++) {
      	      	if (rep==1) printf("%sR*8 : %lf\n",ospace,*(double_item++));
      	      	else printf("%sR*8[%d] : %lf\n",ospace,i+1,*(double_item++));
                if (first) {
      	          strcpy(ospace,space);
                  first = FALSE;
                }
      	      }
      	      item_cur_ptr = (char *)double_item;
      	      break;
      	    default :
      	      for (i=0;i<rep;i++) {
      	        if (rep==1) printf("%sR*%d : not supported\n",ospace,size);
      	        else printf("%sR*%d[%d] : not supported\n",ospace,size,i+1);
      	        item_cur_ptr += size;
                if (first) {
      	          strcpy(ospace,space);
                  first = FALSE;
                }
      	      }
      	  }
      	  break;
      	case 'C' :
      	  switch (size) {
      	  	case 8 :
      	  	  float_item = (float *)item_cur_ptr;
      	  	  for (i=0;i<rep;i++) {
      	  	  	real_float = *(float_item++);
      	  	  	imaginary_float = *(float_item++);
      	  	  	if (rep==1)
      	  	  	  printf("%sC*8 : (%f,%f)\n",ospace,real_float,imaginary_float);
      	  	    else
      	  	  	  printf("%sC*8[%d] : (%f,%f)\n",ospace,i+1,real_float,imaginary_float);
                if (first) {
      	          strcpy(ospace,space);
                  first = FALSE;
                }
      	  	  }
      	  	  item_cur_ptr = (char *)float_item;
      	  	  break;
      	  	case 16 :
      	  	  double_item = (double *)item_cur_ptr;
      	  	  for (i=0;i<rep;i++) {
      	  	  	real_double = *(double_item++);
      	  	  	imaginary_double = *(double_item++);
      	  	  	if (rep==1) printf("%sC*16 : (%lf,%lf)\n",ospace,
      	  	     real_double,imaginary_double);
      	  	  	else printf("%sC*16[%d] : (%lf,%lf)\n",ospace,i+1,
      	  	     real_double,imaginary_double);
                if (first) {
      	          strcpy(ospace,space);
                  first = FALSE;
                }
      	  	  }
      	  	  item_cur_ptr = (char *)double_item;
      	  	  break;
      	  	default :
      	      for (i=0;i<rep;i++) {
      	        if (rep==1) printf("%sC*%d : not supported\n",ospace,size);
      	        else printf("%sC*%d[%d] : not supported\n",ospace,size,i+1);
      	        item_cur_ptr += size;
                if (first) {
      	          strcpy(ospace,space);
                  first = FALSE;
                }
      	      }
      	  }
      	  break;
      	case 'S' :
      	  char_item = item_cur_ptr;
      	  for (i=0;i<rep;i++) {
      	  	if (rep==1) {
      	  	  sprintf(print_str,"%sS*%1d : %%.%1ds\n",ospace,size,size);
      	  	  printf(print_str,char_item);
      	  	}
      	  	else {
      	  	  sprintf(print_str,"%sS*%1d[%%d] : %%.%1ds\n",ospace,size,size);
      	  	  printf(print_str,i+1,char_item);
      	  	}
      	  	char_item += size;
            if (first) {
      	      strcpy(ospace,space);
             first = FALSE;
            }
      	  }
      	  item_cur_ptr = char_item;
      	  break;
      	default :
      	  for (i=0;i<rep;i++) {
      	  	if (rep==1) printf("%s%c*%d : not emplimented\n",ospace,type,size);
      	  	else printf("%s%c*%d[%d] : not emplimented\n",ospace,type,size,i+1);
      	  	item_cur_ptr += size;
            if (first) {
      	      strcpy(ospace,space);
              first = FALSE;
            }
      	  }
      }
    }
    end_syntax_parse();
}

void prep_syntax_parse(syntax)
char *syntax;
{
	tmp_string = malloc((size_t)(strlen(syntax)+1));
	save_string = tmp_string;
	strcpy(tmp_string,syntax);
	tmp_string[strlen(syntax)] = 0;
	parse_ok = TRUE;
}

void end_syntax_parse()
{
	free(save_string);
}

int get_syntax_item(type,size,rep)
char *type;
int *size,*rep;
{

	int i,struct_size=0,repeat_item,item_len;
	int repeat_str_len,parse_return;
	char *save_string,*struct_item,*comma;
	char repeat_str[10],ok;

	parse_return = TRUE;
	if (parse_ok) {
	  struct_item = tmp_string;
	  if (comma=strchr(tmp_string,',')) {
	    *comma = 0;
	    struct_item = tmp_string;
	    tmp_string = comma+1;
	  }
	  else {
	    parse_ok = FALSE;
	    struct_item = tmp_string;
	  }

	  repeat_str_len = strcspn(struct_item,"IiRrCcLlSs");
	  if (repeat_str_len == strlen(struct_item)) {
	    printf("%CLIST-Error in struct syntax\n");
	    return(-1);
	  }
	  *type = struct_item[repeat_str_len];
	  if ((*type>='a')&&(*type<='z')) *type = *type - 'a' + 'A';
	  if (repeat_str_len == 0) *rep = 1;
	  else {
	    strncpy(repeat_str,struct_item,repeat_str_len);
	    sscanf(repeat_str,"%d",rep);
	  }
	  sscanf(&struct_item[repeat_str_len+2],"%d",size);

	}
	else parse_return = FALSE;
	return(parse_return);
}
