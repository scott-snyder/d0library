#include <unistd.h>
sleep_time_(int sec)
{
	unsigned int islept;
	islept = sleep ( sec );
	return(islept);
}
