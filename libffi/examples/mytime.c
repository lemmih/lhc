#include <time.h>

struct tm mylocaltime(const time_t);
time_t mymktime(struct tm);

struct tm mylocaltime(const time_t t)
{
	return *localtime(&t);
}

time_t mymktime(struct tm t)
{
	return mktime(&t);
}
