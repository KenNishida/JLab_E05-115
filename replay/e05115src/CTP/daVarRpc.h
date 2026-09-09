#include <time.h>
#define _xdr_result xdr_result
#define _xdr_argument xdr_argument
#define DAVARINT_RPC 1
#define DAVARFLOAT_RPC 2
#define DAVARDOUBLE_RPC 3
#define DAVARSTRING_RPC 4
#define DAVARERROR_RPC 999

typedef char *PNAME;
bool_t xdr_PNAME();


typedef struct {
	u_int NAMELIST_len;
	PNAME *NAMELIST_val;
} NAMELIST;
bool_t xdr_NAMELIST();


struct any {
	int valtype;
	union {
		struct {
			u_int i_len;
			int *i_val;
		} i;
		struct {
			u_int r_len;
			float *r_val;
		} r;
		struct {
			u_int d_len;
			double *d_val;
		} d;
		char *s;
		int error;
	} any_u;
};
typedef struct any any;
bool_t xdr_any();


struct wany {
	PNAME name;
	any *val;
};
typedef struct wany wany;
bool_t xdr_wany();


typedef struct {
	u_int RVALLIST_len;
	any *RVALLIST_val;
} RVALLIST;
bool_t xdr_RVALLIST();


typedef struct {
	u_int WVALLIST_len;
	wany *WVALLIST_val;
} WVALLIST;
bool_t xdr_WVALLIST();


typedef struct {
	u_int ERRLIST_len;
	int *ERRLIST_val;
} ERRLIST;
bool_t xdr_ERRLIST();


struct TESTNAMELIST {
	char *test_condition;
	int max_time_wait;
	int max_event_wait;
	int prog;
	int vers;
	NAMELIST *NAMELISTP;
};
typedef struct TESTNAMELIST TESTNAMELIST;
bool_t xdr_TESTNAMELIST();


#define DAVARSVR ((u_long)0x2c0daFF8)
#define DAVARVERS ((u_long)1)
#define DAVAR_ACKMESSAGE ((u_long)101)
extern int *davar_ackmessage_1();
#define DAVAR_GETLIST ((u_long)102)
extern NAMELIST *davar_getlist_1();
#define DAVAR_READMULTIPLE ((u_long)103)
extern RVALLIST *davar_readmultiple_1();
#define DAVAR_WRITEMULTIPLE ((u_long)104)
extern ERRLIST *davar_writemultiple_1();
#define DAVAR_READMULTIPLE_TEST ((u_long)105)
extern int *davar_readmultiple_test_1();
#define DAVAR_READMULTIPLE_TEST_CB ((u_long)106)
extern int *davar_readmultiple_test_cb_1();
#define DAVAR_READPATTERNMATCH ((u_long)107)
extern WVALLIST *davar_readpatternmatch_1();

