#include <rpc/rpc.h>
#include <sys/time.h>
#include "daVarRpc.h"
#include <time.h>
#define _xdr_result xdr_result
#define _xdr_argument xdr_argument

/* Default timeout can be changed using clnt_control() */
static struct timeval TIMEOUT = { 25, 0 };

int *
davar_ackmessage_1(argp, clnt)
	char **argp;
	CLIENT *clnt;
{
	static int res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, DAVAR_ACKMESSAGE, xdr_wrapstring, argp, xdr_int, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


NAMELIST *
davar_getlist_1(argp, clnt)
	char **argp;
	CLIENT *clnt;
{
	static NAMELIST res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, DAVAR_GETLIST, xdr_wrapstring, argp, xdr_NAMELIST, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


RVALLIST *
davar_readmultiple_1(argp, clnt)
	NAMELIST *argp;
	CLIENT *clnt;
{
	static RVALLIST res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, DAVAR_READMULTIPLE, xdr_NAMELIST, argp, xdr_RVALLIST, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


ERRLIST *
davar_writemultiple_1(argp, clnt)
	WVALLIST *argp;
	CLIENT *clnt;
{
	static ERRLIST res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, DAVAR_WRITEMULTIPLE, xdr_WVALLIST, argp, xdr_ERRLIST, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


int *
davar_readmultiple_test_1(argp, clnt)
	TESTNAMELIST *argp;
	CLIENT *clnt;
{
	static int res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, DAVAR_READMULTIPLE_TEST, xdr_TESTNAMELIST, argp, xdr_int, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}


WVALLIST *
davar_readpatternmatch_1(argp, clnt)
	char **argp;
	CLIENT *clnt;
{
	static WVALLIST res;

	bzero((char *)&res, sizeof(res));
	if (clnt_call(clnt, DAVAR_READPATTERNMATCH, xdr_wrapstring, argp, xdr_WVALLIST, &res, TIMEOUT) != RPC_SUCCESS) {
		return (NULL);
	}
	return (&res);
}

