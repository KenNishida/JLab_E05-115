#include <stdio.h>
#include <rpc/rpc.h>
#include "daVarRpc.h"
#include <time.h>
#define _xdr_result xdr_result
#define _xdr_argument xdr_argument

void
davarsvr_1(rqstp, transp)
	struct svc_req *rqstp;
	SVCXPRT *transp;
{
	union {
		char *davar_ackmessage_1_arg;
		char *davar_getlist_1_arg;
		NAMELIST davar_readmultiple_1_arg;
		WVALLIST davar_writemultiple_1_arg;
		TESTNAMELIST davar_readmultiple_test_1_arg;
		RVALLIST davar_readmultiple_test_cb_1_arg;
		char *davar_readpatternmatch_1_arg;
	} argument;
	char *result;
	bool_t (*xdr_argument)(), (*xdr_result)();
	char *(*local)();

	switch (rqstp->rq_proc) {
	case NULLPROC:
		(void)svc_sendreply(transp, xdr_void, (char *)NULL);
		return;

	case DAVAR_ACKMESSAGE:
		xdr_argument = xdr_wrapstring;
		xdr_result = xdr_int;
		local = (char *(*)()) davar_ackmessage_1;
		break;

	case DAVAR_GETLIST:
		xdr_argument = xdr_wrapstring;
		xdr_result = xdr_NAMELIST;
		local = (char *(*)()) davar_getlist_1;
		break;

	case DAVAR_READMULTIPLE:
		xdr_argument = xdr_NAMELIST;
		xdr_result = xdr_RVALLIST;
		local = (char *(*)()) davar_readmultiple_1;
		break;

	case DAVAR_WRITEMULTIPLE:
		xdr_argument = xdr_WVALLIST;
		xdr_result = xdr_ERRLIST;
		local = (char *(*)()) davar_writemultiple_1;
		break;

	case DAVAR_READMULTIPLE_TEST:
		xdr_argument = xdr_TESTNAMELIST;
		xdr_result = xdr_int;
		local = (char *(*)()) davar_readmultiple_test_1;
		break;

	case DAVAR_READMULTIPLE_TEST_CB:
		xdr_argument = xdr_RVALLIST;
		xdr_result = xdr_int;
		local = (char *(*)()) davar_readmultiple_test_cb_1;
		break;

	case DAVAR_READPATTERNMATCH:
		xdr_argument = xdr_wrapstring;
		xdr_result = xdr_WVALLIST;
		local = (char *(*)()) davar_readpatternmatch_1;
		break;

	default:
		svcerr_noproc(transp);
		return;
	}
	bzero((char *)&argument, sizeof(argument));
	if (!svc_getargs(transp, xdr_argument, &argument)) {
		svcerr_decode(transp);
		return;
	}
	result = (*local)(&argument, rqstp);
	if (result != NULL && !svc_sendreply(transp, xdr_result, result)) {
		svcerr_systemerr(transp);
	}
	if (!svc_freeargs(transp, xdr_argument, &argument)) {
		(void)fprintf(stderr, "unable to free arguments\n");
		exit(1);
	}
}

