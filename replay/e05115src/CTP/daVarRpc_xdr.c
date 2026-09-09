#include <rpc/rpc.h>
#include "daVarRpc.h"
#include <time.h>
#define _xdr_result xdr_result
#define _xdr_argument xdr_argument


bool_t
xdr_PNAME(xdrs, objp)
	XDR *xdrs;
	PNAME *objp;
{
	if (!xdr_string(xdrs, objp, ~0)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_NAMELIST(xdrs, objp)
	XDR *xdrs;
	NAMELIST *objp;
{
	if (!xdr_array(xdrs, (char **)&objp->NAMELIST_val, (u_int *)&objp->NAMELIST_len, ~0, sizeof(PNAME), xdr_PNAME)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_any(xdrs, objp)
	XDR *xdrs;
	any *objp;
{
	if (!xdr_int(xdrs, &objp->valtype)) {
		return (FALSE);
	}
	switch (objp->valtype) {
	case DAVARINT_RPC:
		if (!xdr_array(xdrs, (char **)&objp->any_u.i.i_val, (u_int *)&objp->any_u.i.i_len, ~0, sizeof(int), xdr_int)) {
			return (FALSE);
		}
		break;
	case DAVARFLOAT_RPC:
		if (!xdr_array(xdrs, (char **)&objp->any_u.r.r_val, (u_int *)&objp->any_u.r.r_len, ~0, sizeof(float), xdr_float)) {
			return (FALSE);
		}
		break;
	case DAVARDOUBLE_RPC:
		if (!xdr_array(xdrs, (char **)&objp->any_u.d.d_val, (u_int *)&objp->any_u.d.d_len, ~0, sizeof(double), xdr_double)) {
			return (FALSE);
		}
		break;
	case DAVARSTRING_RPC:
		if (!xdr_string(xdrs, &objp->any_u.s, ~0)) {
			return (FALSE);
		}
		break;
	case DAVARERROR_RPC:
		if (!xdr_int(xdrs, &objp->any_u.error)) {
			return (FALSE);
		}
		break;
	}
	return (TRUE);
}




bool_t
xdr_wany(xdrs, objp)
	XDR *xdrs;
	wany *objp;
{
	if (!xdr_PNAME(xdrs, &objp->name)) {
		return (FALSE);
	}
	if (!xdr_pointer(xdrs, (char **)&objp->val, sizeof(any), xdr_any)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_RVALLIST(xdrs, objp)
	XDR *xdrs;
	RVALLIST *objp;
{
	if (!xdr_array(xdrs, (char **)&objp->RVALLIST_val, (u_int *)&objp->RVALLIST_len, ~0, sizeof(any), xdr_any)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_WVALLIST(xdrs, objp)
	XDR *xdrs;
	WVALLIST *objp;
{
	if (!xdr_array(xdrs, (char **)&objp->WVALLIST_val, (u_int *)&objp->WVALLIST_len, ~0, sizeof(wany), xdr_wany)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_ERRLIST(xdrs, objp)
	XDR *xdrs;
	ERRLIST *objp;
{
	if (!xdr_array(xdrs, (char **)&objp->ERRLIST_val, (u_int *)&objp->ERRLIST_len, ~0, sizeof(int), xdr_int)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_TESTNAMELIST(xdrs, objp)
	XDR *xdrs;
	TESTNAMELIST *objp;
{
	if (!xdr_string(xdrs, &objp->test_condition, ~0)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->max_time_wait)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->max_event_wait)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->prog)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->vers)) {
		return (FALSE);
	}
	if (!xdr_pointer(xdrs, (char **)&objp->NAMELISTP, sizeof(NAMELIST), xdr_NAMELIST)) {
		return (FALSE);
	}
	return (TRUE);
}


