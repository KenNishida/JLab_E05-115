include ../Makefile.in

ALLOBJS= daVarHandlers.o daVarHashLib.o daVarRegister.o daVarRpcProc.o \
         daVarRpc_clnt.o daVarRpc_svc.o daVarRpc_xdr.o daVarServ.o \
         thClient.o thGethit.o thGroup.o thHandlers.o thHist.o thLoad.o \
         thParm.o thReport.o thTest.o thTestExecute.o thTestParse.o thUtils.o

LIB= libctp.a
GCC=/usr/bin/gcc
MACROS= -Df2cFortran -Dextname

#CFLAGS= -m32 -O -g $(MACROS)
CFLAGS= -O -g $(MACROS)

INC= $(CERNINC) -I/usr/include/tirpc

.c.o:
	$(GCC) $(CFLAGS) $(INC) -c $<

none: $(LIB)

all: $(LIB)

$(LIB): $(ALLOBJS)
	$(AR) r $@ $?

install:
	cp $(LIB) $(LIBROOT)

clean:
	$(RM) *.o $(LIB)
