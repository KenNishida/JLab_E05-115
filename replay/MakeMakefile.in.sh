#!/bin/sh

echo "Please select your CERNINC directory:"
echo "   1 = /site/cernlib/i386_rhel3/2004/include"
echo "   2 = /usr/include/cernlib/2006"
echo "   3 = Other directory"
read OPINC
echo "Please select your CERNLIB directory:"
echo "   1 = /site/cernlib/i386_rhel3/2004/lib"
echo "   2 = /usr/lib/cernlib/2006/lib"
echo "   3 = Other directory"
read OPLIB

if [ $OPINC = 1 ]
then
    CERNINC=/site/cernlib/i386_rhel3/2004/include
fi
if [ $OPINC = 2 ]
then
    CERNINC=/usr/include/cernlib/2006
fi
if [ $OPINC = 3 ]
then
    echo "Please input the new CERNINC directory:"
    read CERNINC
fi

if [ $OPLIB = 1 ]
then
    CERNLIB=/site/cernlib/i386_rhel3/2004/lib
fi
if [ $OPLIB = 2 ]
then
    CERNLIB=/usr/lib/cernlib/2006/lib
fi
if [ $OPLIB = 3 ]
then
    echo "Please input the new CERNLIB directory:"
    read CERNLIB
fi

cd ./e05115src/
rm -f Makefile.in
SRCDIR=`pwd`
echo "OFFLINE=${SRCDIR}" > Makefile.in
echo  "##">> Makefile.in
echo 'INC=$(OFFLINE)/INCLUDE' >> Makefile.in
echo 'LIBROOT= $(OFFLINE)/LIB' >> Makefile.in
echo 'MAKEREG= $(OFFLINE)/makereg' >> Makefile.in
echo "CERNINC= -I${CERNINC}/cfortran" >> Makefile.in
echo "CERNLIB= ${CERNLIB}" >> Makefile.in
echo "##">> Makefile.in
echo 'r_%.f : $(INC)/%.cmn' >> Makefile.in
echo -e "\t"'(cp $< . ; $(MAKEREG) $(<F) -o $@ -e /dev/null; rm -f $(<F))' >> Makefile.in
echo '.PRECIOUS: r_%.f' >> Makefile.in

cd ../e05115replay/
rm -rf compile.sh
REPLAYDIR=`pwd`
echo "#!/bin/sh" > compile.sh
echo "##">> compile.sh 
echo "cd ${SRCDIR}" >> compile.sh
echo "make clean">> compile.sh
echo "make">> compile.sh 
echo "make install">> compile.sh 
echo "cd ${REPLAYDIR}/SRC" >> compile.sh
echo "make clean;make">> compile.sh 
echo "cd ${REPLAYDIR}" >> compile.sh

chmod u+x compile.sh
./compile.sh
cd ..
