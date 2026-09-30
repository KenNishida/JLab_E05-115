#!/usr/bin/python
import os
import sys

def executekrd(rootfile1,krdlist):
   rootfile2=rootfile1.split('/')
   rootfile3="krd"+rootfile2[6][4:]
   rootfile4=target+"/"+rootfile3
   rf=rootfile1.rstrip("\n")
   cmd1="./krd "+rf+" "+rootfile4
   os.system(cmd1)	
   krdlist.write(rootfile4)	

def createkrd(target):
   dataname=target+".dat"
   listname="krdlist_"+target+".dat"
   fdata=open(dataname,"r")
   krdlist=open(listname,"w")
   rootfile1=fdata.readline()

   i=0
   while rootfile1:
      executekrd(rootfile1,krdlist)
      rootfile1=fdata.readline()
      i=i+1

argvs = sys.argv
argc = len(argvs)
if (argc == 2):
   target=argvs[1]
   cmd3 = "rm -rf "+target+"/krd*.root"
   os.system(cmd3)
   createkrd(target)
   cmd4 = "./merge "+target
   os.system(cmd4)
else:	
   print "type: ./createkrd.py target"
   print "target is 'ch2', 'c12', 'li7', 'h2o', 'ch2', 'b10', 'be9'"

