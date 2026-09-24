#include <fstream>
#include <iostream>
#include <TFile.h>
#include "coin.h"
using namespace std;
char *ofilename;

int main(int argc, char* argv[]){
   char* filename;
   TFile* file; 
   TTree* tree;
   if (argc==3){ 
      filename=argv[1];
      ofilename=argv[2];
      file = new TFile(filename);
      tree=(TTree*)file->Get("h9500");
   }
   else {
      cout << "usage:./krd file.root"<<endl;
      return 0;
   }
   coin test(tree);
   test.Loop();
}
