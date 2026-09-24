#include <TChain.h>
#include <iostream>
#include <fstream>
using namespace std;

int main(int argc, char **argv)
{
	if (argc!=2){
      cout << "Usage: ./merge target" << endl;
      return 0;
   }
   TChain *c = new TChain("h9500");
	char str1[100];
	char str2[100];
   sprintf(str1,"krdlist_");
   strcat(str1,argv[1]);
   strcat(str1,".dat");
   ifstream data(str1);
	char file[100];
	//while (!(data.eof())){
	while (data.getline(file,sizeof(file),'\n')){
		//data >>file;
		cout << file << endl;
      c->Add(file);
	}
   sprintf(str2,argv[1]);
   strcat(str2,"/");
   strcat(str2,argv[1]);
   strcat(str2,".root");
   c->Merge(str2);
   return 0;
}
